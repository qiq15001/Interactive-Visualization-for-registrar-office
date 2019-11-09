
## TODO:
## 1. Does not properly handle two-day classes on MW / WF / MF

appname <- "Schedule Statistics for the UCONN Agenda Archive"

default.terms <- 5

css <- "
.multicol {
height: 100px;
  -webkit-column-count: 6; /* Chrome, Safari, Opera */ 
  -moz-column-count: 6;    /* Firefox */ 
  column-count: 6; 
  -moz-column-fill: auto;
  -column-fill: auto;
} 
"

library(shiny)
library(ggplot2)

library(gridExtra)
library(magrittr)
library(dplyr)

library(readr)
library(RColorBrewer)
library(reshape2)
library(scales)
library(DT)
library(shinyTime)
library(data.table)
library(formattable)
library(chron)

source("color.R")
source("fun-osm.R")

load("map-latlon.Rdata")

load("room-grid.Rdata")

load("adherences-full.Rdata")
load("summary-plots.Rdata")

## adherences, for meeting patterns
lec[lec=="TRUE"] <- "Yes"
lec[lec=="FALSE"] <- "No"

usage0 <- patterns %>%
    melt(varnames=c("Semester", "Subject", "pattern.ix"), value.name="Count") %>%
    group_by(Semester, pattern.ix) %>%
    summarize(Usage=sum(Count)) %>%
    ungroup %>% 
    acast(pattern.ix ~ Semester, value.var="Usage") %>%
    as.data.frame
usage0$pattern.ix <- as.numeric(rownames(usage0))

usage <- usage0 %>% left_join(lec[c("pattern.ix", "n")]) %>%
    rename(All=n)

times <- lec$Classm.Meeting.Time.Start
time <- strptime(times, "%I:%M:%S %p")
lec$Classm.Meeting.Time.Start <- gsub("^0", "", format(time,format='%I:%M %p'))
times <- lec$Classm.Meeting.Time.End
time <- strptime(times, "%I:%M:%S %p")
lec$Classm.Meeting.Time.End <- gsub("^0", "", format(time,format='%I:%M %p'))

pattern.counts <- patterns %>% apply(c(1, 3), sum)

terms <- dimnames(adherences)[[1]]

## standard patterns, for compliance
smp <- read_csv("standard-patterns.csv", col_types="ccc") %>% 
    mutate(StartLabel=tolower(gsub(":00 ", "", strftime(strptime(paste0(Start, ":00"), "%T"), "%r"))),
           EndLabel=tolower(gsub(":00 ", "", strftime(strptime(paste0(End, ":00"), "%T"), "%r"))))

## subject lookups
subj.grad <- read_csv("graduate_subject.csv")
subj.undergrad <- read_csv("undergraduate_subject.csv")
subj <- bind_rows(subj.grad, subj.undergrad) %>%
    distinct %>%
    arrange(class.code) %>%
    rename(Subject=`Subject Area`,
           School=`School or College`,
           Department=`Department or Institute`,
           Code=class.code,
           AKA=`Former Name of Subject Area`)
subj.lookup <- subj$Subject
names(subj.lookup) <- subj$Code

## 
all.rooms <-
    all.rooms %>%
    filter(!Room %in% c("", "TBA")) %>% 
    mutate(Name=ifelse(is.na(Name),
                       BuildingCode,
                       sprintf("%s (%s)", Name, BuildingCode))) %>%
    rename(RoomCapacity=Capacity)

## Remove weekends and restrict to 7am-7pm
room.grid <- room.grid[,,
                       !dimnames(room.grid)[[3]] %in% c("Saturday", "Sunday"),
                       dimnames(room.grid)[[4]] %in% (7:19),
                       ]

b <- c(0, 25, 40, 50, 100, 350)
all.rooms$Capacity <- cut(all.rooms$RoomCapacity, b, include.lowest=TRUE)

T <- nrow(all.terms)
K <- nrow(all.rooms)
K2 <- sum(all.rooms$Registrar.Controlled)

capacity.names <-
    levels(all.rooms$Capacity) %>%
    gsub("\\)|\\(|\\[|\\]", "", .) %>%
    gsub(",", "-", .)
capacities <- levels(all.rooms$Capacity)
names(capacities) <- capacity.names
names(capacity.names) <- capacities
capacity.names["All"] <- "any number of"

subset.names <- list(registrar="controlled by the Registrar",
                     all="on campus")

times <-
  expand.grid(Minute=seq(0, 55, by=5), Hour=0:23) %>%
  as.data.frame %>%
  mutate(Timestamp = sprintf("%02d:%02d", Hour, Minute),
         AMPM = ifelse(Hour > 11, "pm", "am"),
         Label=ifelse(Hour > 12, Hour-12, Hour),
         Label=replace(Label, Label == 0, 12),
         Label=sprintf("%d:%02d%s", Label, Minute, AMPM))
time.labs <- times$Label

time.df <- data.frame(Time=times$Timestamp,
                      ix=1:nrow(times),
                      stringsAsFactors=FALSE)

std.patterns0 <- read_csv("standard-patterns.csv", col_types="ccc")
std.patterns <-
  std.patterns0 %>%
  left_join(time.df, by=c("Start"="Time")) %>%
  left_join(time.df, by=c("End"="Time"), suffix=c(".start", ".end"))

wd.lookup <- c("Monday"="Mon",
               "Tuesday"="Tues",
               "Wednesday"="Wed",
               "Thursday"="Thur",
               "Friday"="Fri",
               "Saturday"="Sat",
               "Sunday"="Sun")
weekday.lookup <- names(wd.lookup)
names(weekday.lookup) <- wd.lookup

wd.regex <- c("M", "T", "W", "Th", "F")

ui <- navbarPage(title="UCONN Registrar Statistics",
    tabPanel(value="landing",
             title="Home",
             uiOutput("landing_page")),
    tabPanel(value="schedules",
             title="Schedules and rooms",
             uiOutput("schedule_wrapper")),
    tabPanel(value="meetings",
             title="Meeting patterns",
             uiOutput("meetings_wrapper")),
    header=tagList(        
        tags$head(tags$style(HTML(css))),
        fluidRow(align="center",
                 h1(appname))
    ),
    footer=div(HTML("&nbsp;"), style="margin-bottom: 40px;")
)

server <- function(input, output, session) {
    storage <- reactiveValues()

    output$landing_page <- renderUI({
        tagList(
            fluidRow(align="center", 
             p("A collaboration between the",
               a("Department of Statistics",
                 href="https://stat.uconn.edu/"),
               "and the",
               HTML("<a href=\"https://registrar.uconn.edu/\">Office of the Registrar</a>."),
               style="margin-top: 40px; margin-bottom: 40px;"),
                     p("Data was collected from",
                       a(href="https://registrar.uconn.edu/scheduling-proofs/",
                         "Registrar scheduling proofs"), "and",
                       a(href="https://classrooms.uconn.edu/",
                         "classrooms.uconn.edu"),
                       "on the UCONN website.")),
            hr(),
            fluidRow(style="margin-top: 20px;",
                     column(3,
                            uiOutput("by_the_numbers")
                       ),
                column(9,
                       uiOutput("sumplot_wrapper")
                       )
                )
        )
    })
    output$by_the_numbers <- renderUI({
        tagList(
            h3("Past Agendas: By the Numbers",
               style="margin-bottom: 20px;"),
            selectInput("sumplot", "Summary plot", width="100%",
                                        choices=c("Aggregate historical counts"="agg",
                                                  "Component types"="pcomp",
                                                  "Class sizes"="psize")),
            hr(),
            tags$ul(
                     tags$li(nrow(all.terms), "semesters",
                             tags$ul(tags$li(paste(all.terms$Term[c(1, nrow(all.terms))],
                                                   collapse=" - ")))),
                     tags$li(length(unique(all.rooms$BuildingCode)), "buildings",
                             tags$ul(
                                      tags$li(sum(all.rooms$Registrar.Controlled), "classrooms controlled by Registrar"),
                                      tags$li(nrow(all.rooms), "classrooms total at Storrs")
                                  )),
                     tags$li(nrow(subj), "academic subjects"),
                     tags$li(format(dim(patterns)[3], big.mark=","), "class meeting patterns",
                             tags$ul(tags$li(nrow(smp), "standard meeting patterns"))),
                     tags$li(format(sum(component.summary$n),
                                    big.mark=","), "class component observations",
                             do.call(tags$ul,
                                     lapply(1:nrow(component.summary), function(i) {
                                         with(component.summary, {
                                             tags$li(format(n[i], big.mark=","),
                                                     paste0(tolower(component.type[i]),
                                                            "s"))
                                         })
                                     })))
                 )
        )
    })
    output$sumplot_wrapper <- renderUI({
        req(input$sumplot)
        if (input$sumplot == "agg") {
            return(plotOutput("summary_plot1"))
        } else {
            return(plotOutput("summary_plot2"))
        }
    })
    output$summary_plot1 <- renderPlot({
        cols <- as.vector(cols_all[c("Red2", "Blue2")])
        plotdata <- list(
            ncomp=list(rawdata=samplesize.term, 
                       con.var = "term.component.ID", 
                       xaxis = "", xlabel = samplesize.term$Term,
                       yaxis = "Frequency", col.cat = cols,
                       title = "Number of class components"),
            nstudent=list(rawdata=enrollmentdata, 
                          con.var = "total.enrollment.sum", 
                          xaxis = "", xlabel = enrollmentdata$Term,
                          yaxis = "Number of students", col.cat = cols,
                          title = "Total # enrolled in classes")
        )
        grid.arrange(do.call(plot.scatter, plotdata$ncomp),
                     do.call(plot.scatter, plotdata$nstudent),
                     ncol=2)
    }, height=400)
    output$summary_plot2 <- renderPlot({
        if (input$sumplot == "pcomp") {
            plot.mosaic(prop.component,
                        "component.type",
                        as.vector(cols_all[c("Red1", "Yellow1", "GreenB1", "Blue1", "Purple2")]),
                        "Frequency of component type")
        } else if (input$sumplot == "psize") {
            plot.mosaic(prop.size,
                        "sizecat",
                        as.vector(cols_all[c("Red1", "Orange1", "Yellow1", "GreenA1", "Blue1", "Purple1", "Grey1")]),
                        "Frequency of lecture size")
        }
    }, height=600)

    output$schedule_wrapper <- renderUI({
        tagList(
            fluidRow(align="center",
                     h2("Semester Schedules and Room Availability",
                        style="margin-bottom: 30px;")),
            uiOutput("subset_select"),
            uiOutput("summary"),
            tabsetPanel(
                        tabPanel(value="heatmap",
                                 title="Availability",
                                 uiOutput("heatmap_wrapper")),
                        tabPanel(value="occupancy",
                                 title="Occupancy",
                                 uiOutput("occupancy_wrapper"))
                        )
        )
    })

    output$subset_select <- renderUI({
        fluidRow(align="center",
                 column(2, offset=4,
                        selectInput("term", "Academic term",
                                    choices=rev(all.terms$Term))),
                 column(2, selectInput("capacity", label="Classroom seating capacity",
                                       choices=c("All", capacities)))
                 )
    })

    output$summary <- renderUI({
        fluidRow(align="center",
                 em("Displayed are room counts at the Storrs campus.",
                    style="margin-bottom: 30px;"),
                 tableOutput("summary_tab")
                 )
                 
    })
    
    output$summary_tab <- renderTable({
        req(input$term, input$capacity)

        r <- get.r(input$capacity, input$term, FALSE)
        
        d <- data.frame(Registrar=sum(r$Registrar.Controlled),
                        Total=nrow(r))
        
        colnames(d) <- c("Registrar-controlled",
                         "Total at UCONN")
        d
    }, align="c")

    output$heatmap_wrapper <- renderUI({
        req(input$term)
        tagList(
            fluidRow(align="center", h2(input$term, "- Available classrooms")),
            fluidRow(align="center",
                     column(4, offset=4, style="margin-top: 20px;",
                            radioButtons("registrar_only", "Registrar rooms only",
                                         c("Yes", "No"), inline=TRUE)
                            )),
            uiOutput("heatmap_body"))
    })
    
    output$heatmap_body <- renderUI({
        req(input$term, input$capacity, input$registrar_only)

        capacity <- input$capacity
        term <- input$term
        registrar <- input$registrar_only

        r <- get.r(capacity, term, registrar)
        
        if (nrow(r) == 0) {
            css.right <- "padding-left: 20px;"
            return(tagList(
                h3("No rooms are available that satisfy these conditions!",
                   style="text-align: center;"),
                tags$table(
                         tags$tr(
                                  tags$td("Term"),
                                  tags$td(term, style=css.right)
                              ),
                         tags$tr(
                                  tags$td("Capacity"),
                                  tags$td(capacity, style=css.right)
                              ),
                         style="margin-left: auto; margin-right: auto;")
            ))
        } else {
            tr.bldg <- list()
            rr <- r %>%
                distinct(Name, Room, RoomCapacity, RoomID, Description, Subjects, Semesters) %>%
                arrange(Name, Room)

            for (i in 1:nrow(rr)) {
                tr.bldg[[i]] <- with(rr[i,], {
                    tags$tr(tags$td(Name, style="padding-left: 10px; padding-right: 5px"),
                            tags$td(Room, style="padding-left: 5px; padding-right: 5px;"),
                            tags$td(RoomCapacity, style="padding-left: 5px; padding-right: 5px;"),
                            tags$td(Description, style="padding-left: 5px; padding-right: 10px;"))
                })
                
            }

            bb <- r %>%
                group_by(BuildingCode, Name) %>%
                summarize(n=n())
            
            tr.summ <- list()
            for (i in 1:nrow(bb)) {
                tr.summ[[i]] <- with(bb[i,], {
                    tags$tr(
                             tags$td(BuildingCode, style=paste("padding-left: 10px;",
                                                               "padding-right: 5px;")),
                             tags$td(gsub("\\(.*$", "", Name), style=paste("padding-left: 5px;",
                                                       "padding-right: 5px;")),
                             tags$td(n, style=paste("padding-left: 5px;",
                                                    "padding-right: 10px;",
                                                    "text-align: right;"))
                         )
                })
            }
            tr.summ <- c(list(tags$tr(
                                       tags$th("Code", style=paste("padding-left: 10px;",
                                                                    "padding-right: 5px;",
                                                                    "text-decoration: underline;")),
                                       tags$th("Building", style=paste("padding-left: 5px;",
                                                                       "padding-right: 5px;",
                                                                    "text-decoration: underline;")),
                                       tags$th("Rooms", style=paste("padding-left: 5px;",
                                                                    "padding-right: 10px;",
                                                                    "text-decoration: underline;",
                                                                    "text-align: right;"))
                              )),
                         tr.summ)
            bldg.summ <- tags$table(tr.summ, width="100%")
            

            capacity.label <- capacity.names[capacity]
            settings.label <- sprintf("%s students", capacity.label)
            settings.label <- paste(capacity.label,
                                    "students.")
            if (registrar == "Yes") {
                subset.label <- subset.names["registrar"]
            } else { 
                subset.label <- subset.names["all"]
            }

            labs <- bb %>%
                filter(BuildingCode %in% latlon$ID) %>%
                use_series(BuildingCode)
            storage$labs <- labs

            roomtab <- r %>% select(BuildingCode, Name, Room,
                                    Registrar.Controlled,
                                    RoomCapacity, Description,
                                    Subjects, RoomID) %>%
                mutate(Registrar.Controlled=c("No", "Yes")[as.numeric(Registrar.Controlled)+1],
                       Name=gsub("\\(.*$", "", Name),
                       RoomCapacity=as.character(RoomCapacity)) %>% 
                rename(Code=BuildingCode,
                       Building=Name,
                       `Room #`=Room,
                       `Registrar?`=Registrar.Controlled,
                       Capacity=RoomCapacity)
            storage$roomtab <- roomtab

            o <- list(
                fluidRow(align="center", style="margin-bottom: 20px;",
                         h3("There are",
                            span(as.character(nrow(r)),
                                 style=paste(sprintf("color: %s;", cols_all["Blue2"]),
                                             "font-weight: bold;",
                                             "text-decoration: underline;")),
                            "rooms", subset.label,
                            "seating", settings.label,
                            style="margin-bottom: 20px;")),
                uiOutput("heatable"),
                hr(),
                fluidRow(
                    h3("Classrooms by building", style="text-align: center; margin-bottom: 50px;"),
                    column(1, offset=1,
                           actionButton("toggle_all_labels", "Toggle all labels",
                                        style="margin-bottom: 20px;"),
                           checkboxGroupInput("labels", "Building labels",
                                              choices=labs)),
                    column(5,
                           style="text-align: center;",
                           em("Counts give # classrooms in buildings."),
                                        plotOutput("map", height="600px")),
                    column(4, bldg.summ)
                ),
                hr(),
                fluidRow(
                    h3("Classrooms details", style="text-align: center; margin-bottom: 50px;"),
                    column(8, renderDataTable(roomtab %>% select(Code, Building, `Room #`, `Registrar?`, Capacity),
                                              selection="none"),
                           style="margin-top: 10px;"),
                    column(4, h4("Room summary"),
                           column(6, selectInput("bldg_detail", "Building", choices=roomtab$Code,
                                                 width="100%")),
                                    column(6, uiOutput("room_detail_ui")),
                           column(12, em(paste("Room capacity for non-Registrar rooms is approximate.",
                                               "Past class types and subjects provided as-is and are for informational purposes only.")),
                                  uiOutput("room_info", style="margin-top: 20px; margin-bottom: 10px;"))
                           )
                )
            )
            
            return(do.call(tagList, o))
        }
    })
    
    output$room_detail_ui <- renderUI({
        req(input$bldg_detail)
        all.r <- storage$roomtab %>% filter(Code == input$bldg_detail)
        selectInput("room_detail", "Room #", choices=all.r$`Room #`, width="100%")
    })

    output$room_info <- renderUI({
        req(input$room_detail)
        r <- storage$roomtab %>% filter(Code == input$bldg_detail,
                                        `Room #` == input$room_detail)
        storage$roomsel <- r$RoomID
        tagList(
            tags$ul(
                     tags$li(strong("Code:"),
                             r$Code),
                     tags$li(strong("Room #:"),
                             r$`Room #`),
                     tags$li(strong("Building:"),
                             r$Building),
                     tags$li(strong("Registrar-controlled?"),
                             r$`Registrar?`),
                     tags$li(strong("Seating capacity:"),
                             r$Capacity),
                     tags$li(strong("Class types seen in this room:"),
                             do.call(tags$ul, lapply(unlist(strsplit(r$Description, ", ")),
                                                     tags$li))),
                     tags$li(strong("Subjects seen in this room:"),
                             do.call(tags$ul, lapply(unlist(strsplit(r$Subjects, ", ")),
                                                     function(s) {
                                                         subj.cand <- subj %>%
                                                             filter(Code == s)
                                                         if (nrow(subj.cand) == 0) {
                                                             subj.cand <- subj.cand %>%
                                                                 filter(grepl(s, AKA))
                                                         }
                                                         if (nrow(subj.cand) == 0) {
                                                             slab <- s
                                                         } else {
                                                             slab <- subj.cand$Subject[1]
                                                         }
                                                         tags$li(slab)
                                                     })))
                 )
        )
    })

    observeEvent(input$toggle_all_labels, {
        l <- input$labels
        if (length(l) == length(storage$labs)) {
            updateCheckboxGroupInput(session, "labels", choices=storage$labs, selected=NULL)
        } else {
            updateCheckboxGroupInput(session, "labels", choices=storage$labs, selected=storage$labs)
        }
    })

    output$heatable <- renderUI({
        req(input$term, input$capacity, input$registrar_only)
        
        term <- input$term
        capacity <- input$capacity
        registrar <- input$registrar_only
        r <- get.r(capacity, term, registrar)

        r <- r %>% use_series(RoomID)
        rlen <- length(r)

        ## matrices of the number and proportion of rooms that are
        ## free at 24x7 different times
        if (rlen == 1) {
            m.sum <- 1*room.grid[term,r,,,]
        } else {
            m.sum <- apply(room.grid[term,r,,,], 2:4, sum)
        }

        m <- m.sum %>%
            melt(varnames=c("Weekday",
                            "Hour",
                            "Minute"),
                 value.name="Unoccupied") %>%
            ## mutate(Weekday=factor(Weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
            arrange(Weekday, Hour, Minute)

        min.inc <- 5
        mm <- m %>% distinct(Hour, Minute) %>% filter(Minute %in% seq(0, 55, by=min.inc))
        mm$ix <- 1:nrow(mm)
        
        m <- m %>% filter(Minute %in% mm$Minute) %>% left_join(mm)
        m.sum <- m %>% acast(Weekday ~ ix, value.var="Unoccupied")
        m.mean <- m.sum/rlen
        
        mm$label <- sapply(1:nrow(mm), function(i) {
            x <- mm[i,]
            if (x$Hour < 12) {
                sprintf("%d:%02dam", x$Hour, x$Minute)
            } else if (x$Hour == 12) {
                sprintf("%d:%02dpm", x$Hour, x$Minute)
            } else {
                sprintf("%d:%02dpm", x$Hour-12, x$Minute)
            }
        })

        colnames(m.mean) <- mm$Hour
        colnames(m.mean) <- sapply(as.numeric(colnames(m.mean)), function(x) {
            if (x == 0) {
                "12am"
            } else if (x < 12) {
                sprintf("%dam", x)
            } else if (x == 12) {
                "12pm"
            } else {
                sprintf("%dpm", x-12)
            }
        })
        colnames(m.mean)[duplicated(colnames(m.mean))] <- ""
        
        b <- seq(0, 1, by=.1)
        cols <- colorRampPalette(c("white", cols_all["Red2"]))(11) %>% tail(-1)

        css.td <- c("padding-top: 5px;",
                    "padding-bottom: 5px;",
                    "text-align: center;") %>%
            paste(collapse=" ")
        rows <- list()
        for (i in 1:nrow(m.mean)) {
            r <- list()
            for (j in 1:ncol(m.mean)) {
                css <- css.td
                if (m.sum[i,j] == 0) {
                    css <- c(css,
                             "background-color: black;",
                             "color: gray;",
                             "border: 1px solid black")
                } else {
                    ix <- cut(1-m.mean[i,j], b, include.lowest=TRUE) %>% as.numeric
                    css <- c(css,
                             sprintf("background-color: %s;", cols[ix]),
                             sprintf("border: 1px solid %s;", cols_all["Red2"]))
                }
                if (i == 1) {
                    css <- c(css,
                             "border-top: 1px solid grey;")
                } else if (i == nrow(m.mean)) {
                    css <- c(css,
                             "border-bottom: 1px solid grey;")
                }
                if (j == 1) {
                    css <- c(css,
                             "border-left: 1px solid grey;")
                } else if (j == ncol(m.mean)) {
                    css <- c(css,
                             "border-right: 1px solid grey;")
                }
                css <- c(css, "width: .6%;")
                ## if (colnames(m.mean)[j] == "") {
                    nstr <- HTML("&nbsp;")
                ## } else {
                     ## nstr <- m.sum[i,j]
                ## }
                css <- c(css, "font-size: x-small;")
                r[[j]] <- tags$td(nstr,
                                  title=paste(rownames(m.mean)[i], mm$label[j], "-", m.sum[i,j], "rooms available"),
                                  style=paste(css, collapse=" "))
                
            }
            r[[ncol(m.mean)+1]] <- tags$td(rownames(m.mean)[i],
                                           style=paste(css.td,
                                                       "text-align: left;",
                                                       "padding-left: 2em;",
                                                       collapse=" "))
            rows[[i]] <- tags$tr(r)
        }
        rows[[length(rows)+1]] <- tags$tr(lapply(colnames(m.mean), tags$td, style=css.td))
        color.tab <- tags$table(width="100%",
                                tags$tr(c(lapply(2:length(b),
                                               function(ib) {
                                                   bb <- b[ib-1]
                                                   bb1 <- b[ib]
                                                   ix <- cut(bb, b, include.lowest=TRUE) %>% as.numeric
                                                   tags$td(strong(sprintf("%.0f-%.0f", bb*100, bb1*100)),
                                                           sprintf("(%d-%d)", floor((1-bb1)*rlen), floor((1-bb)*rlen)),
                                                           style=paste(css.td,
                                                                       sprintf("background-color: %s;", cols[ix]),
                                                                       "border: 1px solid black;",
                                                                       collapse=" "))
                                               }),
                                          list(tags$td(strong("Impossible"), "(None)",
                                                       style=paste(css.td,
                                                                   "background-color: black;",
                                                                   "color: grey;")))),
                                        style="border: 1px solid black;"),
                                style="table-layout: fixed;")
        tagList(
            fluidRow(style="margin-top: 20px;",
                em(sprintf("Availability measured every %d minutes. Hover mouse over timeslot for number of available rooms.",
                           min.inc)),
                style="text-align: center;"),
            tags$table(rows, width="100%",
                       style=paste("margin-bottom: 10px;",
                                   "margin-top: 20px;",
                                   "table-layout: fixed;")),
            h4("Scheduling difficulty (# rooms available)", style=paste("text-align: center;",
                                                    "margin-top: 20px;")),
            color.tab,
            h5("Difficulty is the percent of rooms that are occupied, and goes from 0 (no difficulty) – 100 (impossible).",
               style="text-align: center;"))
    })

    output$map <- renderPlot({
        req(input$term, input$capacity, input$registrar_only)

        capacity <- input$capacity
        term <- input$term
        registrar <- input$registrar_only

        r <- get.r(capacity, term, registrar)
        rr <- r %>% group_by(BuildingCode) %>% summarize(n=n()) %>% ungroup
        l <- latlon %>% filter(ID %in% r$BuildingCode) %>% left_join(rr, by=c("ID"="BuildingCode"))
        
        maxn <- max(l$n)
        x <- seq(0, maxn, length.out=5)
        
        par(mar=rep(.1, 4))
        plot.osm(map)
        col.bldg <- cols_all["Blue1"]
        points(Y ~ X, l, pch=21,
               bg=alpha(col.bldg, .8),
               col="black", cex=4)
        text(l$X, l$Y, l$n)
        if (length(input$labels) > 0) {
            l <- l %>% filter(ID %in% input$labels)
            if (nrow(l) > 0) {
                cex <- 1.3
                w <- strwidth(l$ID, cex=cex*1.5)
                h <- strheight(l$ID, cex=cex*1.5)
                for (i in 1:nrow(l)) {
                    rect(l$X[i]-.5*w[i],
                         l$Y[i]-1.5*h[i],
                         l$X[i]+.5*w[i],
                         l$Y[i]-.5*h[i],
                         col=alpha("white", .8))
                    text(l$X[i], l$Y[i]-h[i], l$ID[i], cex=cex, col=alpha("black", .8))
                }
            }
        }
    })

    output$occupancy_wrapper <- renderUI({
        req(input$term, input$capacity)

        capacity <- input$capacity
        term <- input$term

        r.reg <- get.r(capacity, term, "Yes")
        N.reg <- nrow(r.reg)
        r.all <- get.r(capacity, term, "No")
        N.all <- nrow(r.all)

        g.reg <- apply(room.grid[term,r.reg$RoomID,,,], 2:4, sum) %>%
            melt(varnames=c("Weekday", "Hour", "Minute"),
                 value.name="Unoccupied") %>%
            mutate(Weekday=factor(Weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
            arrange(Weekday, Hour, Minute)
        gg <- g.reg %>% distinct(Hour, Minute) %>% arrange(Hour, Minute)
        gg$ix <- 1:nrow(gg)
        g.reg <- g.reg %>%
            left_join(gg) %>% 
            acast(Weekday ~ ix, value.var="Unoccupied")
        g.reg <- 1 - g.reg / N.reg
        colnames(g.reg) <- sprintf("%02d:%02d", gg$Hour, gg$Minute)

        g.all <- apply(room.grid[term,r.all$RoomID,,,], 2:4, sum) %>%
            melt(varnames=c("Weekday", "Hour", "Minute"),
                 value.name="Unoccupied") %>%
            mutate(Weekday=factor(Weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
            arrange(Weekday, Hour, Minute)
        gg <- g.all %>% distinct(Hour, Minute) %>% arrange(Hour, Minute)
        gg$ix <- 1:nrow(gg)
        g.all <- g.all %>%
            left_join(gg) %>% 
            acast(Weekday ~ ix, value.var="Unoccupied")
        g.all <- 1 - g.all / N.all
        colnames(g.all) <- sprintf("%02d:%02d", gg$Hour, gg$Minute)

        tagList(
            div(style=paste("text-align: center;",
                            "margin-bottom: 2em"),
                h2(term, "- Weekly occupancy"),
                p(paste("Plot gives proportion of rooms that are occupied (unavailable),",
                        "at 5-minute intervals"))),
            plot.occupancy(g.all, g.reg, N.all, N.reg)
        )
    })

    output$meetings_wrapper <- renderUI({
        tagList(
            div(
                tabsetPanel(
                    tabPanel("Standard patterns",
                             uiOutput("standard_meeting_wrapper")),
                    tabPanel("All patterns",
                             uiOutput("common_patterns_wrapper"),
                             value="all")
                ),
                style="margin-top: 30px;")
        )
    })

    output$standard_meeting_wrapper <- renderUI({
        fluidRow(style="margin-top: 40px;", align="center",
                 sidebarPanel(width=3,
                              h3("Standard meeting patterns", style="text-align: center;"),
                              div(tableOutput("smp"), style="margin-top: 50px;")
                              ),
                 mainPanel(width=6,
                           h2("Standard meeting pattern compliance",
                              style="margin-bottom:20px; text-align: center;"),
                           fluidRow(align="center",
                                    radioButtons("definition", "SMP definition",
                                                 choices=c("Strict", "Loose"),
                                                 inline=TRUE)
                                    ),
                           p("Classes with", strong("strict"),
                             "adherence meet at the correct times on the correct weekdays.",
                             "Classes with", strong("loose"),
                             "adherence meet at standard meeting pattern times, possibly on the wrong weekdays."),
                           tabsetPanel(
                               tabPanel("By semester",
                                        uiOutput("semester")),
                               tabPanel("By subject",
                                        uiOutput("subject"))
                           )),
                 sidebarPanel(width=3,
                              h3("Is this pattern standard?", style="text-align: center;"),
                              selectInput("weekday", "Weekdays",
                                          choices=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                                          multiple=TRUE),
                              column(6, timeInput("start", "Start time", seconds=FALSE,
                                        value=strptime("07:00:00", "%T"))),
                              column(6, timeInput("end", "End time", seconds=FALSE,
                                        value=strptime("08:00:00", "%T"))),
                              hr(),
                              uiOutput("duration"),
                              hr(),
                              uiOutput("standard")
                              )
        )
    })
    output$smp <- renderTable({
        smp %>%
            select(Weekdays, StartLabel, EndLabel) %>%
            rename(Start=StartLabel, End=EndLabel)
    })
    start <- reactive({
        strftime(input$start, "%R")
    })
    end <- reactive({
        strftime(input$end, "%R")
    })
    minutes <- reactive({
        difftime(input$end, input$start, units = "mins")
    })
    output$duration <- renderUI({
        m <- minutes()
        if (m <=0) {
            msg <- strong("Start / end times not valid.")
        } else {
            msg <- strong(m, " minutes")
        }
        tagList(
            h4("Meeting Length"),
            msg
        )
    })
    output$standard <- renderUI({
        req(input$weekday)

        letters <- factor(c(Sunday="Su",
                            Monday="M",
                            Tuesday="T",
                            Wednesday="W",
                            Thursday="Th",
                            Friday="F",
                            Saturday="Sa"),
                          levels=c("Su", "M", "T", "W", "Th", "F", "Sa"))
        hash.code <- paste(sort(letters[input$weekday]), collapse="")
        matched <- smp %>% filter(Weekdays == hash.code,
                                  ## Weekdays %in% c("MW", "MF", "WF"),
                                  Start == start(),
                                  End == end())
        tagList(h4("This ",
                   strong(ifelse(nrow(matched)>0,
                                 "IS",
                                 "IS NOT")),
                   " a standard meeting pattern!"))
    })

    output$semester <- renderUI({
        tagList(
            fluidRow(align="center", style="margin-top: 30px;",
                     selectInput("semester", "Semester", choices=rev(dimnames(adherences)[[1]])),
                     h3("Compliance with standard meeting pattern"),
                     fluidRow(align="center",
                              em("Shown are the proportion of classes that match the definition. Numbers count the total lectures by subject.")),
                     plotOutput("semester_schedule", height="2400px"))
        )
    })
    output$semester_schedule <- renderPlot({
        req(input$definition, input$semester)
        
        a <- adherences[input$semester,,]
        
        if (input$definition == "Strict") {
            n <- a[,"Standard"]
        } else {
            n <- rowSums(a[,c("Standard", "Loose")])
        }
        all.n <- rowSums(a)
        p <- (n / all.n)
        
        rm.ix <- which(is.na(p))
        p <- p[-rm.ix]
        n <- n[-rm.ix]
        all.n <- all.n[-rm.ix]
        
        reorder.ix <- order(p, decreasing=TRUE)
        n <- n[reorder.ix]
        all.n <- all.n[reorder.ix]
        p <- p[reorder.ix]
        
        labs <- subj.lookup[names(p)]
        labs <- ifelse(is.na(labs), names(p), labs) %>% unname
        names(p) <- labs

        k <- 4
        cols <- colorRampPalette(c(cols_all["Red2"], "white", cols_all["Blue2"]))(k)

        all.bins <- cut(p, breaks=seq(0, 1, length.out=k+1), include.lowest=TRUE)
        all.cols <- cols[as.numeric(all.bins)]
        
        par(mar=c(4.1, 30.1, 4.1, 2.1), mfrow=c(4, 1),
            cex.axis=1.5)

        k2 <- 4
        w <- ceiling(length(p)/k2)
        for (i in 1:k2) {
            ix <- (i-1)*w + 1:w
            ix <- ix[ix <= length(p)]
            bp <- barplot(rev(p[ix]),
                          col=rev(all.cols[ix]),
                          horiz=TRUE,
                          las=1,
                          axes=FALSE,
                          xlim=c(0, 1.1))
            box()
            abline(v=seq(0, 1, by=.1), lty=2, lwd=2, col="grey")
            axis(3, at=seq(0, 1, by=.1))
            axis(1, at=seq(0, 1, by=.1))
            bp <- barplot(rev(unname(p[ix])), axes=FALSE, add=TRUE,
                          col=rev(all.cols[ix]),
                          horiz=TRUE)
            last.ix <- tail(bp, 2)
            ix0 <- last.ix[2] + diff(last.ix)
            text(rev(p[ix]), bp, rev(all.n[ix]), pos=4, adj=0,
                 cex=1.5)
            if (i == 1) {
                leg.pos <- "topleft"
            } else {
                leg.pos <- "topright"
            }
            leg.bins <- levels(all.bins)
            leg.cols <- cols[1:length(leg.bins)]
            leg.bins <- leg.bins %>%
                as.character %>%
                gsub("\\[|\\]|\\(", "", .) %>%
                strsplit(",") %>%
                sapply(function(x) {
                    x <- 100*as.numeric(x)
                    sprintf("%.0f%% - %.0f%%", x[1], x[2])
                })
            legend(leg.pos, fill=rev(leg.cols), legend=rev(leg.bins),
                   cex=1.5)
        }
    }, height=2400)
    
    output$subject <- renderUI({
        tagList(
            fluidRow(align="center", style="margin-top: 30px;",
                     selectInput("subject", "Subject", choices=dimnames(adherences)[[2]], multiple=TRUE),
                     actionButton("reset_subjects", "Reset"),
                     h3("Compliance with standard meeting pattern"),
                     fluidRow(align="center",
                              em("Shown are the proportion of classes that match the definition.")),
                     plotOutput("subject_schedule", height="1000px"))
        )
    })
    observeEvent(input$reset_subjects, {
        updateSelectInput(session, "subject", selected=character(0))
    })

    output$subject_schedule <- renderPlot({
        req(input$definition, input$subject)

        these.subjects <- input$subject
        defn <- input$definition

        nsubj <- length(these.subjects)
        
        a <- adherences[,these.subjects,,drop=FALSE]
        p <- n.subj <- array(NA, dim=c(dim(a)[1], nsubj),
                             dimnames=list(dimnames(a)[[1]],
                                           these.subjects))

        for (i in 1:nsubj) {
            if (defn == "Strict") {
                n <- a[,i,"Standard"]
            } else {
                n <- rowSums(a[,i,c("Standard", "Loose")])
            }
            all.n <- rowSums(a[,i,])
            p[,i] <- (n / all.n)
            n[n==0] <- NA
            n.subj[,i] <- n
        }

        dimnames(n.subj) <- NULL
        n.subj <- t(n.subj)

        T <- nrow(p)

        cols <- cols_all[grepl("2", names(cols_all)) & !names(cols_all) == "Grey2"]
        lty <- c(1, 2, 3)

        cols.used <- c()
        lty.used <- c()
        combos <- expand.grid(col=cols, lty=lty, stringsAsFactors=FALSE)
        subj.title <- sapply(these.subjects,
                             function(s) ifelse(s %in% subj$Code,
                                                subj$Subject[s==subj$Code],
                                                s)) %>%
            paste(collapse=", ")
        
        par(cex.axis=1.2, cex.lab=1.2,
            mfrow=c(2, 1))
        
        plot(c(1, T), range(p), type="n", axes=FALSE, lwd=2,
             ylab="Proportion of classes with standard pattern",
             xlab="", ylim=c(0, 1),
             main="Proportion of classes adhering to SMP")
        mtext(side=3,  subj.title)
        for (i in 1:nsubj)  {
            l <- combos$lty[i %% nrow(combos)]
            lty.used <- c(lty.used, l)
            cl <- combos$col[i %% nrow(combos)]
            cols.used <- c(cols.used, cl)
            lines(1:T, p[,i], lty=l,
                  col=cl,
                  lwd=2)
        }
        plab <- gsub("(all|pring) 20", "", rownames(p))
        plab[grepl("^S", plab)] <- ""
        abline(v=which(plab!=""), lty=2, col="grey")
        axis(1, at=1:T, plab)
        axis(2, at=seq(0, 1, by=.2))
        legend("bottomleft", col=cols.used, lty=lty.used,
               these.subjects, lwd=2)

        plot(c(1, T), range(n.subj, na.rm=TRUE), axes=FALSE, lwd=2,
             type="n",
             ylab="Number of classes (any pattern)",
             xlab="", main="Number of classes")
        mtext(side=3,  subj.title)
        for (i in 1:nsubj) {
            lines(n.subj[i,], col=cols.used[i],
                  lwd=2)
        }
        axis(1, at=1:T, plab)
        axis(2)
        legend("topleft", these.subjects, lty=1, col=cols.used, lwd=2)
    }, height=1000)

    output$common_patterns_wrapper <- renderUI({
        tagList(
            h2("Common meeting patterns", style="text-align: center;"),
            fluidRow(
                column(3,
                       selectInput("subset", "Patterns to display",
                                   choices=c("Standard patterns only"="std",
                                             "Non-standard patterns only"="nonstd",
                                             "All patterns"="all")),
                       actionButton("reset_terms", "Reset terms")),
                column(9,
                       div(align="left", class="multicol",
                           checkboxGroupInput("terms", "Terms for ranking",
                                              choices=terms,
                                              selected=tail(terms, default.terms),
                                              inline=FALSE, width="100%")
                           )

                       )
            ),
            hr(),
            fluidRow(
                column(4, uiOutput("pattern_detail")),
                column(8,
                       p(em("Showing patterns observed 5 or more times.",
                            "Only lecture classes were considered."),
                         style="text-align: center; border-bottom: 10px;"),
                       dataTableOutput("pattern"))
            )
        )
    })
    observeEvent(input$reset_terms, {
        updateCheckboxGroupInput(session, "terms", selected=tail(terms, default.terms))
    })
    output$pattern <- renderDataTable({
        req(input$terms)
        
        otherpattern <- lec
        ## otherpattern$n <- usage[as.character(otherpattern$pattern.ix), "All"]
        otherpattern$n <- rowSums(usage[as.character(otherpattern$pattern.ix),
                                        input$terms,drop=FALSE])
                                        ## head(terms, input$nterms),drop=FALSE])
        colnames(otherpattern) <- c("Mon", "Tues", "Wed", "Thur", "Fri", "Sat", "Sun",
                                    "Start time", "End time",
                                    "Usage", "pattern.ix", "Standard", "Loose")

        otherpattern <- otherpattern %>% filter(Usage >= 5) %>% arrange(desc(Usage))

        if (input$subset == "std") {
            otherpattern <- otherpattern[otherpattern$Standard == "Yes",]
        } else if (input$subset == "nonstd") {
            otherpattern <- otherpattern[otherpattern$Standard == "No",]
        } 
        cell.base <- style("display"="block",
                           "padding"="0 4px",
                           "border-radius"="4px",
                           "text-align"="center")
        red.cell <- paste(c(style("background-color"=alpha(cols_all["Red1"], .3)),
                            cell.base),
                          collapse="; ")
        blue.cell <- paste(c(style("background-color"=alpha(cols_all["Blue1"], .3)),
                            cell.base),
                          collapse="; ")
        purple.cell <- paste(c(style("background-color"=alpha(cols_all["Purple1"], .3)),
                            cell.base),
                          collapse="; ")
        white.cell <- paste(c(style("background-color"=alpha(cols_all["White"], .3)),
                            cell.base),
                          collapse="; ")
        grey.cell <- paste(c(style("background-color"=alpha(cols_all["Grey1"], .3)),
                            cell.base),
                          collapse="; ")

        otherpattern[,wd.lookup][otherpattern[,wd.lookup] == "No"] <- "-"
        storage$pattern <- otherpattern

        otherpattern <- otherpattern %>% select(-pattern.ix)
        otherpattern$`%` <- round(otherpattern$Usage / sum(otherpattern$Usage), 2)
        last.ix <- which(colnames(otherpattern) %in% c("Standard", "Loose"))
        otherpattern <- otherpattern[c((1:ncol(otherpattern))[-last.ix], last.ix)]
        outpattern <- as.datatable(
            formattable(
                otherpattern,
                list(
                    Standard = formatter("span", style = x ~ ifelse(x=="Yes", blue.cell, red.cell)),
                    Loose = formatter("span", style = x ~ ifelse(x=="Yes", purple.cell, red.cell))
                )
            ),
            selection="single",
            options=list(columnDefs=list(list(className="dt-center",
                                              targets=which(!colnames(otherpattern) %in%
                                                            c("Usage"))),
                                         list(className="dt-right",
                                              targets=which(colnames(otherpattern)=="Usage"))
                                         ),
                         searching=FALSE)
        )
        
        outpattern$x$data$Usage <- as.numeric(outpattern$x$data$Usage)
        return(outpattern)
        
    })

    output$pattern_detail <- renderUI({
        ix <- input$pattern_rows_selected

        if (is.null(ix)) {
            return(tagList(
                p(
                    em("Select a row for a summary of a pattern."),
                    style="text-align: center;"
                )
            ))
        }

        r <- storage$pattern[ix,]

        mtg.days <- weekday.lookup[wd.lookup[which(r[wd.lookup]=="Yes")]]

        pattern <- patterns[,,as.character(r$pattern.ix)]
        storage$this.pattern <- pattern

        k <- 3
        subj.by.year <- apply(pattern>0, 1, . %>% which %>% names) %>% tail(k) %>% rev

        subj.li <- list()
        for (i in 1:k) {
            this.year <- names(subj.by.year)[i]
            these.subj <- subj.by.year[[i]]
            if (length(these.subj) > 0) {
                subj.li[[i]] <- tags$li(strong(this.year),
                                        do.call(tags$ul,
                                                lapply(1:length(these.subj),
                                                       function(j) {
                                                           tags$li(sprintf("%s (%d)",
                                                                           these.subj[j],
                                                                           pattern[this.year,these.subj[j]]))
                                                           
                                                       }))
                                        )
            } else {
                subj.li[[i]] <- tags$li(strong(this.year),
                                        tags$ul(tags$li(em("None."))))
            }
        }
        subj.ul <- do.call(tags$ul, subj.li)
        
        tagList(
            h2(paste0("Meeting pattern #", r$pattern.ix),
               style="text-align: center;"),
            tags$ul(
                     tags$li(
                              strong("Meeting days:"),
                              tags$ul(tags$li(paste(mtg.days, collapse=", ")))
                          ),
                     tags$li(
                              strong("Meeting time:"),
                              tags$ul(tags$li(r$`Start time`,
                                              "-",
                                              r$`End time`
                                              )
                                      )
                          ),
                     tags$li(
                              strong("Total historical count:"),
                              tags$ul(tags$li(r$Usage))
                          )
                 ),
            hr(),
            h3("Historical frequency",
               style="text-align: center;"),
            plotOutput("pattern_plot", height="200px"),
            hr(),
            h3("Frequency by subject, past 3 semesters",
               style="text-align: center;"),
            em("Number of classes with meeting pattern indicated in parentheses."),
            subj.ul
        )
    })

    output$pattern_plot <- renderPlot({
        pattern <- storage$this.pattern

        pattern.years <- rowSums(pattern)
        names(pattern.years) <- gsub("all|pring 20", "", names(pattern.years))
        
        plot(pattern.years, type="l",
             ylim=c(0, max(pattern.years)),
             axes=FALSE, lwd=2, xlab="", ylab="Frequency")
        axis(1, at=1:length(pattern.years),
             names(pattern.years))
        axis(2)
        
    }, height=200)
}

get.r <- function(capacity, term, registrar) {
    if (capacity != "All") {
        r <-
            all.rooms %>%
            filter(Capacity == capacity)
    } else {
        r <- all.rooms
    }

    if (registrar == "Yes") {
        r <- r %>% filter(Registrar.Controlled)
    }

    r.grid <- room.grid[term,r$RoomID,,,]
    r
}

plot.occupancy <- function(all.week, reg.week, N.all, N.reg) {
    renderPlot({
        layout(mat=matrix(1:6, ncol=1), heights=c(1, rep(2, 5)))
        par(mar=c(0.1, 0.1, 0.1, 0.1))
        plot(1, type="n", xlab="", ylab="", axes=FALSE)
        legend("center", col=c(cols_all[c("Blue2", "Red2")], NA),
               horiz=TRUE,
               lty=c(1, 1, NA),
               lwd=3,
               fill=c(NA, NA, alpha(cols_all["Yellow1"], .1)),
               border=c(NA, NA, "black"),
               c("All rooms", "Registrar rooms", "Standard meeting pattern"),
               cex=2, bty="n")
        par(cex.main=2,
            cex.lab=1.5,
            mar=c(4, 3, 3, 2))
        for (i in 1:nrow(all.week)) {
            d.reg <- data.frame(Time=colnames(all.week), stringsAsFactors=FALSE) %>% left_join(time.df)
            plot(d.reg$ix, all.week[i,], type="n", axes=FALSE,
                 xlab="Time", ylab="Prop. rooms in use",
                 xlim=c(85, 228), ylim=c(0, 1))
            ix.lab <- seq(1, length(time.labs), by=12)
            lab <- time.labs[ix.lab]
            axis(1, at=ix.lab, lab, cex.axis=2)
            ix.y <- seq(0, 1, by=.2)
            axis(2, at=c(0, .2, .6, 1), labels=c("", "0.2", "0.4", "1.0"),
                 las=2, cex.axis=1.8)
            ## 
            wd.pat <- std.patterns %>% filter(grepl(wd.regex[i], Weekdays))
            mtext(paste(N.reg, "rooms controlled by the Registrar,", N.all, "total."),
                  side = 1, adj = 1, line= 3, cex=1.2)
            for (j in 1:nrow(wd.pat)) {
                rect(wd.pat$ix.start[j], 0,
                     wd.pat$ix.end[j], 1,
                     col=alpha(cols_all["Yellow1"], .1), border="black")
            }
            ## Horizontal gridlines
            abline(h=ix.y)
            ## Data
            lines(d.reg$ix, all.week[i,], lwd=3, col=cols_all["Blue2"])
            lines(d.reg$ix, reg.week[i, ], lwd=3, col=cols_all["Red2"])
            ##
            title(main=weekday.lookup[i])
        }
    }, height=800)
}


plot.scatter <- function(rawdata, con.var, xaxis, xlabel, yaxis, title = "", col.cat){
  ggplot(data = rawdata) +
    geom_point(aes_string(x = "Year", y = con.var, color = "Semester"), size=4) +
    geom_path(aes_string(x = "Year", y = con.var, color = "Semester"), 
              linetype = 2, size = 2) +  # Size controls the width of line
                                           # linetype controls the type of line 1=solid, 2=dashed
    scale_x_continuous(xaxis, breaks = c(2009:2018)) +    
    scale_y_continuous(yaxis) + 
    scale_color_manual(values = col.cat) +
    labs(title = title) +
    theme_bw() +  
    theme(plot.title = element_text(size = 20, face = 'bold', hjust = 0.5), 
              legend.position = "right", legend.title=element_text(size=15),
              legend.text=element_text(size=15),
              axis.title=element_text(size=15),
              axis.text = element_text(size=15),
              axis.text.x = element_text(angle = 60, hjust = 1))
}

plot.mosaic <- function(propdata, cat.var, col.cat, title) {
    semesters <- lapply(unique(propdata$termcode),
                        function(tc) {
                            year <- gsub("_[12]", "", tc)
                            sem <- c("Spring", "Fall")[as.numeric(gsub(".*_", "", tc))]
                            paste(sem, year)
                        })
    ggplot(data=propdata, aes_string(x="termcode", y="percentage",
                                     fill=cat.var)) +
        geom_bar(stat="identity",
                 position="fill",
                 width=0.8) +
        geom_text(data = propdata, 
                  aes(x = termcode, label = proplabel, y = position),
                  size = 5) +
        scale_fill_manual("Component",
                          values = col.cat) +
        scale_x_discrete("", label = semesters) +
        scale_y_continuous("Proportion", breaks=c(0, .25, .5, .75, 1)) + 
        labs(title = title) +
        theme_classic() +  
        theme(plot.title = element_text(size = 20, face = 'bold', hjust = 0.5), 
              legend.position = "right", legend.title=element_text(size=15),
              legend.text=element_text(size=15),
              axis.title=element_text(size=15),
              axis.text = element_text(size=15),
              axis.text.x = element_text(angle = 60, hjust = 1))
}

shinyApp(ui, server)

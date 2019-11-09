
## Adapted from OpenStreetMap/R/osm.R

plot.osm <- function(x) {
    mar <- par("mar")
    ##
    plot.new()
    par(mar=c(0, 0, 0, 0))
    plot.window(xlim=c(x$bbox$p1[1], x$bbox$p2[1]),
                ylim=c(x$bbox$p2[2], x$bbox$p1[2]),
                asp = 1, xaxs="i", yaxs="i")
    for (tile in x$tiles) {
        xres <- tile$xres
        yres <- tile$xres
        ##
        rasterImage(as.raster(matrix(tile$colorData, nrow=xres, byrow=TRUE)),
                    x$bbox$p1[1] - .5*abs(x$bbox$p1[1]-x$bbox$p2[1])/yres,
                    x$bbox$p2[2] + .5*abs(x$bbox$p1[2]-x$bbox$p2[2])/xres,
                    x$bbox$p2[1] - .5*abs(x$bbox$p1[1]-x$bbox$p2[1])/yres,
                    x$bbox$p1[2] + .5*abs(x$bbox$p1[2]-x$bbox$p2[2])/xres)
    }
    ##
    par(mar=mar)
}

## plot.OpenStreetMap <- function(x,y=NULL,add=FALSE,removeMargin=TRUE, ...){
## 	mar <- par("mar")
## 	if(add==FALSE){
## 		plot.new()
## 		if(removeMargin)
## 			par(mar=c(0,0,0,0))
## 		plot.window(xlim=c(x$bbox$p1[1],x$bbox$p2[1]),ylim=c(x$bbox$p2[2],x$bbox$p1[2]) ,
## 				xaxs = 'i', yaxs = 'i', asp=T)
## 	}
## 	for(tile in x$tiles)
## 		plot(tile,...)
## 	par(mar=mar)
## }

## plot.osmtile <- function(x, y=NULL, add=TRUE, raster=TRUE, ...){
## 	xres <- x$xres
## 	yres <- x$yres
## 	if(!raster)
## 		image(x=seq(x$bbox$p1[1] - .5*abs(x$bbox$p1[1]-x$bbox$p2[1])/yres,
## 			x$bbox$p2[1] + .5*abs(x$bbox$p1[1]-x$bbox$p2[1])/yres,length=yres),
## 			y=seq(x$bbox$p2[2] - .5*abs(x$bbox$p1[2]-x$bbox$p2[2])/xres,
## 				x$bbox$p1[2] + .5*abs(x$bbox$p1[2]-x$bbox$p2[2])/xres,length=xres),
## 			z=t(matrix(1:(xres*yres),nrow=xres,byrow=TRUE))[,xres:1],
## 			col=x$colorData,add=add,...)
## 	else
## 		rasterImage(as.raster(matrix(x$colorData,nrow=xres,byrow=TRUE)),
## 				x$bbox$p1[1] - .5*abs(x$bbox$p1[1]-x$bbox$p2[1])/yres,
## 				x$bbox$p2[2] + .5*abs(x$bbox$p1[2]-x$bbox$p2[2])/xres,
## 				x$bbox$p2[1] - .5*abs(x$bbox$p1[1]-x$bbox$p2[1])/yres,
## 				x$bbox$p1[2] + .5*abs(x$bbox$p1[2]-x$bbox$p2[2])/xres,
## 				...)
## }

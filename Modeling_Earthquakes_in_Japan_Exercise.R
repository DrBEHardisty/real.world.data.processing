## 2012. "Statistical Thinking in R"
## Use "install.packages” if you don’t have lattice etc yet:

install.packages("latticeExtra")
install.packages("latticedl")
install.packages("maps")

## Use library to open your new data viz. packages etc:
library(latticeExtra)
library(latticedl)
library(maps)

## Call up the data set to model earthquakes in Japan

quakes

quake2 <- quakes

quake2$sendai <- F

sendai <- data.frame(lat=142.369, long=38.322, depth=32, mag=9, stations=NA, sendai=T)

quake2 <- rbind(quake2, sendai)

xyplot(mag~depth, data=quake2, groupls=sendai)
plt <- xyplot(lat~long | cut(depth, 2), data=quakes)
plt <- update(plt, pch=19, cex=0.25, aspect=1)

depth.ord <- rev(order(quakes$depth))

## Cut all the earthquakes into 4 groups of different colors:
heat.colors(4)
heat.colors(4)[cut(quakes$depth, 4, label=F)]

depth.col <- heat.colors(100)[cut(quakes$depth, 100, label=F)]

## We plot based on the colors: Darker colors are
## deeper quakes and lighter colors are shallow quakes
## that we're not so interested in:

plt <- xyplot(lat~long, data=quakes[depth.ord,], aspect="iso", type=c("p", "g"), col="black", pch=21, fill=depth.col[depth.ord], cex=2, xlab="Longitude", ylab="Latitude”)
#Same thing as last problem, except you hardly have to clean any data

data = read.csv("5_5.csv")

#All your cleaning is done here
#Pick coordinates
#Pick dates
pac_indx = which(data[,1]>-60 & data[,1]<60)
cleaned_data = data[pac_indx, 54:103] #1951-2000

svd = svd(cleaned_data)
U=svd$u
D=svd$d
V=svd$v

#a
D[1:10]

#b
x <- 1:72 #360 degrees change in long/ 5 degree steps
y <- 1:24  #120 degrees change in lat/ 5 degree steps
plot.new() #start a new figure from blank
par(mar=c(5,5,2,1))
filled.contour(x, y, matrix(U[,1], ncol = 24), 
               key.title = title(main = "Scale"),
               plot.axes =  {axis(1,seq(1,72, length = 7), cex.axis=1.3, labels = c(-180, -120, -60, 0, 60, 120, 180))  
                 axis(2,at = seq(1,24, length=7), cex.axis=1.3, labels = c(-60, -40, -20, 0, 20, 40, 60))},
               plot.title = title(main = "Spatial pattern (EOF): Global(-Artic) (1951)",
                                  xlab="Longitude",
                                  ylab="Latitude", cex.lab=1.5),
               color.palette = colorRampPalette(c("red", "white", "blue")))

par(mar=c(5,5,2,1))
filled.contour(x, y, matrix(U[,2], ncol = 24), 
               key.title = title(main = "Scale"),
               plot.axes =  {axis(1,seq(1,72, length = 7), cex.axis=1.3, labels = c(-180, -120, -60, 0, 60, 120, 180))  
                 axis(2,at = seq(1,24, length=7), cex.axis=1.3, labels = c(-60, -40, -20, 0, 20, 40, 60))},
               plot.title = title(main = "Spatial pattern (EOF): Global(-Artic) (1952)",
                                  xlab="Longitude",
                                  ylab="Latitude", cex.lab=1.5),
               color.palette = colorRampPalette(c("red", "white", "blue")))

par(mar=c(5,5,2,1))
filled.contour(x, y, matrix(U[,3], ncol = 24), 
               key.title = title(main = "Scale"),
               plot.axes =  {axis(1,seq(1,72, length = 7), cex.axis=1.3, labels = c(-180, -120, -60, 0, 60, 120, 180))  
                 axis(2,at = seq(1,24, length=7), cex.axis=1.3, labels = c(-60, -40, -20, 0, 20, 40, 60))},
               plot.title = title(main = "Spatial pattern (EOF): Global(-Artic) (1953)",
                                  xlab="Longitude",
                                  ylab="Latitude", cex.lab=1.5),
               color.palette = colorRampPalette(c("red", "white", "blue")))

#c
par(mfrow=c(1,1))
par(mar=c(5,4,2,1))
plot(1:50, V[,1],type="o",col="red",lwd=2,
     main="Temporal Pattern (Principal Components): Time series 1",xlab="Time (Years)",
     ylab="PC1 values: dimensionless",
     cex.lab=1.3, cex.axis=1.3)

par(mfrow=c(1,1))
par(mar=c(5,4,2,1))
plot(1:50, V[,2],type="o",col="red",lwd=2,
     main="Temporal Pattern (Principal Components): Time series 2",xlab="Time (Years)",
     ylab="PC2 values: dimensionless",
     cex.lab=1.3, cex.axis=1.3)

par(mfrow=c(1,1))
par(mar=c(5,4,2,1))
plot(1:50, V[,3],type="o",col="red",lwd=2,
     main="Temporal Pattern (Principal Components): Time series 3",xlab="Time (Years)",
     ylab="PC3 values: dimensionless",
     cex.lab=1.3, cex.axis=1.3)


#d
library(maps)
Lat=seq(-57.5,57.5, by=5)
Lon=seq(2.5, 357.5, by=5)
par(mar=c(4,5,3,0))
mapmat=matrix(cleaned_data[,1], nrow=72)
int=seq(-5,5,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 'green','white', 'yellow','red','maroon'),interpolate='spline')

filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(0,360),ylim=c(-90,90),
               plot.title=title(main="Pacific Precipitation [in]: Annual 1951",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

par(mar=c(4,5,3,0))
mapmat=matrix(cleaned_data[,2], nrow=72)
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(0,360),ylim=c(-90,90),
               plot.title=title(main="Pacific Precipitation [in]: Annual 1952",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

par(mar=c(4,5,3,0))
mapmat=matrix(cleaned_data[,3], nrow=72)
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               xlim=c(0,360),ylim=c(-90,90),
               plot.title=title(main="Pacific Precipitation [in]: Annual 1953",
                                xlab="Latitude",ylab="Longitude", cex.lab=1.5),
               plot.axes={axis(1, cex.axis=1.5); axis(2, cex.axis=1.5);
                 map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"),
               key.axes={axis(4, cex.axis=1.5)})

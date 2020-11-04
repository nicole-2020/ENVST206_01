#activity 8
install.packages(c("raster"))

library(raster)
library(ggplot2)
library(rgdal)
#set up directory for oneida data folder
dirR <- "/Users/nicolepapert/Desktop/a08/oneida"

#read in Sentinel data

rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))

plot(rdatB2/10000)
#stack red green blue
rgbS <- stack(rdatB4,rdatB3,rdatB2)/10000
#view raster, a few pixels in blue have reflectanc above 1 so set scale
plotRGB(rgbS, scale=2)
#don't need the scale argument when adding in the contrast stretch
plotRGB(rgbS, stretch="lin")
#full resolutions

#get the total number of pixels by multiplying the number of rows and columns #in the raster
plotRGB(rgbS, stretch="lin",maxpixels=rgbS@nrows*rgbS@ncols)
rgbS@nrows*rgbS@ncols
plotRGB(rgbS, stretch="hist",maxpixels=rgbS@nrows*rgbS@ncols)

#calculate NDVI
#NIR-red/(NIR + RED)
NDVI <- (rdatB8 - rdatB4) / (rdatB8 + rdatB4)
#visualize NDVI across the Oneida lake area 
plot(NDVI)

#read in landcover points data
#you may have to change your slash direction if you are on a windows computer #I've also turned off the info print out here when you read in the file

algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"), verbose=FALSE) 
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"), verbose=FALSE) 
water <- readOGR(paste0(dirR,"/Oneida/water.shp"), verbose=FALSE) 
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"), verbose=FALSE)

#plot points and true color
plotRGB(rgbS, stretch="lin",maxpixels=2297430)
plot(algae, add=TRUE, col=rgb(0.5,0.5,0.5,0.5), pch=19) 
plot(agri, add=TRUE, col=rgb(0.75,0.5,0.5,0.5), pch=19) 
plot(forest, add=TRUE, col=rgb(0.75,0.75,0.25,0.5), pch=19) 
plot(water, add=TRUE, col=rgb(0.33,0.75,0.75,0.5), pch=19) 
plot(wetlands, add=TRUE, col=rgb(0.33,0.33,0.65,0.5), pch=19) 
legend("bottomleft", c("algae","agri","forest","water","wetlands"),
       pch=19,
col=c(rgb(0.5,0.5,0.5,0.5),rgb(0.75,0.5,0.5,0.5),rgb(0.75,0.75,0.25,0.5),rgb(0.33,0.75,0.75, 0.5),rgb(0.33,0.33,0.65,0.5)),
            bty="n", cex=0.75)                                                                                                                                                                                                                                                                                                            pch=19, col=c(rgb(0.5,0.5,0.5,0.5),rgb(0.75,0.5,0.5,0.5),rgb(0.75,0.75,0.25,0.5),rgb(0.33,0.75,0.75, 0.5),rgb(0.33,0.33,0.65,0.5)),
                                                                                                                                                                                                                                                                                                            bty="n", cex=0.75)
#set up a dataframe with all of the point coordinates
landExtract <- data.frame(landcID = as.factor(rep(c("algae","water","agri","forest","wetland"),each=120)),
                          x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],forest@coords[,1],wetlands@coords [,1]),
                          y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],forest@coords[,2],wetlands@coords [,2]))

#stack all bands
allbands <- stack(rdatB2, rdatB3, rdatB4,rdatB8)/10000
#add the raster reflectance values to the point coordinates and classes 
#extract(raster, matrix of coordinates)
#raster:: helps ensure that extract comes from the raster package 
ExtractOut <- raster::extract(allbands,landExtract[,2:3])
#name the bands
colnames(ExtractOut) <- c("B02","B03","B04","B08")
#combine the original data with the coordinates with the raster data 
rasterEx <- cbind(landExtract,ExtractOut)
#look at data
head(rasterEx)

ggplot(data=rasterEx, aes(x=B02, y=B03, color=landcID))+ geom_point(alpha=0.6)+
  theme_classic()



#blue-infrared
ggplot(data=rasterEx, aes(x=B02, y=B08, color=landcID)) + 
  geom_point(alpha= 0.6) +
  theme_classic()+
  ggtitle("Blue vs. Near Infrared Reflectance")

#green-infrared
ggplot(data=rasterEx, aes(x=B03, y=B08, color=landcID)) + 
  geom_point(alpha= 0.6) +
  theme_classic()+
  ggtitle("Green vs. Near Infrared Reflectance")

#red-infrared
ggplot(data=rasterEx, aes(x=B04, y=B08, color=landcID)) + 
  geom_point(alpha= 0.6) +
  theme_classic()+
  ggtitle("Red vs. Near Infrared Reflectance")

extractNDVI <- raster::extract(NDVI,landExtract[,2:3])
NDVIland <- cbind(landExtract,extractNDVI)
ggplot(NDVIland[NDVIland$landcID== c("agri", "forest", "wetland"),], aes(x=landcID, y=extractNDVI))+
  geom_violin(fill="plum2")+
  geom_boxplot(width=0.1,size=0.25, fill="thistle2")+
  theme_classic()+labs(y="NDVI", x="land cover type")



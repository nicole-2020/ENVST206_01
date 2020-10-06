#activity 5
#read in weather station file from the data folder
datW <- read.csv("/Users/nicolepapert/npapert/GitHub/activities/ENVST206_01/noaa2011124 (1).csv") 
#specify that the name column should be a factor
datW$NAME<- as.factor(datW$NAME)
#set up a vector of all names for each level
nameS <- levels(datW$NAME)
nameS
#make a dataframe with just precipitation, year, and site name
#remove NA using na.omit
datP <- na.omit(data.frame(NAME=datW$NAME, year=datW$year, PRCP=datW$PRCP))
#total annual precipitation (mm)
precip <- aggregate(datW$PRCP, by=list(datW$NAME,datW$year), FUN="sum", na.rm=TRUE) #use aggregate to get total annual precipitation
precip <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="sum", na.rm=TRUE) #rename columns
colnames(precip) <- c("NAME","year","totalP")
#add the x column from aggregate looking at the length of observations in each year 
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x
#make a new dataframe
pr <- precip[precip$ncount >=364, ]
#look at only livermore california and morrisville new york preciptiation 
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]
#make a plot of california precip 
plot(ca$year, ca$totalP)
#make a plot of california precip 
plot(ca$year, ca$totalP,
type = "b",
pch = 19,
ylab = "Annual precipitation (mm)", xlab = "Year")
#make a plot of california precip 
plot(ca$year, ca$totalP,
type = "b",
pch = 19,
ylab = "Annual precipitation (mm)", xlab = "Year",
yaxt = "n")
#add y axis
#add y axis
axis(2, seq(200,800, by=200), las=2 )
#add arizona
points(ny$year, ny$totalP,
       type = "b", pch = 19,
       col="tomato3")
#fixing axis range
plot(ca$year, ca$totalP, type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)", xlab = "Year",
     yaxt = "n",
     ylim =c(0, 1600))
#add y axis
axis(2, seq(0,1600, by=400), las=2 ) 
#add arizona
points(ny$year, ny$totalP, type = "b",
       pch = 19, col="tomato3")
#add legend
legend("topleft", #position
       c("California", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn


datMaxTemp <- na.omit(data.frame(NAME=datW$NAME, year=datW$year, tmax = datW$TMAX))

maxtemp <- aggregate(datW$TMAX, by=list(datW$NAME,datW$year), FUN="sum", na.rm=TRUE)
maxtemp <- aggregate(datMaxTemp$tmax, by=list(datMaxTemp$NAME,datMaxTemp$year), FUN="sum", na.rm=TRUE) #rename columns
colnames(maxtemp) <- c("NAME","year","MaxTemp")

maxtemp$ncount <- aggregate(datMaxTemp$tmax, by=list(datMaxTemp$NAME,datMaxTemp$year), FUN="length")$x

mtemp <- maxtemp[maxtemp$ncount >=364, ]

nd <- mtemp[mtemp$NAME == nameS[3], ]
ny <- mtemp[mtemp$NAME == nameS[5], ]
nd$MaxTemp <-nd$MaxTemp/nd$ncount
ny$MaxTemp <-ny$MaxTemp/ny$ncount
plot(nd$year, nd$MaxTemp, type = "b",
     pch = 18,
     main = "Mean Annual Maximum Temperature in Morrisville, NY and Mandan, North Dakota",
     col= "skyblue",
     ylab = "Annual Maximum Temperature (Celcius)", xlab = "Year",
     yaxt = "n",
     ylim =c(6, 16))

axis(2, seq(0,5400, by=2), las=2 ) 

points(ny$year, ny$MaxTemp, type = "b",
       pch = 18, col="violet")

legend("topleft", #position
       c("South Dakota", "New York"), #labels
       col= c("skyblue", "violet"), #colors
       pch=18, #point shape
       lwd=2, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn

install.packages("ggplot2")
library(ggplot2)
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation") #make axis labels

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation")+ #make axis labels 
  theme_classic() #change plot theme

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ geom_point(alpha=0.5)+
  geom_path(alpha=0.5)+
  labs(x="year", y="Annual Precipitation")+ theme_classic()+
  
  scale_color_manual(values = c("tomato","violet", "powderblue", "lightpink","palegreen"))

ggplot(data = datW, aes(x=NAME, y=TMIN))+ #look at daily tmin 
  geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color 
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal 
theme_classic() #git rid of ugly gridlines

sub <- datW[datW$NAME == nameS[4] & datW$ year == 1974,]
#specify date format
#%Y means a four number year
#- indicates that the date uses dashes to seperate #%m means month
#%d means day
sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")

ggplot(data=sub, aes(x=DATE, y=TMAX))+ geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

ggplot(data=sub, aes(x=DATE, y=PRCP))+ geom_col(fill="royalblue3")+ theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")


sub1 <- datW[datW$NAME == nameS[1] & datW$ year == 1974,]
#specify date format
#%Y means a four number year
#- indicates that the date uses dashes to seperate #%m means month
#%d means day
sub1$DATE <- as.Date(sub1$DATE,"%Y-%m-%d")

ggplot(data=sub1, aes(x=DATE, y=TMAX))+ geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)", title= 'Maximum Temperatures in Aberdeen, WA US')

ggplot(data=sub1, aes(x=DATE, y=PRCP))+ geom_col(fill="royalblue3")+ theme_classic()+
  labs(x="year", y="Daily precipitation (mm)", title= 'Daily Precipitation Levels in Aberdeen, WA US')

mormon <- datW[datW$NAME == nameS[4] & datW$year > 1999,]
mormon$DATE <- as.Date(mormon$DATE,"%Y-%m-%d")


ggplot(data = mormon, aes(x=NAME, y=TMIN))+
  geom_violin(fill=rgb(0.963,0.853,0.88))+ 
  geom_boxplot(width=0.2,size=0.25, fill="pink")+
  theme_classic() +
  labs(x = "", y="Minimum temperature (C)", title= 'The Distribution of Minimum Temperatures in Mormon, AZ US from 2000-2019')














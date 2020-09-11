#activity 2

heights <- c(3, 2, 3)
datW <- read.csv("/Users/nicolepapert/npapert/GitHub/activities/ENVST206_01/envst206activity2data.csv")
datW$NAME <- as.factor(datW$NAME)
levels(datW$NAME)
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp
#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)
#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="pink",
     border="white")
help("hist")
help(hist)
help(dnorm)
#pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)) - pnorm(0,
                                                         mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                         sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#pnrom of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#Here I'm calculating the value of the 95th quantile or a probability of 0.95
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1] + 4,na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
hist(datW$PRCP[datW$siteN == 1],
     freq=TRUE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Levels of Precipitation (mm)", 
     breaks = 45,
     xlim = c(0, 90), ylab="Frequency",
     col="turquoise",
     border="white")
AnnualPRCP <- aggregate(datW$TAVE, by=list(datW$NAME, datW$year), FUN="sum",na.rm=TRUE)
AnnualPRCP
hist(AnnualPRCP$x[AnnualPRCP$Group.1 == "ABERDEEN, WA US"],
     freq=TRUE, 
     main = "Aberdeen, WA US",
     xlab = "Levels of Precipitation (mm)", 
     breaks = 15,
     xlim = c(500, 3500), 
     ylab="Frequency",
     col="turquoise",
     border="white")

hist(AnnualPRCP$x[AnnualPRCP$Group.1 == "MANDAN EXPERIMENT STATION, ND US"],
     freq=TRUE, 
     main = "Mandan Experiment Station, ND US",
     xlab = "Levels of Precipitation (mm)", 
     breaks = 15,
     xlim = c(500, 3500), 
     ylab="Frequency",
     col="turquoise",
     border="white")
pnorm(700,
      mean(AnnualPRCP$x[AnnualPRCP$Group.1 == "ABERDEEN, WA US"],na.rm=TRUE),
      sd(AnnualPRCP$x[AnnualPRCP$Group.1 == "ABERDEEN, WA US"],na.rm=TRUE))
pnorm(700,
      mean(AnnualPRCP$x[AnnualPRCP$Group.1 == "MANDAN EXPERIMENT STATION, ND US"],na.rm=TRUE),
      sd(AnnualPRCP$x[AnnualPRCP$Group.1 == "MANDAN EXPERIMENT STATION, ND US"],na.rm=TRUE))     

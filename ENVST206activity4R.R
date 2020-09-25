#Reading in the data
datB <- read.csv("/Users/nicolepapert/npapert/Github/activities/ENVST206_01/beaver_dam.csv") 
head(datB)
#scatterplot of # of dams vs. total area of surface area
plot(datB$dams.n, datB$area.h, pch = 19,
     col = "royalblue4",
     ylab = "Surface water area (ha)", xlab = "Number of beaver dams")
#set up regression
dam.mod <- lm(datB$area.ha ~ datB$dams.n)
#get standarized residuals 
dam.res <- rstandard(dam.mod)
#set up qq plot 
qqnorm(dam.res)
#add qq line
qqline(dam.res)
shapiro.test(dam.res)
#make residual plot 
plot(datB$dams.n, dam.res,
  xlab = "beaver damns",
  ylab = "standardized residual")
#add a horizontal line at zero 
abline(h=0)
#summary function to print out regression table
summary(dam.mod)
#make plot of beaver dams and surface water 
plot(datB$dams.n, datB$area.h,
pch = 19,
col = "royalblue4",
ylab = "Surface water area (ha)", xlab = "Number of beaver dams")
#add regression line #make line width thicker 
abline(dam.mod, lwd=2)
pheno <- read.csv("/Users/nicolepapert/npapert/Github/activities/ENVST206_01/red_maple_pheno.csv")
#set up panel of plots with one row and four columns 
par(mfrow=c(1,4))
plot(pheno$Lat,pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Latitude")
plot(pheno$elev,pheno$doy, pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out", 
     xlab = "Elevation")
plot(pheno$Tmax,pheno$doy, pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out", 
     xlab = "Maximum temperature (C)")
pheno$siteDesc <- as.factor(pheno$siteDesc)
plot(pheno$siteDesc,pheno$doy, pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out", 
     xlab = "Urban/Rural Site")
dev.off()
plot( ~ pheno$Lat + pheno$Tmax+ pheno$Tmin +pheno$Prcp + pheno$elev + pheno$siteDesc)
pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)
mlr <- lm(pheno$doy ~ pheno$Tmax + pheno$Prcp + pheno$elev + pheno$urID)
mlFitted <- fitted(mlr)
#get standarized residuals 
mlr.res <- rstandard(mlr)
#set up qq plot 
qqnorm(mlr.res)
#add qq line
qqline(mlr.res)
#summarize regression
summary(mlr)
plot(mlFitted, mlr.res,
     xlab = "Fitted Values",
     ylab = "standardized residual")
#add a horizontal line at zero 
abline(h=0)




































































#air pollution
data <- read.csv("/Users/nicolepapert/Desktop/environmental data science/uspollution(1).csv")

#create a new dataframe with just New York City data
NewYorkData <- na.omit(data.frame(City=data[data$City == "New York",]))

#Since the dataset repeats rows, we need to make a new datatset with one observation 
#per day by using the aggregate function

NewYorkNO2 <-aggregate(NewYorkData$City.NO2.Mean, by=list(NewYorkData$City.Date.Local), FUN="mean", na.rm=TRUE)
NewYorkSO2 <-aggregate(NewYorkData$City.SO2.Mean, by=list(NewYorkData$City.Date.Local), FUN="mean", na.rm=TRUE)
NewYorkCO <-aggregate(NewYorkData$City.CO.Mean, by=list(NewYorkData$City.Date.Local), FUN="mean", na.rm=TRUE)
NewYorkO3 <-aggregate(NewYorkData$City.O3.Mean, by=list(NewYorkData$City.Date.Local), FUN="mean", na.rm=TRUE)

#Using full join to join the aggregated datasets together
NY1 <- full_join(NewYorkNO2,NewYorkSO2, by="Group.1")
NY2 <- full_join(NY1 ,NewYorkCO, by="Group.1")
NewYork <- full_join(NY2 ,NewYorkO3, by="Group.1")

#Finally, we must rename the columns so that the new dataset is easy to understand
NewYork <- NewYork %>% 
  rename(
    Day = Group.1,
    NO2.Mean = x.x,
    SO2.Mean = x.y,
    CO.Mean = x.x.x,
    O3.Mean = x.y.y,
  )

#SO2 variable has exponential distribution, so I am taking log(SO2+1).
SO2_log <- log(NewYork$SO2.Mean+1)

#Looking at the histograms of each pollutnt to see what the distribution looks like
hist(NewYork$NO2.Mean,
     main = "Distribution of Mean NO2 Levels", 
     xlab = "Daily mean (in parts per billion)",
     ylab = "Frequency",
     col = "hotpink")

hist(NewYork$O3.Mean,
     main = "Distribution of Mean O3 Levels", 
     xlab = "Daily mean (in parts per million)",
     ylab = "Frequency",
     col = "plum")

hist(SO2_log,
     main = "Distribution of Mean SO2 Levels", 
     xlab = "Daily mean (in parts per billion)",
     ylab = "Frequency",
     col = "skyblue")

hist(NewYork$CO.Mean,
     main = "Distribution of Mean CO Levels", 
     xlab = "Daily mean (in parts per million)",
     ylab = "Frequency",
     col = "palegreen1")

#Checking the scatterplots of the different pollutants to see which ones could potentially have a linear relationship
plot( ~ NewYork$O3.Mean + NewYork$NO2.Mean + SO2_log + NewYork$CO.Mean)
      

#Setting up the regression
O3_NO2.mod <- lm(NewYork$O3.Mean ~ NewYork$NO2.Mean)
O3_CO.mod <- lm(NewYork$O3.Mean ~ NewYork$CO.Mean)
O3_SO2_log.mod <- lm(NewYork$O3.Mean ~ SO2_log)
NO2_SO2_log.mod <- lm(NewYork$NO2.Mean ~ SO2_log)
NO2_CO.mod <- lm(NewYork$NO2.Mean ~ NewYork$CO.Mean)
CO_SO2_log.mod <- lm(NewYork$CO.Mean ~ SO2_log)


#get standarized residuals 
O3_NO2.res <- rstandard(O3_NO2.mod)
O3_CO.res <- rstandard(O3_CO.mod)
O3_SO2_log.res <- rstandard(O3_SO2_log.mod)
NO2_SO2_log.res <- rstandard(NO2_SO2_log.mod)
NO2_CO.res <- rstandard(NO2_CO.mod)
CO_SO2_log.res <- rstandard(CO_SO2_log.mod)


#Setting up qq plot 
qqnorm(O3_NO2.res)
qqnorm(O3_CO.res)
qqnorm(O3_SO2_log.res)
qqnorm(NO2_SO2_log.res)
qqnorm(NO2_CO.res)
qqnorm(CO_SO2_log.res)


#add qq line
qqline(O3_NO2.res)
qqline(O3_CO.res)
qqline(O3_SO2_log.res)
qqline(NO2_SO2_log.res)
qqline(NO2_CO.res)
qqline(CO_SO2_log.res)


#I can't use a Shapiro-Wilks Test because the sample exceeds 5,000 data points, 
#so I will have to rely on the Q-Q Plot to access normality. 


#Making the residual plots
plot(NewYork$NO2.Mean, O3_NO2.res,
     xlab = "Mean NO2 Levels",
     ylab = "Standardized residual",
     main = "Residual plot fot O3 and NO2")
abline(h=0)

plot(NewYork$CO.Mean, O3_CO.res,
     xlab = "Mean CO Levels",
     ylab = "Standardized residual",
     main = "Residual plot fot O3 and CO")
abline(h=0)

plot(SO2_log, O3_SO2_log.res,
     xlab = "Mean log(SO2+1) Levels",
     ylab = "Standardized residual",
     main = "Residual plot fot O3 and log(SO2 + 1)")
abline(h=0)

plot(SO2_log, NO2_SO2_log.res,
     xlab = "Mean log(SO2+1) Levels",
     ylab = "Standardized residual",
     main = "Residual plot fot NO2 and log(SO2 + 1)")
abline(h=0)

plot(NewYork$CO, NO2_CO.res,
     xlab = "Mean CO Levels",
     ylab = "Standardized residual",
     main = "Residual plot fot NO2 and CO")
abline(h=0)

plot(SO2_log, CO_SO2_log.res,
     xlab = "Mean log(SO2+1) Levels",
     ylab = "Standardized residual",
     main = "Residual plot fot CO and log(SO2 + 1)")
abline(h=0)


#summary function to print out regression table
summary(O3_NO2.mod)
summary(O3_CO.mod)
summary(O3_SO2_log.mod)
summary(NO2_SO2_log.mod)
summary(NO2_CO.mod)
summary(CO_SO2_log.mod)  #This is not relevant to report because
                          #none of the relationships passed the Q-Q Plot
                          #test, but I left the code in the R script anyways
                          #in case you want to look at it!


#Making the plots with regression lines!

#O3 and NO2
plot(NewYork$NO2.Mean, NewYork$O3.Mean,
     main = "Plot of Mean O3 and NO2 Levels",
     pch = 19,
     col = "royalblue4",
     ylab = "O3 Daily Mean (in parts per million)", xlab = "NO2 Daily Mean (in parts per billion)")

abline(O3_NO2.mod, lwd=2)

#O3 and CO
plot(NewYork$CO.Mean, NewYork$O3.Mean,
     main = "Plot of Mean O3 and CO Levels",
     pch = 19,
     col = "slateblue1",
     ylab = "O3 Daily Mean (in parts per million)", xlab = "CO Daily Mean (in parts per million)")

abline(O3_CO.mod, lwd=2)

#O3 and SO2
plot(SO2_log, NewYork$O3.Mean,
     main = "Plot of Mean log(SO2 + 1) and CO Levels",
     pch = 19,
     col = "tomato",
     ylab = "O3 Daily Mean (in parts per million)", xlab = "log(SO2 +1) Daily Mean (in parts per billion)")

abline(O3_SO2_log.mod, lwd=2)

#NO2 and SO2
plot(SO2_log, NewYork$NO2.Mean,
     main = "Plot of Mean log(SO2 + 1) and NO2 Levels",
     pch = 19,
     col = "purple2",
     ylab = "NO2 Daily Mean (in parts per billion)", xlab = "log(SO2 +1) Daily Mean (in parts per billion)")

abline(NO2_SO2_log.mod, lwd=2)

#NO2 and CO
plot(NewYork$CO.Mean, NewYork$NO2.Mean,
     main = "Plot of Mean CO and NO2 Levels",
     pch = 19,
     col = "red3",
     ylab = "NO2 Daily Mean (in parts per billion)", xlab = "CO Daily Mean (in parts per million)")

abline(NO2_CO.mod, lwd=2)

#SO2 and CO
plot(SO2_log, NewYork$CO.Mean,
     main = "Plot of Mean log(SO2 + 1) and CO Levels",
     pch = 19,
     col = "navy",
     ylab = "CO Daily Mean (in parts per million)", xlab = "log(SO2 +1) Daily Mean (in parts per billion)")

abline(CO_SO2_log.mod, lwd=2)






























































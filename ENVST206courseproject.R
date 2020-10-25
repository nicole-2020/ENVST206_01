#air pollution
data <- read.csv("/Users/student/uspollution(1).csv")
meanNO2 <- mean(data$NO2.Mean)
print(meanNO2)
sdNO2 <- sd(data$NO2.Mean)
print(sdNO2)
meanO3 <- mean(data$O3.Mean)
print(meanO3)
sdO3 <- sd(data$O3.Mean)
print(sdO3)
meanCO <- mean(data$CO.Mean)
print(meanCO)
sdCO <- sd(data$CO.Mean)
print(sdCO)
meanSO2 <- mean(data$SO2.Mean)
print(meanSO2)
sdSO2 <- sd(data$SO2.Mean)
print(sdSO2)


hist(data$NO2.Mean, 
     main = "Distribution of Mean NO2 Levels",
     xlab = "Daily mean (Parts per billion)", 
     ylab = "Frequency",
     col = "skyblue")
hist(data$O3.Mean, 
     main = "Distribution of Mean O3 Levels",
     xlab = "Daily mean (Parts per million)", 
     ylab = "Frequency",
     col = "skyblue")

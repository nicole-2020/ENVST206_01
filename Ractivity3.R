#Activity 3
ch4 <- read.csv("/Users/nicolepapert/npapert/GitHub/activities//ENVST206_01/lemming_herbivory.csv")
#Treating the herbitory column as factor data: 
ch4$herbivory <- as.factor(ch4$herbivory)
#Boxplot
plot(ch4$CH4_Flux ~ ch4$herbivory, xlab ="Treatment", ylab="CH4 fluxes (mgC m –2 day–1) ")
#Checking if the data is normally distributed
#shapiro-wilk test on grazing plots 
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])
#shapiro-wilk test on grazing exclusion plots 
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
#use bartlett test since testing for equal variance 
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)
#2 sample t-test
t.test(ch4$CH4_Flux ~ ch4$herbivory)
#read in insect data
datI <- read.csv("/Users/nicolepapert/npapert/GitHub/activities/ENVST206_01/insect_richness.csv")
datI
#converting names to factor data
datI$urbanName <- as.factor(datI$urbanName)
#shapiro-wilk test on Suburban 
shapiro.test(datI$Richness[datI$urbanName == "Suburban"])
#shapiro-wilk test on Dense
shapiro.test(datI$Richness[datI$urbanName == "Dense"])
#shapiro-wilk test on Developed
shapiro.test(datI$Richness[datI$urbanName == "Developed"])
#shapiro-wilk test on Natural
shapiro.test(datI$Richness[datI$urbanName == "Natural"])
#use bartlett test testing for equal variance 
bartlett.test(datI$Richness ~ datI$urbanName)
#specify model for species richness and urban type 
in.mod <- lm(datI$Richness ~ datI$urbanName) 
#run the ANOVA
in.aov <- aov(in.mod)
#print out ANOVA table 
summary(in.aov)
#run Tukey HSD
tukeyT <- TukeyHSD(in.aov) 
#view results of Tukey HSD
tukeyT
#make a plot
#make axes labels smaller than usual to fit on plot using cex.axis 
plot(tukeyT, cex.axis=0.75)
tapply(datI$Richness, datI$urbanName, "mean")

#set up contigency table
species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE) 
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")
#make a mosaic plot with an informative title and axes labels 
mosaicplot(species, xlab="population status", ylab="legal protection",
       main="Legal protection impacts on populations")
totalspecies = 73
73/4
#Conduct a chi-squared test
chisq.test(species)
##
## Pearson's Chi-squared test with Yates' continuity correction ##
## data: species






































































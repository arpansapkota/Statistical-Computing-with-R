#Work 1: Work/Assignment 1:

#Show the histogram of the z variable and interpret it carefully.
z<- c(1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,5,5,5,6,6,7)
hist(z)

#Get a summary of this variable and decide which measure of central tendency and measure of dispersion must be used for this variable?
summary(z)

#Get the five number summary of this variable and interpret them carefully.
fivenum(z)

#Create a boxplot of this variable and interpret it carefully.
boxplot(z)

#Outlier from box plot
boxplot(z)$out

#Outlier Index
which(z %in% boxplot(z)$out)


#Work 2: Working with covnep_252days.csv file

#Importing the csv file using readr pkg as base pkg is slow
library(readr)
covnep_252days <- read_csv("/Users/arpan/desktop/mds/01 MDS I-I/MDS 503 - Statistical Computing with R/lab/Arpan Sapkota - covnep_252days.csv")

#Date is in character data type so need to change into date
class(covnep_252days$date)
covnep_252days$date <- as.Date(covnep_252days$date, format = "%m/%d/%Y")

#Plotting Date Vs Total Case (Cumulative Sum of total cases)
plot(covnep_252days$date,cumsum(covnep_252days$totalCases))

#Get summary of totalCases variable:
summary(covnep_252days$totalCases)
boxplot(covnep_252days$totalCases)

#Removing the outliers from the data
bp<-boxplot.stats(covnep_252days$totalCases)
outliers <- bp$out
clean_data <- subset(covnep_252days$totalCases, !covnep_252days$totalCases %in% outliers)
summary(clean_data)

#df<-data.frame(covnep_252days,stringsAsFactors = FALSE)
#summary(df$totalCases)

#Get histogram of newCases
hist(covnep_252days$newCases)

#Get summary of newCases
summary(covnep_252days$newCases)
boxplot(covnep_252days$newCases)


#Work 3: Working with SAQ8.sav file
(.packages()) #list packages
#install.packages("foreign")

library(foreign)
Arpan_Sapkota_SAQ8 <- read.spss("Desktop/MDS/01 MDS I-I/MDS 503 - Statistical Computing with R/Lab/Arpan Sapkota - SAQ8.sav")

library(haven)
Arpan_Sapkota_SAQ8 <- read_sav("Desktop/MDS/01 MDS I-I/MDS 503 - Statistical Computing with R/Lab/Arpan Sapkota - SAQ8.sav")

table(Arpan_Sapkota_SAQ8)
summary(Arpan_Sapkota_SAQ8$q01)

install.packages("sjPlot")
library(sjPlot)

data(efc)
sjt.frq(efc$e42dep)

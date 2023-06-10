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

#Problem in summary The minimum value can’t be 0
#So, we change row 2:60 of totalCases as 1
covnep_252days$totalCases[2:60] <- 1

#Again Get correct summary of totalCases:
summary(covnep_252days$totalCases)

#Removing the outliers from the data
#bp<-boxplot.stats(covnep_252days$totalCases)
#outliers <- bp$out
#clean_data <- subset(covnep_252days$totalCases, !covnep_252days$totalCases %in% outliers)
#summary(clean_data)

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
Arpan_Sapkota_SAQ8_AttributesName <- read_sav("Desktop/MDS/01 MDS I-I/MDS 503 - Statistical Computing with R/Lab/Arpan Sapkota - SAQ8.sav")
names(Arpan_Sapkota_SAQ8_AttributesName)

# View the current column names
names(Arpan_Sapkota_SAQ8)

# Rename the columns with its attribute name
names(Arpan_Sapkota_SAQ8) <- c("Statistics makes me cry", "My friend will think I'm stupid for not being able to cope with SPSS", "Standard deviations excite me", "I dream that Pearson is attacking me with correlation coefficients", "I don't understand statistics", "I have little experience of computers", "All computers hate me", "I have never been good at mathematics")

#install.packages('epiDisplay')
library(epiDisplay)

tab1(Arpan_Sapkota_SAQ8$`Statistics makes me cry`)
tab1(Arpan_Sapkota_SAQ8$`Standard deviations excite me`)
tab1(Arpan_Sapkota_SAQ8$`I have little experience of computers`)
tab1(Arpan_Sapkota_SAQ8$`I have never been good at mathematics`)

#Easy way to get the frequency table but sjp.frq() wont work now
#library(sjPlot)
#sjp.frq()

##Another Way
library(foreign)
Arpan_Sapkota_SAQ8 <- read.spss(file.choose())
Arpan_Sapkota_SAQ8 <- read.spss(Arpan_Sapkota_SAQ8.sav) #working folder
class(Arpan_Sapkota_SAQ8) 
str(Arpan_Sapkota_SAQ8)
library(plyr)
table.q1 <- count(Arpan_Sapkota_SAQ8$q01)
table.q1$percent <- round(table.q1$freq/sum(table.q1$freq)*100,1)
table.q1$val.percent <- table.q1$percent #As no missing values present!
table.q1$cum.percent <- cumsum(table.q1$percent)
colnames(table.q1) <- c("Q01", "Frequency", "Percent", "Valid Percent","Cumulative Percent")
table.q1 #And same for other tables 



#Work 4: Working with MR_drugs.xls file
# Calculate the multiple response frequencies for all the income columns

#install.packages("readxl")
library(readxl)

#Loading the xls file
Arpan_Sapkota_MR_Drugs <- read_excel("Desktop/MDS/01 MDS I-I/MDS 503 - Statistical Computing with R/Lab/Arpan Sapkota - MR_Drugs.xlsx")

# Install the summarytools package 
#install.packages("summarytools")

# Load the summarytools package
#library(summarytools)

head(Arpan_Sapkota_MR_Drugs)
#names(Arpan_Sapkota_MR_Drugs[4:10])

# Calculate the counts and percentages for each income variable
incomes <- c("inco1", "inco2", "inco3", "inco4", "inco5", "inco6", "inco7")
counts <- sapply(Arpan_Sapkota_MR_Drugs[incomes], sum)
percentages <- round(counts/sum(counts) * 100, 1)

# Create a data frame with the counts and percentages
income_freq <- data.frame(
  Income = incomes,
  Frequencies = counts,
  Percent = percentages,
  `Percent.of.Cases` = round(percentages / 100 * 182.9, 1)
)

# Add a row for the total count and percentage
income_freq <- rbind(
  income_freq,
  c("Total", sum(counts), 100, 182.9)
)

# Print the table
income_freq

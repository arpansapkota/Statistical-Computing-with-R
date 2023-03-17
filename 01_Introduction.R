data <- c(1,2,3,4,5,6,7,8,9) #num 
#data1 <- c(1:9) int
text <- c("a","b","c","d","e","f","g","h","i")
matrix <- cbind(data,text)
print(matrix)
class(matrix)

#We create df, df upto 2 GB data data.table = upto 20GB

#define x varibale as a series from 1 to 30
x<- c(1:30)
y <- x^3

#Plot Histogram
hist(x)
hist(y)

#Frequency
table(x)
table(y)

#How interval of 5000 and bar number defined in SW, log min 5 max 15, Rule of Thumb

#Summary of x
summary(x)

#mean - sd
#median - IQR (Q3-Q1)

outlierxp <- 15.50+1.5*(22.75-8.25)
outlierxp

outlierxn <- 15.50-1.5*(22.75-8.25)
outlierxn

#Outlier not present in x

#Summary of y
summary(y)
outlieryp <- 3735.5+1.5*(11787.2-566.2)
outlieryp

outlieryn <- 3735.5-1.5*(11787.2-566.2)
outlieryn

boxplot(y)

#Outlier not present in x as per classical box plot
#Plot tukey boxplot in R to get outlier

#If there is outlier then there will be O in boxplot

#create dataframe 
df <- data.frame(x<-c(1:30),y<-c(x^3))
df

#Changing column name of df
colnames(df)<-c('x','y')
df

#Correlation: 
#Person(r) = linear

#sperman(P) = non linear

#Scatter Plot
plot(df$y,df$x) #(y,x)
plot(df$x,df$y) #(x,y)

#Non-linear so use sperman plot
corr <-cor.test(x,y,method='spearman')
corr

df$z <-log(df$y)
plot(df$x, df$z) #linear

cor(df$z,df$x)




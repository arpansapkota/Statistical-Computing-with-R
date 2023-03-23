#Dataframe
df<-data.frame(x<-c(1:30),y<-x^3)
plot(df$x,df$y) #Scatter Plot
View(df)
print(df)

#Change the variable name of df
colnames(df) <-c('x','y')
View(df)

#Calculate correlation
cor(df$x,df$y) #Pearson= 0.92011
#This is a biased estimate as the relationship is not linear!

#Transforming to make linear 
df$z <- log(df$y)
plot(df$x, df$z) #Still non-linear

df$w <- log(df$x)
plot(df$w, df$z) #Now Linear

cor(df$w,df$z)
#Person’s correlation=1=Perfect positive correlation!




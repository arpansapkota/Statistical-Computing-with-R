#1. Create a barplot of cyl variable of mtcars data with and without using factor argument

# load the mtcars dataset
data(mtcars)
head(mtcars)

# create a barplot without using factor argument
barplot(table(mtcars$cyl), 
        main = "Number of Cars by Cylinders", 
        xlab = "Cylinders", 
        ylab = "Number of Cars")

# create a barplot using factor argument
barplot(table(factor(mtcars$cyl, levels = c(4, 6, 8))), 
        main = "Number of Cars by Cylinders", 
        xlab = "Cylinders", 
        ylab = "Number of Cars")

#2. Locate median of mpg variable of mtcars data graphically and compare the median value with in-built function of R

# create a boxplot of mpg variable
boxplot(mtcars$mpg, 
        main = "Boxplot of MPG Variable", 
        ylab = "Miles per Gallon")

# add a horizontal line at the median value
abline(h = median(mtcars$mpg), col = "red", lwd = 2)

# print the median value
cat("Median value of mpg variable (calculated using in-built function):", median(mtcars$mpg))

#As we can see from the output and the plot, the median value of the mpg variable is approximately 19.2. We can confirm this value by using the median function of R, which gives us the same value.

#3. Locate mode of mpg variable of mtcars data graphically and compare the mode value with the in-built/custom function of R

# create a kernel density estimate of the mpg variable
density_mpg <- density(mtcars$mpg)

# plot the kernel density estimate
plot(density_mpg, main = "Kernel Density Estimate of MPG Variable")

# find the mode by locating the peak of the density estimate
mode_mpg <- density_mpg$x[which.max(density_mpg$y)]
abline(v = mode_mpg, col = "red", lwd = 2)

# print the mode value
cat("Mode value of mpg variable:", mode_mpg)

#As we can see from the output and the plot, the estimated mode value of the mpg variable is approximately 17.90. However, this is just an estimate based on the kernel density estimate, and may not be exactly accurate.

# custom function to calculate the mode of a vector
calculate_mode <- function(x) {
  freq_table <- table(x)

    mode <- names(freq_table)[which.max(freq_table)]
  
  return(mode)
}

# calculate the mode of the mpg variable using the custom function
mode_mpg <- calculate_mode(mtcars$mpg)
cat("Mode value of mpg variable (calculated using custom function):", mode_mpg)


#4. Create a scatterplot of mgp (dependent) and wt (independent) variable of mtcars data, add ablines at mean plus-minus 2 standard deviation and show the name of the cars with mpg > 2 standard deviation from mean inside the scatterplot obtained above

# create a scatterplot of mpg (dependent) and wt (independent) variables
plot(mtcars$wt, mtcars$mpg, main = "Scatterplot of mpg vs. wt", xlab = "Weight (in 1000 lbs)", ylab = "Miles per gallon")

# add ablines at mean plus-minus 2 standard deviation
abline(v = mean(mtcars$wt) - 2*sd(mtcars$wt), lty = 2, col = "red")
abline(v = mean(mtcars$wt) + 2*sd(mtcars$wt), lty = 2, col = "red")
abline(h = mean(mtcars$mpg) - 2*sd(mtcars$mpg), lty = 2, col = "blue")
abline(h = mean(mtcars$mpg) + 2*sd(mtcars$mpg), lty = 2, col = "blue")

# identify cars with mpg > 2 standard deviation from mean
high_mpg_cars <- mtcars[mtcars$mpg > mean(mtcars$mpg) + 2*sd(mtcars$mpg),]
high_mpg_cars 
text(high_mpg_cars$wt, high_mpg_cars$mpg, rownames(high_mpg_cars), pos = 4, col = "red")


#5. Create a x variable with 100 random numbers and y variable with x + 100 random numbers; create a factor variable with 100 monthly random observations, create a time series data with 100 random values starting from January 1970; create a date variable with 100 random values starting from 1970/01/01 increasing each day; create a new variable z with square of x variable
# create x and y variables
set.seed(07)
x <- rnorm(100)
y <- x + rnorm(100, mean = 100)

# create factor variable with 100 monthly random observations
set.seed(07)
months <- month.name[1:12]
month_factor <- factor(sample(months, 100, replace = TRUE))

# create time series data with 100 random values starting from January 1970
set.seed(07)
ts_data <- ts(rnorm(100), start = c(1970, 1), frequency = 12)
ts_data

# create date variable with 100 random values starting from 1970/01/01 increasing each day
date <- as.Date("1970-01-01") + 0:99
date

# create a new variable z with square of x variable
z <- x^2


#6. Create a 2 x 3 plot window with scatterplot, barplot, boxplot, time series plot, date plot and square function plot

# create a 2 x 3 plot window
par(mfrow = c(2, 3))

# scatterplot of x vs. y
plot(x, y, main = "Scatterplot of x vs. y")

# barplot of factor variable
barplot(table(month_factor), main = "Barplot of factor variable")

# boxplot of y variable
boxplot(y, main = "Boxplot of y variable")

# time series plot of ts_data
plot(ts_data, main = "Time Series Plot")

# date plot of date vs. y
plot(date, y, main = "Date Plot")

# plot of z vs. x using the square function
plot(x, z, main = "Plot of z vs. x", type = "l")


#7. Perform log transformation on x and z variables i.e. log of x, log of z and log of x and z in the plot command and interpret the results carefully 

# perform log transformation on x and z variables
log_x <- log(x)
log_y <- log(y)
log_z <- log(z)
log_xz <- log(x*z)

# perform log transformation on x and z variables and create plots
par(mfrow = c(2, 3))

# scatterplot of log(x) vs. log(y)
plot(log_x, log_y, main = "Scatterplot of log(x) vs. log(y)")

# barplot of factor variable
barplot(table(month_factor), main = "Barplot of factor variable")

# boxplot of log(y) variable
boxplot(log_y, main = "Boxplot of log(y) variable")

# time series plot of log(ts_data)
plot(log(ts_data), main = "Time Series Plot (log)")

# date plot of date vs. log(y)
plot(date, log_y, main = "Date Plot (log)")

# plot of log(z) vs. log(x) using the square function
plot(log_x, log_z, main = "Plot of log(z) vs. log(x)", type = "l")


#log transformations were performed on the x and z variables, and then the transformed variables were plotted. The log transformation is often used to reduce the variability in the data and to make the relationships between variables more linear. The results show that the log transformations have a significant effect on the shape and distribution of the data. For example, plot of log(z) vs. log(x) shows that the relationship between x and y becomes more linear after log transformation, and the boxplot shows that the log transformation of x and z variables reduces the variability of the data.


#8. Create a 1x1 plot window with correlation matrix plot of first three numerical variable of mtcars data (cyl is not numeric, its factor!)
#Resetting the previous windows size
par(mfrow = c(1, 1))

# Select first three numerical variables
mtcars_subset <- mtcars[, c("mpg", "cyl", "disp")]

# Create correlation matrix
cor_matrix <- cor(mtcars_subset)

# Create correlation matrix plot
#install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         number.cex = 0.7, tl.cex = 0.7, cl.cex = 0.7, diag = FALSE)

#9. Interpret the result of the correlation matrix plot obtained above with respect to the correlation coefficient to be used for each pair
#Looking at the plot, we can see that there is a strong negative correlation between cyl and mpg, with a correlation coefficient of -0.85. This means that as the number of cylinders in a car increases, its fuel efficiency (measured in miles per gallon, or mpg) tends to decrease.
#There is also a moderate negative correlation between disp and mpg, with a correlation coefficient of -0.85. This suggests that as the engine displacement of a car increases, its fuel efficiency tends to decrease.
#There is a positive correlation between cyl and disp, with a correlation coefficient of 0.9. This means that as the number of cylinders (cyl) in an engine increases, the displacement (disp) of the engine also tends to increase, and vice versa. 
#The correlation matrix plot provides a useful summary of the relationships between the first three numerical variables in the mtcars dataset, and suggests that the strongest correlation exists between cyl and mpg.

#10. Write advantage and limitations of plots created using R base packages based on these nine steps
#R base plots provide a useful starting point for generating simple visualizations quickly and easily, but may not be the best choice for complex or highly customized plots. While they are widely available and can be useful for communication, they may not be optimized for performance and may not offer the same level of customization as more advanced plotting libraries.

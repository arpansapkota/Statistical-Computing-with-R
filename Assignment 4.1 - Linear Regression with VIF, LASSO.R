#Linear Regression with VIF/LASSO in R

#Use the "mtcars" data and do as follows in R Studio 
#1. Fit multiple linear regression with mpg as dependent variable and rest of the variables in the mtcars data as independent variables and save it as mlr object 
mlr <- lm(mpg ~ ., data = mtcars)

#2. Get the summary of mlr and interpret the result carefully
summary(mlr)

#3. Get the VIF of mlr model and drop variables with VIF > 10 one-by-one until none of the predictors have VIF > 10
library(car)

# Calculate VIF for the mlr model and Check if any VIF value is greater than 10
vif(mlr)

# Drop the variable with the highest VIF from the mlr model
#Removing “disp” variable:

mlr1 <- lm(mpg ~ cyl+hp+drat+wt+qsec+vs+am+gear+carb, data = mtcars)
vif(mlr1)

# Recalculate VIF for the updated mlr model

#Removing “cyl” variable:

mlr2 <- lm(mpg ~hp+drat+wt+qsec+vs+am+gear+carb,data = mtcars)
vif(mlr2)

#4. Fit the mlr model with predictors having VIF <=10, get the summary of mlr and interpret the result carefully
# Fit mlr model with predictors having VIF <= 10
mlr <- lm(mpg ~ hp+drat+wt+qsec+vs+am+gear+carb, data = mtcars)

# Get the summary of the updated mlr model
summary(mlr)

#5. Fit lasso regression with mpg as dependent variable and rest of the variables in the mtcars data as independent variables as cv_model object using cv.glmnet model included in the glmnet package
# Fit LASSO regression using cv.glmnet
library(glmnet)

# Prepare the data
x <- as.matrix(mtcars[, -1])  # Independent variables
y <- mtcars$mpg              # Dependent variable

# Fit LASSO regression with cross-validation
cv_model <- cv.glmnet(x, y, alpha = 1)

# Print the cv_model object
cv_model

#6. Get the best lambda value from the lasso regression fitted above, plot the cv_model and interpret them carefully
# Get the best lambda value
best_lambda <- cv_model$lambda.min

# Plot the cv_model
plot(cv_model)

# Interpretation

#7. Fit the best lasso regression model as best_model using the best_lambda value obtained above
# Fit the best LASSO regression model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

# Print the best_model object
best_model

#8. Get the coefficients of the best_model and identify the important variables with s0 non-missing values
# Extract the coefficients from the best_model
coefficients <- coef(best_model, s = best_lambda)

# Identify the important variables with non-missing values
important_variables <- rownames(coefficients)[coefficients[, 1] != 0]

# Print the coefficients and important variables
coefficients
important_variables

#9. Fit the multiple linear regression model using the independent variables obtained from the best_model above
# Get the independent variables from the best_model
independent_vars <- rownames(coefficients)[coefficients[, 1] != 0]
independent_vars

# Fit the multiple linear regression model
mlr_best <- lm(mpg ~ cyl+hp+wt+am+carb, data = mtcars)
summary(mlr_best)

#10. Compare the statistically significant variables obtained from step 4 and step 9
#Step 4 Model:
  
#* The significant variables at the 0.05 level are: wt.

#* The significant variables at the 0.1 level are: None.

#* The significant variables at the 0.5 level are: hp, carb.

#Step 9 Model:
  
#* The significant variables at the 0.05 level are: wt.

#* The significant variables at the 0.1 level are: None.

#* The significant variables at the 0.5 level are: None.

#It is important to note that the significance levels may vary depending on the chosen threshold (e.g., 0.05, 0.1, 0.5). In both models, the R-squared values are relatively high, indicating a good fit to the data.

#Comparing the significant variables between the two models provides insights into the impact of variable selection using VIF dropouts (step 4) and LASSO regression (step 9) on the inclusion and significance of predictors. The selection of the most appropriate approach depends on the specific requirements of the analysis, such as interpretability, model complexity, and predictive performance.

## 11. Write a summary for handling multicollinearity with VIF dropouts and LASSO regression
#Both VIF dropouts and LASSO regression are effective methods for addressing multicollinearity in regression analysis. VIF dropouts manually remove highly correlated predictors based on VIF values, while LASSO regression automatically selects important predictors by shrinking less relevant coefficients to zero. Choosing the most appropriate approach depends on the specific requirements of the analysis and the goals of the model.
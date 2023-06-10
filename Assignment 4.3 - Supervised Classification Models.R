#Use the attached "titanic.csv" data and do as follows in R Studio with R script:
  
#1. Read the titanic.csv data with base R function and save it as "data" and
# remove the name column and save again as data
setwd("/Users/arpan/Desktop/MDS/01 MDS I-I/MDS 503 - Statistical Computing with R/Lab/Data")
data <- read.csv("Arpan Sapkota - titanic.csv")
data <- data[, -3]  # Remove the name column
head(data)
str(data)

#Converting factor
table(data$Pclass)
data$Pclass<-as.factor(data$Pclass)
str(data$Pclass)

data$Sex <- as.factor(data$Sex)
str(data$Sex)

data$Survived<-as.factor(data$Survived)
data$Siblings.Spouses.Aboard<-as.factor(data$Siblings.Spouses.Aboard)
data$Parents.Children.Aboard<-as.factor(data$Parents.Children.Aboard)




#2. Fit binary logistic regression model with "Survived" variable as dependent 
#variable and rest of variables as independent variables using "data", 
# get summary of the model, check VIF and interpret the results carefully
model <- glm(Survived ~ ., data = data, family = binomial)
summary(model)
library(car)
vif(model)

# The VIF values indicate that there is no severe multicollinearity among the 
# predictor variables in the logistic regression model. The variables Pclass, Sex, 
# Age, Siblings.Spouses.Aboard, Parents.Children.Aboard, and Fare have VIF values 
# ranging from 1.167931 to 2.115788 These values suggest that there is little to 
# moderate correlation between the predictor variables, indicating that they can 
# be included in model without significant issues related to multicollinearity.

#3. Randomly split the data into 70% and 30% with replacement of samples as 
#"train" and "test" data
set.seed(07)
ind <- sample(2, nrow(data),replace = T, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

#4. Fit binary logistic regression classifier, knn classifier, ann classifier, 
# naive bayes classifier, svm classifier, decision tree classifier, decision 
# tree bagging classifier, random forest classifier, tuned random forest 
# classifier and random forest boosting classifier models using the "train" data
library(class)
library(nnet)
library(e1071)
library(rpart)
library(randomForest)
library(caret)

# Fit binary logistic regression classifier
logit_model <- glm(Survived ~ ., data = train, family = binomial)
summary(logit_model)


library(class)
# Separate predictor variables (train_x) and the class variable (train_y)
train_x <- train[, -1]
train_y <- train$Survived

sum(is.na(train_x))
str(train_x)

# Convert character variables to numeric
train_x$Age <- as.numeric(train_x$Age)
train_x$Fare <- as.numeric(train_x$Fare)
train_x$Pclass<-as.numeric(train_x$Pclass)
train_x$Sex<-as.numeric(train_x$Sex)
train_x$Siblings.Spouses.Aboard<-as.numeric(train_x$Siblings.Spouses.Aboard)
train_x$Parents.Children.Aboard<-as.numeric(train_x$Parents.Children.Aboard)


# Fit k-NN classifier
knn_model <- knn(train_x, train_x, train_y, k = 3)
summary(knn_model)

# Artificial Neural Network (ANN) classifier
ann_model <- nnet(Survived ~ ., data = train, size = 5)
summary(ann_model)

# Naive Bayes classifier
nb_model <- naiveBayes(Survived ~ ., data = train)
summary(nb_model)

# Support Vector Machine (SVM) classifier
svm_model <- svm(Survived ~ ., data = train)
summary(svm_model)

# Decision Tree classifier
tree_model <- rpart(Survived ~ ., data = train)
summary(tree_model)

# Decision Tree Bagging classifier
bagging_model <- randomForest(Survived ~ ., data = train, mtry = 3, ntree = 10)
summary(bagging_model)

# Random Forest classifier
rf_model <- randomForest(Survived ~ ., data = train, mtry = 3)
summary(rf_model)

# Tuned Random Forest classifier
tuned_rf_model <- randomForest(Survived ~ ., data = train, mtry = 3, nodesize = 5)
summary(tuned_rf_model)

# Random Forest Boosting classifier
boosting_model <- randomForest(Survived ~ ., data = train, mtry = 3, ntree = 10, method = "adaboost")
summary(boosting_model)


#5. Get confusion matrix and accuracy/misclassification error for all the 
# classifier models and interpret them carefully
library(caret)

# Function to calculate confusion matrix and accuracy
calculate_metrics <- function(model, test_data) {
  # Get predicted class labels
  preds <- predict(model, newdata = test_data[-1])
  
  # Ensure predicted and reference labels have the same levels
  preds <- factor(preds, levels = levels(test_data$Survived))
  
  # Create confusion matrix
  cm <- confusionMatrix(preds, test_data$Survived)
  
  # Extract accuracy from confusion matrix
  accuracy <- cm$overall['Accuracy']
  
  # Return confusion matrix and accuracy
  return(list(confusion_matrix = cm$table, accuracy = accuracy))
}


# Logistic Regression
logistic_results <- calculate_metrics(logit_model, test)
logistic_results

# K-NN
#knn_results <- calculate_metrics(knn_model, test)

# ANN
ann_results <- calculate_metrics(ann_model, test)
ann_results

# Naive Bayes
naive_bayes_results <- calculate_metrics(nb_model, test)
naive_bayes_results

# SVM
svm_results <- calculate_metrics(svm_model, test)
svm_results

# Decision Tree
#dt_results <- calculate_metrics(tree_model, test)

# Decision Tree Bagging
bagging_results <- calculate_metrics(bagging_model, test)
bagging_results

# Random Forest
rf_results <- calculate_metrics(rf_model, test)
rf_results

# Tuned Random Forest
tuned_rf_results <- calculate_metrics(tuned_rf_model, test)
tuned_rf_results

# Random Forest Boosting
boosting_results <- calculate_metrics(boosting_model, test)
boosting_results

#Based on the accuracy values, the Tuned Random Forest model achieved the 
# highest accuracy of approximately 0.8309353. However, comparing accuracy alone 
# may not provide a comprehensive evaluation of the models. 


#6. Get confusion matrix and accuracy/misclassification error for all the predicted models and interpret them carefully
#7. Compare accuracy and misclassification error of predicted models based on "test" data to decide the "best" model
#8. Write a reflection on your own word focusing on "what did I learn from this assignment?"

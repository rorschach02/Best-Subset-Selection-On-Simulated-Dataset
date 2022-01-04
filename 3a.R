### Clear the environment 
rm(list = ls())


### First we will set the directory of the R script 
setwd("C:/Users/anike/Desktop/Sem 1/EAS 506 Statistical Data Mining/Homework/Homework 3")


## Loading all the libraries 
library(ISLR)
library(corrplot)
library(MASS)
library(klaR)
library(leaps)
library(lattice)
library(ggplot2)
library(corrplot)
library(car)
library(caret)
library(class)


## Simulated Dataset 
set.seed(1)
X <- rnorm(1000 * 20)
X <- matrix(X,1000,20)
colnames(X) <- paste("X", 1:20 , sep = "")
X[1:5,1:5]

set.seed(1)
beta <- runif(20)
beta[c(4,7,12,16)] = 0
beta

set.seed(1)
epsilon <- 0.001 * rnorm(1000)
epsilon

Y <- X%*%beta + epsilon
length(Y)

full_dataset <- data.frame(X,Y)
dim(X)
head(full_dataset,2)

#density plot 
x11()
par(mfrow=c(4,5)) 
densityPlot(full_dataset$X1)
densityPlot(full_dataset$X2)
densityPlot(full_dataset$X3)
densityPlot(full_dataset$X4)
densityPlot(full_dataset$X5)
densityPlot(full_dataset$X6)
densityPlot(full_dataset$X7)
densityPlot(full_dataset$X8)
densityPlot(full_dataset$X9)
densityPlot(full_dataset$X10)
densityPlot(full_dataset$X11)
densityPlot(full_dataset$X12)
densityPlot(full_dataset$X13)
densityPlot(full_dataset$X14)
densityPlot(full_dataset$X15)
densityPlot(full_dataset$X16)
densityPlot(full_dataset$X17)
densityPlot(full_dataset$X18)
densityPlot(full_dataset$X19)
densityPlot(full_dataset$X20)


## Splitting dataset into train and test data ##
train_index = sample(1:nrow(full_dataset) , nrow(full_dataset)*.1)
train_data <- full_dataset[train_index, ]
test_data <- full_dataset[-train_index, ]
dim(test_data)
dim(train_data)
y.test = test_data$Y
y.train = train_data$Y

## Performing best subset selection ## 
dataset_best_subset_selection <- regsubsets(Y ~ . , data = train_data , nbest = 1,  really.big = TRUE , nvmax = 21) 
dataset_best_subset_selection_sum <- summary(dataset_best_subset_selection)

dataset_best_subset_selection_sum

## i took beta zero for 4,7,12,16. so best subset of 16 should be without those betas. 
# it is without those beta 
coef(dataset_best_subset_selection , 16)

names(dataset_best_subset_selection_sum)


## Plotting RSS, adjusted R2, Cp, and BIC for all of the models at once will help us decide which model to select.

## 1. RSS 
plot(dataset_best_subset_selection_sum$rss ,xlab=" Number of Variables ",ylab=" RSS", type="l")
which.min(dataset_best_subset_selection_sum$rss) ## 20 
points(20, dataset_best_subset_selection_sum$rss[20],col="red" , cex = 2 , pch = 20)

## 2. Adjusted R Sqaure

plot(dataset_best_subset_selection_sum$adjr2 ,xlab=" Number of Variables ",ylab=" Adjusted RSq", type="l")
which.max(dataset_best_subset_selection_sum$adjr2) ## 18 
points(18, dataset_best_subset_selection_sum$adjr2[18],col="red" , cex = 2 , pch = 20)

## 3. Cp 

plot(dataset_best_subset_selection_sum$cp ,xlab=" Number of Variables ",ylab=" Cp", type="l")
which.min(dataset_best_subset_selection_sum$cp) ## 17
points(17, dataset_best_subset_selection_sum$cp[17],col="red" , cex = 2 , pch = 20)

## 4.BIC
plot(dataset_best_subset_selection_sum$bic ,xlab=" Number of Variables ",ylab=" BIC", type="l")
which.min(dataset_best_subset_selection_sum$bic) ## 16 
points(16, dataset_best_subset_selection_sum$bic[16],col="red" , cex = 2 , pch = 20)


## Training MSE and Testing MSE 

predict.regsubsets = function(object,newdata,id , ...){
  form = as.formula((object$call[[2]]))
  mat = model.matrix(form, newdata)
  coefi = coef(object , id = id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
  
}

training_error_value <- matrix(rep(NA,20))  
testing_error_value <- matrix(rep(NA,20))

y_true_train = train_data$Y
y_true_test = test_data$Y

for (i in 1:20){
  training_pred = predict(dataset_best_subset_selection , newdata = train_data , id = i )
  testing_pred = predict(dataset_best_subset_selection , newdata = test_data , id = i )
  
  ## 
  training_error_value[i] =  (1/length(y_true_train)) * sum ((y_true_train - training_pred ) ^ 2) #MSE training error
  testing_error_value[i] = (1/length(y_true_test)) * sum ((y_true_test - testing_pred ) ^ 2) # MSE testing error 
}
training_error_value
testing_error_value

x11()
plot(training_error_value, col= "blue" , type = "b" , ylab = "Training Error")
which.min(training_error_value)
points(20, training_error_value[20],col="red" , cex = 2 , pch = 20)

x11()
plot(testing_error_value , col= "red" , type = "b", ylab = "Testing Error")
which.min(testing_error_value)
points(16, testing_error_value[20],col="red" , cex = 2 , pch = 20)


# Which model size does the test set MSE takes it's minimum value 
min(testing_error_value)
testing_error_value 
## 16 model minimum 


## here also 16 
coef(dataset_best_subset_selection, which.min(testing_error_value))
## so the best model caught all zeroed out coefficients.

##

val.errors <- rep(NA, 20)
x_cols = colnames(X, do.NULL = FALSE, prefix = "X")
for (i in 1:20) {
  coefi <- coef(dataset_best_subset_selection, id = i)
  val.errors[i] <- sqrt(sum((beta[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2) + sum(beta[!(x_cols %in% names(coefi))])^2)
}

val.errors

x11()
plot(val.errors, xlab = "Number of coefficients", ylab = "Error between estimated and true coefficients", pch = 19, type = "b")

which.min(val.errors)

points(18, testing_error_value[18],col="red" , cex = 2 , pch = 20)
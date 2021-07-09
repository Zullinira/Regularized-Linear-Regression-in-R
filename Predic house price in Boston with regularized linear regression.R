# Description: Assigment Day 16 - Regularized Linear Regression in R
# Author: Zullinira Dwi Utami
# Date: 2021-07-08


#upload dataset
data <- read.csv('boston.csv')
data

#split data
library(caTools)
set.seed(123)

#split train
sample <- sample.split(data$medv, SplitRatio = .80)
pre_train <- subset(data, sample == TRUE)
sample_train <- sample.split(pre_train$medv, SplitRatio = .80)

#split train-validation
train <- subset(pre_train, sample_train == TRUE)
validation <- subset(pre_train, sample_train == FALSE)

#split test data
test <- subset(data,sample == FALSE)

#cek korelasi
library(psych)
pairs.panels(train, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
) 


#drop kolom agar tidak multilinearitas
drop_cols <- c('rad','indus','rm','nox')

#data baru tanpa yang di drop
library(dplyr)
train <- train %>% select(-drop_cols)
validation <-  validation %>% select(-drop_cols)
test <- test %>% select(-drop_cols)

#hendle categorical
x <- model.matrix(medv ~ ., train)[,-1]
y <- train$medv

#Fit models on training data
#ridge regression
#lambdas = [0.01, 0.1, 1, 10]
ridge_reg_pointzeroone <- glmnet(x, y, alpha=0, lambda = 0.01)
coef(ridge_reg_pointzeroone)

ridge_reg_pointone <- glmnet(x, y, alpha = 0, lambda = 0.1)
coef(ridge_reg_pointone)

ridge_reg_one <- glmnet(x, y, alpha = 0, lambda = 1)
coef(ridge_reg_pointone)

ridge_reg_ten <- glmnet(x, y, alpha = 0, lambda = 10)
coef(ridge_reg_ten)

#Lasso regression
#lambdas = [0.01, 0.1, 1, 10]
lasso_reg_pointzeroone <- glmnet(x, y, alpha = 1, lambda = 0.01)
coef(lasso_reg_pointzeroone) 

lasso_reg_pointone <- glmnet(x, y, alpha = 1, lambda = 0.1)
coef(lasso_reg_pointone) 

lasso_reg_one <- glmnet(x, y, alpha = 1, lambda = 1)
coef(lasso_reg_pointone)

lasso_reg_ten <- glmnet(x, y, alpha = 1, lambda = 10)
coef(lasso_reg_ten)


# choose the best lamda,compare with validation data
x_validation <- model.matrix(medv ~., validation)[,-1]
y_validation <- validation$medv


#RMSE ridge
RMSE_ridge_pointzeroone <- sqrt(mean((y_validation - predict(ridge_reg_pointzeroone, x_validation))^2))
RMSE_ridge_pointzeroone

RMSE_ridge_pointone <- sqrt(mean((y_validation - predict(ridge_reg_pointone, x_validation))^2))
RMSE_ridge_pointone

RMSE_ridge_one <- sqrt(mean((y_validation - predict(ridge_reg_one, x_validation))^2))
RMSE_ridge_one 

RMSE_ridge_ten <- sqrt(mean((y_validation - predict(ridge_reg_ten, x_validation))^2))
RMSE_ridge_ten 

#RMSE Lasso
RMSE_lasso_pointzeroone <- sqrt(mean((y_validation - predict(lasso_reg_pointzeroone, x_validation))^2))
RMSE_lasso_pointzeroone

RMSE_lasso_pointone <- sqrt(mean((y_validation - predict(lasso_reg_pointone, x_validation))^2))
RMSE_lasso_pointone

RMSE_lasso_one <- sqrt(mean((y_validation - predict(lasso_reg_one, x_validation))^2))
RMSE_lasso_one 

RMSE_lasso_ten <- sqrt(mean((y_validation - predict(lasso_reg_ten, x_validation))^2))
RMSE_lasso_ten 


#Interpretasi koefisien the best model 
#didapatkan model yang lebih baik pada RMSE_ridge_pointone dan RMSE_lasso_pointone
#Hal tersebut dikarenakan nilai RMSE lebih kecil daripda lamda dengan nilai lain

#Evaluate best model
#Ridge regression
x_test <- model.matrix(medv ~., test)[,-1]
y_test <- test$medv

#RMSE
RMSE_ridge_best <- sqrt(mean((y_test - predict(ridge_reg_pointone, x_test))^2))
RMSE_ridge_best

# MAE
MAE_ridge_best <- mean(abs(y_test-predict(ridge_reg_pointone, x_test)))
MAE_ridge_best

# MAPE
MAPE_ridge_best <- mean(abs((predict(ridge_reg_pointone, x_test) - y_test))/y_test) 
MAPE_ridge_best


#Lasso Regression
#RMSE
RMSE_lasso_best <- sqrt(mean((y_test - predict(lasso_reg_pointone, x_test))^2))
RMSE_lasso_best

# MAE
MAE_lasso_best <- mean(abs(y_test-predict(lasso_reg_pointone, x_test)))
MAE_lasso_best

# MAPE
MAPE_lasso_best <- mean(abs((predict(lasso_reg_pointone, x_test) - y_test))/y_test) 
MAPE_lasso_best

#interpretasi
#didapatkan nilai RMSE yang cukup besar (meskipun pada best model), hal tersebut bisa disebabkan karena tidak dilakukannya EDA
#Nilai RMSE yg cukup tinggi bisa diakibatkan outlier pada data yang tidak di treatment terlebih dahulu

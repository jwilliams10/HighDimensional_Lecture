rm(list = ls())

data(mtcars)
str(mtcars)

if(!("caret" %in% installed.packages())){
  install.packages("caret")
}
if(!("GA" %in% installed.packages())){
  install.packages("GA")
}
if(!("memoise" %in% installed.packages())){
  install.packages("memoise")
}

library(caret)
library(GA)
library(memoise)

set.seed(1330)

######################## Model Selection with the AIC

Base_Model <- lm(mpg ~ 1,data = mtcars)
Full_Model <- lm(mpg ~ .,data = mtcars)

### Forward Selection with AIC
Forward_Selection_AIC <- step(Base_Model,scope = list(lower = Base_Model,upper = Full_Model),direction = "forward",k = 2,trace = 0)

Forward_Selection_AIC

### Backward Selection with AIC
Backward_Selection_AIC <- step(Full_Model,scope = list(lower = Base_Model,upper = Full_Model),direction = "backward",k = 2,trace = 0)

Backward_Selection_AIC

### Stepwise Selection with AIC
Stepwise_Selection_AIC <- step(Base_Model,scope = list(lower = Base_Model,upper = Full_Model),direction = "both",k = 2,trace = 0)

Stepwise_Selection_AIC

### Exhaustive Search with AIC
X <- subset(mtcars, select = -mpg)
Y <- mtcars[,"mpg"]
All_Models <- as.matrix(expand.grid(rep(list(0:1), ncol(X))))
AIC_Evaluation <- function(x){
  X_try <- X[,x == 1]
  model_data <- data.frame(Y = Y,X = X_try)
  return(AIC(lm(Y~.,data = model_data)))
}
All_AICs <- apply(All_Models,1,AIC_Evaluation)
model_data <- mtcars[,c("mpg",colnames(X)[which(All_Models[which.min(All_AICs),] == 1)])]
Exhaustive_Search_AIC <- lm(mpg ~ .,data = model_data)

Exhaustive_Search_AIC

### Genetic Algorithm with AIC
suggestedsol <- rbind(0,diag(ncol(X)))
AIC_Evaluation <- function(x){
  X_try <- X[,x == 1]
  model_data <- data.frame(Y = Y,X = X_try)
  return((-1)*AIC(lm(Y~.,data = model_data)))
}
AIC_Evaluation <- memoise::memoise(AIC_Evaluation)
ans <- GA::ga("binary", fitness = AIC_Evaluation, nBits = ncol(X),maxiter = 400,run = 40,suggestions = suggestedsol,monitor = FALSE)
memoise::forget(AIC_Evaluation)
Best_Solution <- ans@solution[1,]
model_data <- mtcars[,c("mpg",colnames(X)[Best_Solution == 1])]
Exhaustive_Search_AIC <- lm(mpg ~ .,data = model_data)

Exhaustive_Search_AIC

######################## Cross-Validation

## Leave one out cross validation
train.control <- trainControl(method = "LOOCV")
LOOCV_Model <- train(mpg ~ ., data = mtcars, method = "lm",trControl = train.control)

LOOCV_Model

## k-fold cross validation (5)
train.control <- trainControl(method = "cv", number = 5)
KFold_Model <- train(mpg ~ ., data = mtcars, method = "lm",trControl = train.control)

KFold_Model

## k-fold cross validation by hand
folds <- createFolds(mtcars$mpg, k = 5)
rmse <- vector()
for(i in 1:5){
  train <- mtcars[-folds[[i]],]
  test <- mtcars[folds[[i]],]
  model <- lm(mpg ~ ., data = train)
  prediction <- predict(model,test)
  rmse[i] <- sqrt(mean((prediction - test$mpg)^2))
}

######################## BIC

### Exhaustive Search with BIC
BIC_Evaluation <- function(x){
  X_try <- X[,x == 1]
  model_data <- data.frame(Y = Y,X = X_try)
  return(BIC(lm(Y~.,data = model_data)))
}
All_BICs <- apply(All_Models,1,BIC_Evaluation)
model_data <- mtcars[,c("mpg",colnames(X)[which(All_Models[which.min(All_BICs),] == 1)])]
Exhaustive_Search_BIC <- lm(mpg ~ .,data = model_data)

Exhaustive_Search_BIC

######################## RMSE

### Exhaustive Search with RMSE on 5-fold cross-validation
# Slow, be careful when running
CrossValidation_Evaluation <- function(x){
  X_try <- X[,x == 1]
  model_data <- data.frame(Y = Y,X = X_try)
  for(i in 1:5){
    train <- model_data[-folds[[i]],,drop = FALSE]
    test <- model_data[folds[[i]],,drop = FALSE]
    model <- lm(Y ~ ., data = train)
    prediction <- predict(model,test)
    rmse[i] <- sqrt(mean((prediction - test$Y)^2))
  }
  return(mean(rmse))
}
All_RMSEs <- apply(All_Models,1,CrossValidation_Evaluation)
model_data <- mtcars[,c("mpg",colnames(X)[which(All_Models[which.min(All_RMSEs),] == 1)])]
Exhaustive_Search_CrossValidation <- lm(mpg ~ .,data = model_data)

Exhaustive_Search_CrossValidation

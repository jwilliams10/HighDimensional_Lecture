######################### LASSO/Ridge 

## LASSO
LASSO_Model <- glmnet(x = as.matrix(X),y = Y,alpha = 1,family = "gaussian")
## Returns beta's for a grid of lambda values
dim(LASSO_Model$beta)
## cv.glmnet identifies best lambda based on MSE in the gaussian case
cvLASSO_Model <- cv.glmnet(x = as.matrix(X),y = Y,alpha = 1,family = "gaussian")
## best lambda
cvLASSO_Model$lambda.min
## highest lambda within 1 se of the best lambda
cvLASSO_Model$lambda.1se
## Specify Lambda
LASSO_Model <- glmnet(x = as.matrix(X),y = Y,alpha = 1,family = "gaussian",lambda = cvLASSO_Model$lambda.1se)
dim(LASSO_Model$beta)

## LASSO does not produce the same Beta's as linear regression
model_data <- mtcars[,c("mpg",colnames(X)[which(LASSO_Model$beta != 0)])]
lm_lasso_model <- lm(mpg~.,data = model_data)
summary(predict(LASSO_Model,as.matrix(X)) - predict(lm_lasso_model,mtcars))

## Ridge
cvRidge_Model <- cv.glmnet(x = as.matrix(X),y = Y,alpha = 0,family = "gaussian")
Ridge_Model <- glmnet(x = as.matrix(X),y = Y,alpha = 0,family = "gaussian",lambda = cvRidge_Model$lambda.1se)

## Elasticnet
Elasticnet_gridsearch <- NULL
for(a in seq(0.1, 0.9, 0.05)){
  tmp_model <- cv.glmnet(as.matrix(X),y = Y, family = "gaussian", alpha = a)
  Elasticnet_gridsearch <- rbind(Elasticnet_gridsearch,data.frame(cvm = tmp_model$cvm[tmp_model$lambda == tmp_model$lambda.1se], lambda.1se = tmp_model$lambda.1se, alpha = a))
}
Elasticnet_gridsearch <- Elasticnet_gridsearch[Elasticnet_gridsearch$cvm == min(Elasticnet_gridsearch$cvm), ]
Elasticnet_Model <- glmnet(as.matrix(X),y = Y, family = "gaussian", lambda = Elasticnet_gridsearch$lambda.1se, alpha = Elasticnet_gridsearch$alpha)

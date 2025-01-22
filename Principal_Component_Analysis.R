rm(list = ls())

## Adapted from https://www.r-bloggers.com/2021/05/principal-component-analysis-pca-in-r/

# Using the iris data as an example
data("iris")
str(iris)

# Split the data into training and testing
set.seed(111)
ind <- sample(2, nrow(iris),replace = TRUE,prob = c(0.8, 0.2))
training <- iris[ind == 1,]
testing <- iris[ind == 2,]

# PCA does not require a response variable
X <- subset(training,select = -Species)
pca_data <- prcomp(X,center = TRUE,scale. = TRUE)
pca_formula <- prcomp(~.,data = X,center = TRUE,scale. = TRUE)

# Second Row is Proportion of Variance Explained
summary(pca_data)
barplot(summary(pca_data)$importance[2,])

# Project onto training to get PCs for training
training_pcs <- predict(pca_data, training)
# Project onto testing to get PCs for training
testing_pcs <- predict(pca_data, testing)

# Train a random forest to predict species
if(!("randomForest" %in% installed.packages())){
  install.packages("randomForest")
}
library(randomForest)
rf_model <- randomForest(y = training$Species,x = training_pcs)

# Predict onto training and evaluate
training_prediction <- predict(rf_model,training_pcs)
table(training_prediction,training$Species)

# Predict onto testing and evaluate
testing_prediction <- predict(rf_model,testing_pcs)
table(testing_prediction,testing$Species)



# PCA does cannot work with categorical data in it's standard from
X <- subset(training,select = -Petal.Width)
# pca_data <- prcomp(X,center = TRUE,scale. = TRUE) # returns error

# Use model.matrix to change categorical variables into indicators
X <- model.matrix(~. - 1,data = X)
pca_data <- prcomp(X,center = TRUE,scale. = TRUE)
summary(pca_data)

# Project onto the model matrix object for training to get PCs for training
training_pcs <- predict(pca_data, X)
training_pcs <- data.frame(training_pcs,Petal.Width = training$Petal.Width)

# See how the a linear regression model performs
summary(lm(Petal.Width~.,data = training_pcs))

# If we collapse to the first 4 PC's we see little decrease in R-squared, this is because the majority of variation is explained by the first 4 PCs 
summary(lm(Petal.Width~PC1 + PC2 + PC3 + PC4,data = training_pcs))



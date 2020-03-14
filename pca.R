
library(ggplot2)
library(rpart)
library(randomForest)
library(e1071)
data <- read.csv("openml_phpJNxH0q.csv")
X <- data %>% select(-Class) %>% sapply(as.numeric)

pca <- prcomp(X)

rotated <- X %*% pca$rotation

ggplot(cbind(as.data.frame(rotated), Class=data$Class), aes(x = PC1, y = PC2)) +
  geom_point(aes(colour=Class))

# Regresja Liniowa

linear_model <- glm(Class ~ ., data=data, family = "binomial")
summary(linear_model)

plot(sort(abs(predict(linear_model, type = "response") - as.numeric(data$Class)+1)), type="l")

linear_acc <- 1 - mean(abs(predict(linear_model, type = "response") - as.numeric(data$Class)+1))


# Rpart

rpart_model <- rpart(Class ~ ., data=data)

rpart_acc <- 1 - mean(abs(predict(rpart_model)[,"benign"] + as.numeric(data$Class) - 2))


# Random Forest

rf_model <- randomForest(Class ~ ., data=data)
summary(rf_model)

rf_acc <- mean(predict(rf_model) == data$Class)

data$Class %>% as.numeric %>% mean - 1 # Dość dobrze zrównoważone klasy

# SVM

s <- svm(x = X,
         y = data$Class,
         cost=1)

acc_svm <- mean(predict(s) == data$Class)


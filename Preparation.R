
library(dplyr)
library(readr)

# Loading data

data <- read_csv("kidney.csv")

data
pairs(data[,c(1,2,3,4,7)])

# One hot

data %>%
  mutate(disease_type_1 = as.numeric(as.numeric(as.factor(data$disease_type)) == 1),
         disease_type_2 = as.numeric(as.numeric(as.factor(data$disease_type)) == 2),
         disease_type_3 = as.numeric(as.numeric(as.factor(data$disease_type)) == 3),
         disease_type_4 = as.numeric(as.numeric(as.factor(data$disease_type)) == 4)) %>%
  select(-disease_type) -> data

data$sex <- as.numeric(as.factor(data$sex)) - 1

data <- data %>% arrange(patient)

d0 <- data[1:nrow(data) %% 2  == 0,] %>% select(-patient)
d1 <- data[1:nrow(data) %% 2  == 1,] %>% select(time, status, age)
 
# d0 == d1

data <- cbind(d1, d0)
names(data)[1:3] <- paste(names(data)[1:3], "pre", sep="_")

data %>% summary
data %>% nrow
data %>% ncol

data

# SVM

svm_model <- svm()


# Model

m <- lm(frailty ~ ., data=data)
summary(m)


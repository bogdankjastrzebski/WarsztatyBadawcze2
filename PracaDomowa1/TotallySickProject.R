library(dplyr)
library(ggplot2)
library(e1071)
library(mlr)

sick <- OpenML::getOMLDataSet(data.name = "sick")$data

sick %>% apply(2, table)

sick %>% select(-TBG, -TBG_measured, -FTI_measured, -T4U_measured, -TT4_measured, -T3_measured, -TSH_measured) %>% na.exclude() %>% apply(2, unique)

sick_tidy <- sick %>% select(-TBG, -TBG_measured, -FTI_measured, -T4U_measured, -TT4_measured, -T3_measured, -TSH_measured) %>% na.exclude()

sick_tidy

# Ważne w tych danych jest to, że te kolumny wskazują, czy pomiar obok został wykonany

nrow(sick)
nrow(sick_tidy) # Nie jest aż tak źle

# Random Forest

library(randomForest)

sick_tidy %>% apply(2, typeof)
sick_tidy %>% mutate(age = as.numeric(age))


rf <- randomForest(as.factor(Class) ~., data=sick_tidy)
rf # Bardzo ładny model

1- mean(predict(rf) != sick_tidy$Class)

# GLM

g <- glm(Class ~., data=sick_tidy, family="binomial")
summary(g)
1 - mean(abs(round(predict(g, type="response")) - (as.numeric(sick_tidy$Class)-1)))


table(paste(round(predict(g, type="response")), (as.numeric(sick_tidy$Class)-1)))


# Plots
sick_t <- sick_tidy

pairs(sick_tidy[, 17:21], color=sick_tidy$Class)

# 17
hist(log(sick_tidy[, 17]))
hist(sick_tidy[, 17])




i <- 17
skewness(sick_tidy[, i])
skewness(sqrt(sick_tidy[, i])) 
skewness(log(sick_tidy[, i])) #


sick_t[,17] <- log(sick_tidy[,17])


# 18
hist(sick_tidy[, 18])
hist(sqrt(sick_tidy[, 18]))

shapiro.test(sqrt(sick_tidy[, 18])) #
shapiro.test(log(sick_tidy[, 18]))

i <- 18
skewness(sick_tidy[, i])
skewness(sqrt(sick_tidy[, i])) # 
skewness(log(sick_tidy[, i]))

sick_t[,i] <- sqrt(sick_tidy[,i])

# 19
hist(sick_tidy[, 19])
hist(sqrt(sick_tidy[, 19]))
hist(log(sick_tidy[, 19]))

shapiro.test(sick_tidy[, 19])
shapiro.test(sqrt(sick_tidy[, 19])) #
shapiro.test(log(sick_tidy[, 19]))

i <- 19
skewness(sick_tidy[, i])
skewness(sqrt(sick_tidy[, i])) # 
skewness(log(sick_tidy[, i]))

sick_t[,i] <- sqrt(sick_tidy[,i])
# 20
shapiro.test(sick_tidy[, 20])
shapiro.test(sqrt(sick_tidy[, 20]))
shapiro.test(log(sick_tidy[, 20]))

i <- 20
skewness(sick_tidy[, i])
skewness(sqrt(sick_tidy[, i]))
skewness(log(sick_tidy[, i])) #

hist(sick_tidy[, 20])
hist(sqrt(sick_tidy[, 20]))
hist(log(sick_tidy[, 20])) # 

sick_t[,i] <- log(sick_tidy[,i])

# 21

hist(sick_tidy[, 21])
skewness(sick_tidy[,21])
skewness(sqrt(sick_tidy[,21]))
skewness(log(sick_tidy[,21]))

sick_t[,21] <- sqrt(sick_tidy[,21])

pairs(sick_t[, 17:21])
pairs(sick_tidy[, 17:21])
# Ostatnia klasy

# GLM robi to automatycznie
unique(sick_tidy$referral_source)
summary(g)
names(sick_tidy)

# Nowy model

g2 <- glm(Class ~., data=sick_t, family="binomial")
summary(g2)
1 - mean(abs(round(predict(g2, type="response")) - (as.numeric(sick_t$Class)-1)))

table(paste(round(predict(g2, type="response")), (as.numeric(sick_t$Class)-1)))

# Drzewo

library(rpart)
library(rpart.plot)

?rpart

rp <- rpart(Class ~., data=sick_t)
summary(rp)
rpart.plot(rp)

1 - mean(abs(round(predict(rp)[,2]) - (sick_t$Class == "sick")))
s<- predict(rp)
s[,2]



# PLSRGLM


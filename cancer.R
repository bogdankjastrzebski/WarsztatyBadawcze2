library(mlr)
#https://www.openml.org/d/13
library(readr)
library(dplyr)
library(e1071)

df <- read_csv("dataset_13_breast-cancer.csv")

names(df) <- sub("-", "_", names(df))

# Usunięcie obserwacji z brakami - 9 obs
df1 <- df[!apply(df == '?', 1, any), ]

# BREAST_QUAD
df2 <- df1 %>% mutate(breast_quad_horiz = as.numeric(breast_quad == "'left_low'" | breast_quad == "'left_up'") - as.numeric(breast_quad == "'right_low'" | breast_quad == "'right_up'"),
               breast_quad_vert = as.numeric(breast_quad == "'left_up'" | breast_quad == "'right_up'") - as.numeric(breast_quad == "'left_down'" | breast_quad == "'right_down'")) %>% 
        select(-breast_quad)

# AGE
df3 <- df2
df3$age <- 10*(as.numeric(as.factor(df2$age))+1.5)


# MENOPAUSE
df3$menopause %>% unique

df4 <- df3 %>% mutate(meno = as.numeric(menopause != "'premeno'"),
               gltmeno = as.numeric(menopause == "'lt40'")) %>% select(-menopause)

# TUMOR_SIZE

df5 <- df4

s <- substr(as.character(df4$tumor_size), 2,3)
df5$tumor_size <- as.numeric(ifelse(substr(s, 2,2) == '-', substr(s, 1,1), s))+2.5

# INV_NODES


df6 <- df5

s <- substr(df5$inv_nodes, 2,3)
df6$inv_nodes <- as.numeric(ifelse(substr(s, 2,2) == '-', substr(s, 1,1), s))+1

# DEG_MALIG

df7 <- df6
df6$deg_malig <- as.numeric(substr(df6$deg_malig, 2,2))

# REST

df6$node_caps %>% unique
df6$breast %>% unique
df6$irradiat %>% unique
df6$Class %>% unique

df7 <- df6 %>% mutate(node_caps = as.numeric(node_caps == "'yes'"),
               breast = as.numeric(breast == "'left'"),
               irradiat = as.numeric(irradiat == "'yes'"),
               Class = as.numeric(Class == "'recurrence-events'"))

###################################################

i <- sample(1:nrow(df7), 30)

train <- df7[-i,]
valid <- df7[i,]

# Prosta regresja liniowa

basic_linear <- glm(Class ~ ., data=train, family = "binomial")
predict(basic_linear, type="response")

acc_lm <- 1-mean(abs(round(predict(basic_linear, valid, type="response")) - valid$Class))

# Random Forest
train$Class <- as.factor(train$Class) 
valid$Class <- as.factor(valid$Class)

task<-makeClassifTask(data = train, target = "Class")
rf_learner<-makeLearner("classif.randomForest", predict.type = "prob")

rf_model <- train(rf_learner, task)
prediction <- predict(fr_model, newdata = valid)

acc_rf <- 1 - mean(abs(as.numeric(prediction$data$response) - as.numeric(prediction$data$truth)))

# svm
s <- svm(x = train %>% select(-Class),
         y = train$Class,
         cost=1)

acc_svm <- 1 - mean(abs(as.numeric(as.character(predict(s, valid %>% select(-Class)))) - as.numeric(as.character(valid$Class))))



# Olafa

df<-mutate_if(df,is.character,as.factor)
names(df)<-make.names((names(df)))

task<-mlr::makeClassifTask(data = df,target = "Class")
learner<-makeLearner("classif.randomForest",predict.type = "prob")

#testy Acc, AUC, Specificity, Recall, Precision, F1 regresja:MSE, RMSE, MAE, R2 | "f1", "acc" "auc" "tnr" "tpr" "ppv"| "mse" "mae" "rmse" "rsq" 
Rcuda<-list(f1=f1, acc=acc, auc=auc, tnr=tnr, tpr=tpr ,ppv=ppv)
measures<-intersect(listMeasures(task),c("f1", "acc", "auc", "tnr", "tpr" ,"ppv"))

cv <- makeResampleDesc("CV", iters = 5)
r <- resample(learner, task, cv, measures = Rcuda[measures])


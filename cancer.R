library(mlr)
#https://www.openml.org/d/13
library(readr)
library(dplyr)
df <- read_csv("dataset_13_breast-cancer.csv")

df<-mutate_if(df,is.character,as.factor)
names(df)<-make.names((names(df)))

task<-mlr::makeClassifTask(data = df,target = "Class")
learner<-makeLearner("classif.randomForest",predict.type = "prob")

#testy Acc, AUC, Specificity, Recall, Precision, F1 regresja:MSE, RMSE, MAE, R2 | "f1", "acc" "auc" "tnr" "tpr" "ppv"| "mse" "mae" "rmse" "rsq" 
Rcuda<-list(f1=f1, acc=acc, auc=auc, tnr=tnr, tpr=tpr ,ppv=ppv)
measures<-intersect(listMeasures(task),c("f1", "acc", "auc", "tnr", "tpr" ,"ppv"))

cv <- makeResampleDesc("CV", iters = 5)
r <- resample(learner, task, cv,measures = Rcuda[measures])

library(mlr)
#https://www.openml.org/d/13
library(readr)
library(dplyr)
library(rpart.plot)

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

# Prosta regresja liniowa

basic_linear <- glm(Class ~ ., data=df7, family = "binomial")
summary(basic_linear)

abs(round(basic_linear$fitted.values) - df7$Class) %>% sort %>% plot

acc <- 1-mean(abs(round(basic_linear$fitted.values) - df7$Class))


# Random Forest
df8 <- df7
df8$Class <- as.factor(df8$Class) 


task<-makeClassifTask(data = df8, target = "Class")
learner<-makeLearner("classif.randomForest", predict.type = "prob")

model <- train(learner, task)
prediction <- predict(model, task)

acc_rf <-1-mean(abs(as.numeric(prediction$data$response) - as.numeric(prediction$data$truth)))

#losowe dane stworzone na podstawie datasetu i predykcji lasu losowego

#wszystkie permutacje
temp<-sapply(1:12, function(x){length(unique(df8[[x]]))})

temp<-sapply(1:12, function(x){unique(df8[[x]])})

temp<-expand.grid(temp)

names(temp)<-names(df8)

#losowe
random<-sapply(1:12,FUN = function(x){df8[sample(1:277,100000,replace = TRUE),x]})

random<-data.frame(random)

task_random<-makeClassifTask(data = random, target = "Class")

prediction <- predict(model, task_random)

response<-prediction$data$response

random$Class<-response
#random<-rbind(random,df8)

#pewnosc klasy
#certainty<-((prediction$data$prob.0-0.5)^2)*4
certainty<-abs(prediction$data$prob.0-0.5)*2

#certainty<-c(certainty,rep(1,277))

task_random<-makeClassifTask(data = random, target = "Class",weights = certainty)

#permutacje
task_all<-makeClassifTask(data = temp, target = "Class")

prediction <- predict(model, task_all)

response<-prediction$data$response

temp$Class<-response

#pewnosc klasy
#certainty<-((prediction$data$prob.0-0.5)^2)*4
certainty<-abs(prediction$data$prob.0-0.5)*2

hist(certainty)

task_all<-makeClassifTask(data = temp, target = "Class",weights = certainty)

sure<-certainty>0.3

task_sure<-makeClassifTask(data = temp[sure,], target = "Class",weights = certainty[sure])

#
learner<-makeLearner("classif.rpart", predict.type = "prob")

#0.7942238
model_rpart_random <- train(learner, task_random)
prediction <- predict(model_rpart_random, task)
acc_rpart_random <-1-mean(abs(as.numeric(prediction$data$response) - as.numeric(prediction$data$truth)))

#0.7870036
model_rpart_all <- train(learner, task_all)
prediction <- predict(model_rpart_all, task)
acc_rpart_all <-1-mean(abs(as.numeric(prediction$data$response) - as.numeric(prediction$data$truth)))

#0.7870036
model_rpart_sure <- train(learner, task_sure)
prediction <- predict(model_rpart_sure, task)
acc_rpart_sure <-1-mean(abs(as.numeric(prediction$data$response) - as.numeric(prediction$data$truth)))

dt_param <- makeParamSet( 
  makeDiscreteParam("minsplit", values=seq(5,10,1)), makeDiscreteParam("minbucket", values=seq(round(5/3,0), round(10/3,0), 1)), 
  makeNumericParam("cp", lower = 0.01, upper = 0.05), makeDiscreteParam("maxcompete", values=6), makeDiscreteParam("usesurrogate", values=0),
  makeDiscreteParam("maxdepth", values=10) )

#nie ma roznicy
ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 3L, stratify=TRUE)
(dt_tuneparam <- tuneParams(learner=learner, 
                            resampling=rdesc, 
                            measures=list(mlr::acc, setAggregation(tpr, test.sd)), 
                            par.set=dt_param, 
                            control=ctrl, 
                            task=task_sure, 
                            show.info = TRUE) )

dtree <- setHyperPars(learner, par.vals = dt_tuneparam$x)
dtree_train <- train(learner=dtree, task=task_sure) 
prediction <- predict(dtree_train, task)

acc_rpart_sure_tune <-1-mean(abs(as.numeric(prediction$data$response) - as.numeric(prediction$data$truth)))

#0.805
model_rpart <- train(learner, task)
prediction <- predict(model_rpart, task)
acc_rpart <-1-mean(abs(as.numeric(prediction$data$response) - as.numeric(prediction$data$truth)))

rpart.plot(model_rpart$learner.model)

#rpart_random nie działa lepiej niz rpart

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






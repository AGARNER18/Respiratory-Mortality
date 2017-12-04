install.packages(c("caret", "mlbench","DT"))
install.packages("doParallel")
install.packages("e1071")
install.packages("LogicReg")
install.packages("randomForest")
install.packages("caretEnsemble")
install.packages("xgboost")
library(ipred)
library(xgboost)
library(caretEnsemble)
library(LogicReg)
library(randomForest)
library(doParallel)
library(caret)
library(mlbench)
library(DT)
library(plyr)
library(e1071)

train<-read.csv("training.csv", header=TRUE)
# take sample of 20% for pilot models
train=train[sample(nrow(train),replace=F,size=0.60*nrow(train)),]
train$X<-NULL
names(train)<-tolower(names(train))
train$admissiontype<-train$admission_type
train$admission_type<-NULL
train$admissionlocation<-train$admission_location
train$admission_location<-NULL
train$maritalstatus<-train$marital_status
train$marital_status<-NULL
train$orgname<-train$org_name
train$org_name<-NULL
train$urineoutput<-train$urine_output
train$urine_output<-NULL
train$heartratemin<-train$heartrate_min
train$heartrate_min<-NULL
train$heartratemax<-train$heartrate_max
train$heartrate_max<-NULL
train$sysbpmin<-train$sysbp_min
train$sysbp_min<-NULL
train$sysbpmax<-train$sysbp_max
train$sysbp_max<-NULL
train$diasbpmin<-train$diasbp_min
train$diasbp_min<-NULL
train$diasbpmax<-train$diasbp_max
train$diasbp_max<-NULL
train$respratemax<-train$resprate_max
train$resprate_max<-NULL
train$tempcmax<-train$tempc_max
train$tempc_max<-NULL
train$spo2min<-train$spo2_min
train$spo2_min<-NULL
train$spo2max<-train$spo2_max
train$spo2_max<-NULL
train$glucosemin<-train$glucose_min
train$glucose_min<-NULL
train$glucosemax<-train$glucose_max
train$glucose_max<-NULL
train$ventduration<-train$vent_duration
train$vent_duration<-NULL
train$lodspulm<-train$lods_pulm
train$lods_pulm<-NULL
train$target<-as.factor(train$target)
train$target <- revalue(train$target, c("0"="survived", "1"="died"))

test<-read.csv("test.csv")
test$X<-NULL
names(test)<-tolower(names(test))
test$admissiontype<-test$admission_type
test$admission_type<-NULL
test$admissionlocation<-test$admission_location
test$admission_location<-NULL
test$maritalstatus<-test$marital_status
test$marital_status<-NULL
test$orgname<-test$org_name
test$org_name<-NULL
test$urineoutput<-test$urine_output
test$urine_output<-NULL
test$heartratemin<-test$heartrate_min
test$heartrate_min<-NULL
test$heartratemax<-test$heartrate_max
test$heartrate_max<-NULL
test$sysbpmin<-test$sysbp_min
test$sysbp_min<-NULL
test$sysbpmax<-test$sysbp_max
test$sysbp_max<-NULL
test$diasbpmin<-test$diasbp_min
test$diasbp_min<-NULL
test$diasbpmax<-test$diasbp_max
test$diasbp_max<-NULL
test$respratemax<-test$resprate_max
test$resprate_max<-NULL
test$tempcmax<-test$tempc_max
test$tempc_max<-NULL
test$spo2min<-test$spo2_min
test$spo2_min<-NULL
test$spo2max<-test$spo2_max
test$spo2_max<-NULL
test$glucosemin<-test$glucose_min
test$glucose_min<-NULL
test$glucosemax<-test$glucose_max
test$glucose_max<-NULL
test$ventduration<-test$vent_duration
test$vent_duration<-NULL
test$lodspulm<-test$lods_pulm
test$lods_pulm<-NULL
test$target<-as.factor(test$target)
test$target <- revalue(test$target, c("1"="survived", "2"="died"))

names(train2)<-tolower(names(train2))
train2$admissiontype<-train2$admission_type
train2$admission_type<-NULL
train2$admissionlocation<-train2$admission_location
train2$admission_location<-NULL
train2$maritalstatus<-train2$marital_status
train2$marital_status<-NULL
train2$orgname<-train2$org_name
train2$org_name<-NULL
train2$urineoutput<-train2$urine_output
train2$urine_output<-NULL
train2$heartratemin<-train2$heartrate_min
train2$heartrate_min<-NULL
train2$heartratemax<-train2$heartrate_max
train2$heartrate_max<-NULL
train2$sysbpmin<-train2$sysbp_min
train2$sysbp_min<-NULL
train2$sysbpmax<-train2$sysbp_max
train2$sysbp_max<-NULL
train2$diasbpmin<-train2$diasbp_min
train2$diasbp_min<-NULL
train2$diasbpmax<-train2$diasbp_max
train2$diasbp_max<-NULL
train2$respratemax<-train2$resprate_max
train2$resprate_max<-NULL
train2$tempcmax<-train2$tempc_max
train2$tempc_max<-NULL
train2$spo2min<-train2$spo2_min
train2$spo2_min<-NULL
train2$spo2max<-train2$spo2_max
train2$spo2_max<-NULL
train2$glucosemin<-train2$glucose_min
train2$glucose_min<-NULL
train2$glucosemax<-train2$glucose_max
train2$glucose_max<-NULL
train2$ventduration<-train2$vent_duration
train2$vent_duration<-NULL
train2$lodspulm<-train2$lods_pulm
train2$lods_pulm<-NULL

# find all the classification algorithms in caret
m <- unique(modelLookup()[modelLookup()$forClass,c(1)])
# algorithms that are known to give errors or take extra long to train
removeModels <- c("AdaBag", "AdaBoost.M1", "bag","FH.GBML", "pda2", "PenalizedLDA",
                  "GFS.GCCL", "rbf", "RFlda", "nodeHarvest", "ORFsvm", "dwdLinear", "dwdPoly", "gam",
                  "gaussprLinear", "ownn", "sddaLDA", "sddaQDA", "SLAVE", "smda", "snn", "rmda", 
                  "rFerns", "wsrf","ordinalNet","awnb", "awtan","manb","nbDiscrete","nbSearch","tan",
                  "tanSearch","bartMachine","randomGLM", "Rborist", "adaboost")
# remove the difficult algorithms
m <- m[!m %in% removeModels]
# see algorithms that remain to choose from 
m


# 10 fold cross validation method for models
fitControl <- trainControl(method= "cv", number = 5)

# set up parallel front end
cl <- makeCluster(38); registerDoParallel(cl)

# bagging
bag <- bagging(target ~ ., data=train, coob=TRUE, nbagg=10)
predictions <- predict(bag)
bag_train_conf<-table(predictions, train$target)
predictions<-predict(bag, test)
bag_test_conf<-table(predictions, test$target)


# xgboost trees
xgbGrid <- expand.grid(nrounds = c(1, 10),
                       max_depth = c(1, 4),
                       eta = c(.1, .4),
                       gamma = 0,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = c(.8, 1))
cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
set.seed(110)
xgboost <- train(target~.,data=train,method = "xgbTree", 
                 trControl = cctrl1,
                 metric = "ROC", 
                 preProc = c("center", "scale"),
                 tuneGrid = xgbGrid)
predictions<-predict(xgboost)
xgboost_train_conf<-confusionMatrix(predictions, train[,22])
predictions<-predict(xgboost, test[,-22])
xgboost_test_conf<-confusionMatrix(predictions, test[,22])

# neural network 
fitControl3 <- trainControl(method= "cv", number = 5, classProbs = TRUE, verboseIter = TRUE, preProcOptions = list(thresh=0.75, ICAcomp=3, k=5))
set.seed(115)
nnet<-train(target~.,data=train,method="nnet",preProcess=c('center','scale'),trControl=fitControl3, tuneGrid=expand.grid(size=c(10),decay=c(0.1)))
predictions<-predict(nnet)
nnet_train_conf<-confusionMatrix(predictions, train[,22])
predictions<-predict(nnet, test[,-22])
nnet_test_conf<-confusionMatrix(predictions, test[,22])

# random forest
set.seed(113)
rf<-randomForest(target~.,data=train, ntree=100, corr.bias=TRUE)
predictions <- predict(rf)
rf_train_conf<-table(predictions, train$target)
predictions<-predict(bag, test)
rf_test_conf<-table(predictions, test$target)


# k-nearest neighbor
fitControl4<-trainControl(method= "repeatedcv", number = 5, repeats=2)
set.seed(116)
knn<-train(target~.,data=train,method="knn",preProcess=c('center','scale'),trControl=fitControl4, tuneLength=10)
predictions<-predict(knn)
knn_train_conf<-confusionMatrix(predictions, train[,22])
predictions<-predict(knn, test[,-22])
knn_test_conf<-confusionMatrix(predictions, test[,22])

#ranger
model <- "ranger"

## In case the package or one of its dependencies uses random numbers
## on startup so we'll pre-load the required libraries: 

for(i in getModelInfo(model)[[1]]$library)
  do.call("requireNamespace", list(package = i))
seeds<-vector(mode = "list", length = nrow(train)+1)
seeds<-lapply(seeds, function(x) 1:20)
fitControl5<-trainControl(method="cv", number = 3, returnResamp = "all", classProbs = TRUE, seeds=seeds)
set.seed(118)
ranger<-train(target~.,data=train, method="ranger", trControl=fitControl5, 
              preProc=c("center", "scale"), num.trees=30, seed=345, num.threads=1, importance="permutation")
predictions<-predict(ranger)
ranger_train_conf<-confusionMatrix(predictions, train[,22])
predictions<-predict(ranger, test[,-22])
ranger_test_conf<-confusionMatrix(predictions, test[,22])


# support vector machine: polynomial
set.seed(20)
svmpol <- svm(target~., data = train, kernel = "polynomial", scale = TRUE)
predictions <- predict(svmpol)
svmpol_train_conf<-table(predictions, train$target)
svmpol_train_conf
predictions<-predict(svmpol, test)
svmpol_test_conf<-table(predictions, test$target)
svmpol_test_conf

rf_train_conf
rf_test_conf
ranger_train_conf
ranger_test_conf
nnet_train_conf
nnet_test_conf
svmpol_train_conf
svmpol_test_conf
xgboost_train_conf
xgboost_test_conf
knn_train_conf
knn_test_conf

knn_prob<-predict(knn, type = "prob")
names(knn_prob)<-c("survived"="knn_sur", "died"="knn_die")
rf_prob<-predict(rf, type = "prob")
rf_prob<-as.data.frame(rf_prob)
names(rf_prob)<-c("survived"="rf_sur", "died"="rf_die")
ranger_prob<-predict(ranger, type = "prob")
names(ranger_prob)<-c("survived"="ranger_sur", "died"="ranger_die")
nnet_prob<-predict(nnet, type = "prob")
names(nnet_prob)<-c("survived"="nnet_sur", "died"="nnet_die")
bag_prob<-predict(bag, type = "prob")
bag_prob<-as.data.frame(bag_prob)
names(bag_prob)<-c("survived"="bag_sur", "died"="bag_die")
svmrad_prob<-predict(svmrad, type = "prob")
names(svmrad_prob)<-c("survived"="svm_sur", "died"="svm_die")
xgb_prob<-predict(xgboost, type="prob")
names(xgb_prob)<-c("survived"="xgb_sur", "died"="xgb_die")
svmpol_prob<-predict(svmpol, type="prob")
names(svmpol_prob)<-c("survived"="svm_sur", "died"="svm_die")

avg<-cbind(knn_prob, rf_prob, ranger_prob, nnet_prob, bag_prob,xgb_prob, svmpol_prob)
head(avg)
avg$prob_survived<-NA
avg$prob_survived<-((avg$knn_sur + avg$rf_sur + avg$ranger_sur + avg$nnet_sur + avg$bag_sur + avg$xgb_sur)/6)
avg$prob_died<-((avg$knn_die + avg$rf_die + avg$ranger_die + avg$nnet_die + avg$bag_die + avg$xgb_die)/6)
avg<-avg[,c(13:15)]
avg$pred<-ifelse(avg$prob_survived > 0.40, "survived", "died")
head(avg)
table(avg$pred, train$target)


knn_prob_test<-predict(knn, test, type = "prob")
names(knn_prob_test)<-c("survived"="knn_sur", "died"="knn_die")
rf_prob_test<-predict(rf, test, type = "prob")
rf_prob_test<-as.data.frame(rf_prob_test)
names(rf_prob_test)<-c("survived"="rf_sur", "died"="rf_die")
ranger_prob_test<-predict(ranger, test, type = "prob")
names(ranger_prob_test)<-c("survived"="ranger_sur", "died"="ranger_die")
nnet_prob_test<-predict(nnet, test, type = "prob")
names(nnet_prob_test)<-c("survived"="nnet_sur", "died"="nnet_die")
bag_prob_test<-predict(bag, test, type = "prob")
bag_prob_test<-as.data.frame(bag_prob_test)
names(bag_prob_test)<-c("survived"="bag_sur", "died"="bag_die")
svmrad_prob_test<-predict(svmrad, test, type = "prob")
names(svmrad_prob_test)<-c("survived"="svm_sur", "died"="svm_die")
xgb_prob_test<-predict(xgboost, test, type="prob")
names(xgb_prob_test)<-c("survived"="xgb_sur", "died"="xgb_die")

avg_test<-cbind(knn_prob_test, rf_prob_test, ranger_prob_test, nnet_prob_test, bag_prob_test,xgb_prob_test)
head(avg_test)
avg_test$prob_survived<-NA
avg_test$prob_survived<-((avg_test$knn_sur + avg_test$rf_sur + avg_test$ranger_sur + avg_test$nnet_sur + avg_test$bag_sur + avg_test$xgb_sur)/6)
avg_test$prob_died<-((avg_test$knn_die + avg_test$rf_die + avg_test$ranger_die + avg_test$nnet_die + avg_test$bag_die + avg_test$xgb_die)/6)
avg_test<-avg_test[,c("prob_survived", "prob_died")]
avg_test$pred<-ifelse(avg_test$prob_survived > 0.4, "survived", "died")
head(avg_test)
table(avg_test$pred, test$target)


# stop cluster and register sequntial front end
stopCluster(cl); registerDoSEQ();


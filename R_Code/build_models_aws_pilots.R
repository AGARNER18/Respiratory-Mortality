install.packages(c("caret", "mlbench","DT"))
install.packages("doParallel")
install.packages("e1071")
install.packages("LogicReg")
install.packages("randomForest")
install.packages("caretEnsemble")
library(caretEnsemble)
library(LogicReg)
library(randomForest)
library(doParallel)
library(caret)
library(mlbench)
library(DT)
library(plyr)
library(e1071)

pilot<-read.csv("pilot.csv", header=TRUE)
pilot$X<-NULL
names(pilot)<-tolower(names(pilot))
pilot$admissiontype<-pilot$admission_type
pilot$admission_type<-NULL
pilot$admissionlocation<-pilot$admission_location
pilot$admission_location<-NULL
pilot$maritalstatus<-pilot$marital_status
pilot$marital_status<-NULL
pilot$orgname<-pilot$org_name
pilot$org_name<-NULL
pilot$urineoutput<-pilot$urine_output
pilot$urine_output<-NULL
pilot$heartratemin<-pilot$heartrate_min
pilot$heartrate_min<-NULL
pilot$heartratemax<-pilot$heartrate_max
pilot$heartrate_max<-NULL
pilot$sysbpmin<-pilot$sysbp_min
pilot$sysbp_min<-NULL
pilot$sysbpmax<-pilot$sysbp_max
pilot$sysbp_max<-NULL
pilot$diasbpmin<-pilot$diasbp_min
pilot$diasbp_min<-NULL
pilot$diasbpmax<-pilot$diasbp_max
pilot$diasbp_max<-NULL
pilot$respratemax<-pilot$resprate_max
pilot$resprate_max<-NULL
pilot$tempcmax<-pilot$tempc_max
pilot$tempc_max<-NULL
pilot$spo2min<-pilot$spo2_min
pilot$spo2_min<-NULL
pilot$spo2max<-pilot$spo2_max
pilot$spo2_max<-NULL
pilot$glucosemin<-pilot$glucose_min
pilot$glucose_min<-NULL
pilot$glucosemax<-pilot$glucose_max
pilot$glucose_max<-NULL
pilot$ventduration<-pilot$vent_duration
pilot$vent_duration<-NULL
pilot$lodspulm<-pilot$lods_pulm
pilot$lods_pulm<-NULL
pilot$target<-as.factor(pilot$target)
pilot$target <- revalue(pilot$target, c("0"="survived", "1"="died"))

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

names(pilot2)<-tolower(names(pilot2))
pilot2$admissiontype<-pilot2$admission_type
pilot2$admission_type<-NULL
pilot2$admissionlocation<-pilot2$admission_location
pilot2$admission_location<-NULL
pilot2$maritalstatus<-pilot2$marital_status
pilot2$marital_status<-NULL
pilot2$orgname<-pilot2$org_name
pilot2$org_name<-NULL
pilot2$urineoutput<-pilot2$urine_output
pilot2$urine_output<-NULL
pilot2$heartratemin<-pilot2$heartrate_min
pilot2$heartrate_min<-NULL
pilot2$heartratemax<-pilot2$heartrate_max
pilot2$heartrate_max<-NULL
pilot2$sysbpmin<-pilot2$sysbp_min
pilot2$sysbp_min<-NULL
pilot2$sysbpmax<-pilot2$sysbp_max
pilot2$sysbp_max<-NULL
pilot2$diasbpmin<-pilot2$diasbp_min
pilot2$diasbp_min<-NULL
pilot2$diasbpmax<-pilot2$diasbp_max
pilot2$diasbp_max<-NULL
pilot2$respratemax<-pilot2$resprate_max
pilot2$resprate_max<-NULL
pilot2$tempcmax<-pilot2$tempc_max
pilot2$tempc_max<-NULL
pilot2$spo2min<-pilot2$spo2_min
pilot2$spo2_min<-NULL
pilot2$spo2max<-pilot2$spo2_max
pilot2$spo2_max<-NULL
pilot2$glucosemin<-pilot2$glucose_min
pilot2$glucose_min<-NULL
pilot2$glucosemax<-pilot2$glucose_max
pilot2$glucose_max<-NULL
pilot2$ventduration<-pilot2$vent_duration
pilot2$vent_duration<-NULL
pilot2$lodspulm<-pilot2$lods_pulm
pilot2$lods_pulm<-NULL

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
fitControl <- trainControl(method= "repeatedcv", number = 10, repeats = 10)

# set up parallel front end
cl <- makeCluster(40); registerDoParallel(cl)

# bagging
library(ipred)
bag <- bagging(target ~ ., data=pilot, coob=TRUE, nbagg=25)
predictions <- predict(bag)
bag_train_conf<-table(predictions, pilot$target)
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
xgboost <- train(target~.,data=pilot,method = "xgbTree", 
                 trControl = cctrl1,
                 metric = "ROC", 
                 preProc = c("center", "scale"),
                 tuneGrid = xgbGrid)
predictions<-predict(xgboost)
xgboost_train_conf<-confusionMatrix(predictions, pilot[,22])
predictions<-predict(xgboost, test[,-22])
xgboost_test_conf<-confusionMatrix(predictions, test[,22])

# neural network 
fitControl3 <- trainControl(method= "cv", number = 10, classProbs = TRUE, verboseIter = TRUE, preProcOptions = list(thresh=0.75, ICAcomp=3, k=5))
set.seed(115)
nnet<-train(target~.,data=pilot,method="nnet",preProcess=c('center','scale'),trControl=fitControl3, tuneGrid=expand.grid(size=c(10),decay=c(0.1)))
predictions<-predict(nnet)
nnet_train_conf<-confusionMatrix(predictions, pilot[,22])
predictions<-predict(nnet, test[,-22])
nnet_test_conf<-confusionMatrix(predictions, test[,22])

# random forest
set.seed(113)
rf<-randomForest(target~.,data=pilot, ntree=200, corr.bias=TRUE)
predictions <- predict(rf)
rf_train_conf<-table(predictions, pilot$target)
predictions<-predict(bag, test)
rf_test_conf<-table(predictions, test$target)


# k-nearest neighbor
fitControl4<-trainControl(method= "repeatedcv", number = 10, repeats=2)
set.seed(116)
knn<-train(target~.,data=pilot,method="knn",preProcess=c('center','scale'),trControl=fitControl4, tuneLength=10)
predictions<-predict(knn)
knn_train_conf<-confusionMatrix(predictions, pilot[,22])
predictions<-predict(knn, test[,-22])
knn_test_conf<-confusionMatrix(predictions, test[,22])

#ranger
model <- "ranger"

## In case the package or one of its dependencies uses random numbers
## on startup so we'll pre-load the required libraries: 

for(i in getModelInfo(model)[[1]]$library)
  do.call("requireNamespace", list(package = i))
seeds<-vector(mode = "list", length = nrow(pilot)+1)
seeds<-lapply(seeds, function(x) 1:20)
fitControl5<-trainControl(method="cv", number = 3, returnResamp = "all", classProbs = TRUE, seeds=seeds)
set.seed(118)
ranger<-train(target~.,data=pilot, method="ranger", trControl=fitControl5, 
              preProc=c("center", "scale"), num.trees=30, seed=345, num.threads=1, importance="permutation")
predictions<-predict(ranger)
ranger_train_conf<-confusionMatrix(predictions, pilot[,22])
predictions<-predict(ranger, test[,-22])
ranger_test_conf<-confusionMatrix(predictions, test[,22])


# support vector machine: linear
set.seed(10)
svm <- svm(target~., data = pilot, kernel = "linear", scale = TRUE)
predictions <- predict(svm)
svm_train_conf<-table(predictions, pilot$target)
predictions<-predict(svm, test)
svm_test_conf<-table(predictions, test$target)

# support vector machine: polynomial
set.seed(20)
svmpol <- svm(target~., data = pilot, kernel = "polynomial", scale = TRUE)
predictions <- predict(svm)
svmpol_train_conf<-table(predictions, pilot$target)
svmpol_train_conf
predictions<-predict(svm, test)
svmpol_test_conf<-table(predictions, test$target)
svmpol_test_conf

# support vector machine: radial
set.seed(30)
svmrad <- svm(target~., data = pilot, kernel = "radial", scale = TRUE)
predictions <- predict(svmrad)
svmrad_train_conf<-table(predictions, pilot$target)
svmrad_train_conf
predictions<-predict(svmrad, test)
svmrad_test_conf<-table(predictions, test$target)
svmrad_test_conf


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
table(avg$pred, pilot$target)
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


#Model fitting using all the variables on both NASH and ASH patients

#data processing
setwd("/view/zhuru4_view/vob/CVAY785A/CVAY785A2204/csr_1/analysis_data")
load("model_all.RData")

library(caret)
set.seed(998)

train <- data.com
train$Y <- as.factor(train$Y)
levels(train$Y)<- c("notrans", "transfer")

miss <- apply(train,2, function(x) sum(is.na(x)))
miss[miss>0]

levels(train$INIT_INR)

table(train$INIT_ASCITES)
train$INIT_ASCITES[train$INIT_ASCITES==4]=1
table(train$FINAL_ASCITES)
train$FINAL_ASCITES_miss <- train$FINAL_ASCITES==4
train$FINAL_ASCITES[train$FINAL_ASCITES==4]=0

table(train$INIT_ENCEPH)
train$INIT_ENCEPH[train$INIT_ENCEPH==4]=1
table(train$FINAL_ENCEPH)
train$FINAL_ENCEPH_miss <- train$FINAL_ENCEPH==4
train$FINAL_ENCEPH[train$FINAL_ENCEPH==4]=0

train$INIT_CREAT_CLEAR <- (as.numeric(train$GENDER)*0.15+0.7)*((140-train$INIT_AGE)/(train$INIT_SERUM_CREAT))*(train$INIT_WGT_KG/72)
train$FINAL_CREAT_CLEAR <- (as.numeric(train$GENDER)*0.15+0.7)*((140-(train$INIT_AGE+floor(train$DAYSWAIT_CHRON/365)))/(train$FINAL_SERUM_CREAT))*(train$INIT_WGT_KG/72)

  # Sex * ((140 - Age) / (SerumCreat)) * (Weight / 72)
train <- train[,-c(34,43)]

#cross validation
training <- train
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 3,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using
                           ## the following function
                           summaryFunction = twoClassSummary)

set.seed(825)

#Gradient Boosting
gbmGrid <-  expand.grid(interaction.depth = c(2, 3, 4, 5, 6),
                        n.trees = (1:10)*50,
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbmFit3 <- train(Y ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 ## Specify which metric to optimize
                 metric = "ROC",
                 tuneGrid = gbmGrid)
gbmFit3
summary(gbmFit3$finalModel) #better

trellis.par.set(caretTheme())
plot(gbmFit3)

save.image("model_all.RData")

#Support Vector Machine
svmFit <- train(Y ~ ., data = training,
                method = "svmRadial",
                trControl = fitControl,
                preProc = c("center", "scale"),
                tuneLength = 5,
                metric = "ROC")
svmFit
svmFit$finalModel

plot(svmFit, metric = "ROC", scales = list(x = list(log =2)))

save.image("model_all.RData")

#Random Forest
rfgrid <- expand.grid(mtry=seq(5,40,by=5), splitrule=c("gini","extratrees"), min.node.size=1)
rfFit <- train(Y ~ ., data = training,
               method = "ranger",
               trControl = fitControl,
               tuneGrid=rfgrid,
               metric = "ROC",
               num.tree=1000)
rfFit
plot(rfFit)
summary(rfFit$finalModel)

save.image("model_all.RData")

options(max.print=100)
rfFit$finalModel

rfgrid <- expand.grid(mtry=seq(20,30,by=1), splitrule=c("gini"), min.node.size=1)
rfFit1 <- train(Y ~ ., data = training,
                method = "ranger", 
                trControl = fitControl, 
                tuneGrid=rfgrid,
                metric = "ROC",
                num.tree=1000)
rfFit1
plot(rfFit1)

save.image("model_all.RData")

library(ranger)
rfModel <- ranger(Y ~ ., data = training, importance = "impurity", num.tree=400, mtry=24)
sort(rfModel$variable.importance, decreasing = T)

save.image("model_all.RData")

#Lasso
tunegrid <- expand.grid(alpha=0.1*c(1:10), lambda=c(0.004,0.005,0.006))

lassoFit <- train(Y ~ ., data = training,
                  method = "glmnet", 
                  trControl = fitControl, 
                  preProc = c("center", "scale"),
                  tuneGrid=tunegrid,
                  metric = "ROC")
lassoFit
plot(lassoFit)

save.image("model_all.RData")



#comparing different method
resamps <- resamples(list(GBM = gbmFit3,
                          SVM = svmFit,
                          RF=rfFit1,
                          LASSO=lassoFit))
summary(resamps)

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))


save.image("model_all.RData")

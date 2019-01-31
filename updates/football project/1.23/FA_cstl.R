# focus on FA measurement:
# tract: cst_l
# tract: cst_r
#load("//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/fromdropbox/football project/alldata_qdtg.Rdata")
library(dplyr)
library(refund)
load("/Users/mawanying/moon\ Dropbox/Wanying\ Ma/fromdropbox/alldata_qdtg4.Rdata")
load("/Users/mawanying/moon\ Dropbox/Wanying\ Ma/fromdropbox/alldata_qdtg5.Rdata")
#############################################################################
######################### players information table #########################
######################### create group index ################################
######################### delete duplicates #################################
#############################################################################
player.info <- read.csv("/Users/mawanying/moon\ Dropbox/Wanying\ Ma/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9
files <- list.files(path="/Users/mawanying/moon\ Dropbox/Wanying\ Ma/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)

#player.info <- read.csv("C:/Users/wma9/Dropbox/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9
#files <- list.files(path="C:/Users/wma9/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)
subname <- gsub(pattern=".csv", replacement="", files)
player.info <- player.info %>% filter(INJURYSPORTVARSITY=='Football')

group1 <- as.character(unique(player.info$SUBJECTSTUDYNUM)) #73 # there is no empty in group1
group1 <- group1[group1!=''] #73
group2 <- as.character(unique(player.info$CONTACTSPORTQGID)) #60
group2 <- group2[group2!=''] #59
group3 <- as.character(unique(player.info$NONCONTACTSPORTQGID)) #59
group3 <- group3[group3!=''] #58


d1 <- group1 %in% group2 #8 there are 8 football players that occurs in both group 1 and group 2
sum(d1)
d2 <- group1 %in% group3 #0
sum(d2)
d3 <- group2 %in% group1 #8 there are 8 football players that occurs in both group 1 and group 2
sum(d3)
d4 <- group2 %in% group3 #0
sum(d4)
d5 <- group3 %in% group1 #0
sum(d5)
d6 <- group3 %in% group2 #0
sum(d6)

group1[which(d1==TRUE)] #c("UCLA-FB-1098", "UCLA-FB-1102", "UCLA-FB-1670", "UNC-FB-1093",  "WISC-FB-1934", "WISC-FB-1937", "WISC-FB-1938", "WISC-FB-1939")
group2[which(d3==TRUE)] #c("UCLA-FB-1098", "UCLA-FB-1102", "UCLA-FB-1670", "UNC-FB-1093",  "WISC-FB-1934", "WISC-FB-1937", "WISC-FB-1938", "WISC-FB-1939") all are fb, and 4 of them are in 195


group1 <- group1[which(d1==FALSE)] #65
group2 <- group2[which(d3==FALSE)] #51

sum(group1 %in% subname) #39
sum(group2 %in% subname) #34
sum(group3 %in% subname) #34

group1 <- group1[group1 %in% subname]#39
group2 <- group2[group2 %in% subname]#34
group3 <- group3[group3 %in% subname]#34

#############################################################################
############## construct group index for football players ###################
##############      group1: SUBJECTSTUDYNUM                 ############
##############      group2: CONTACTSPORTQGID                ############
##############      group3: NONCONTACTSPORTQGID             ############
#############################################################################


addgroupindex <- function(data){
  alldata.player <- as.character(data$player) #21052x1
  alldata.fb.player <- alldata.player %in% c(group1, group2, group3) #indicator vector: 9496x1
  groupindex <- c()
  for(i in alldata.player[alldata.fb.player]){
    if(i %in% group1){
      groupindex <- c(groupindex, 1)
    }
    if(i %in% group2){
      groupindex <- c(groupindex, 2)
    }
    if(i %in% group3){
      groupindex <- c(groupindex, 3)
    }
  }
  fbdata <- data[alldata.fb.player, ]
  colnames(fbdata)[4:203] <- paste0("den", 1:200)
  data.fb <- fbdata %>% mutate(group=groupindex)
  colnames(data.fb)[4:203] <- paste0("den", 1:200)
  return(data.fb)
}

alldata.fb_qdtg <- addgroupindex(alldata_qdtg)


############################################################
## perform cluster ##
############################################################
PC.pvalue <- c()
  # data: input dataset
  # m: measurement, in {"FA", "MD", "Da", "Dr"}
  # t: tract
fa.tract <- c("cst_l","cst_r", "ar_l", "ar_r", "fmi")
md.tract <- c("cgc_l", "cst_l","cgh_l","ar_l", "cgh_r")
da.tract <- c("cst_r", "ptr_l","cgh_r","slf_l", "ml_l")
dr.tract <- c("cgc_l", "fma","unc_l","slf_r", "str_l")
PC.pvalue <- c()
for(ii in 1:5){
  t <- md.tract[ii]
  m <- "MD"
  data <- alldata.fb_qdtg
  data.tm <- data %>% filter(tract==t, measure==m, group != 3) #86x204
  den.tm <- select(data.tm, den1:den200)    #86x200
  #den.tm <- den.tm - as.vector(rep(1, dim(den.tm)[1])) %*% t(colMeans(den.tm))
  den.fpca <- refund::fpca.face(data.matrix(den.tm), center = TRUE, argvals = (1:200)/200, knots = 7, pve = 0.95, p = 5, lambda = 0)
  den.score <- den.fpca$scores/sqrt(dim(den.tm)[2]) #73x8
  
  mis_i <- c()
  for (i in 1:dim(den.score)[2]){
    mis_j <- c()
    for(j in 1:20){
      den.cluster <- kmeans(den.score[,1:i], centers = 2, iter.max = 100)
      permute <- function(vec, p1, p2){
        for(i in 1:length(vec)){
          if(vec[i]==1){vec[i] <- p1}else{vec[i] <- p2}
        }
        return(vec)
      }
      
      perm1 <- permute(den.cluster$cluster, 1,2)
      perm2 <- permute(den.cluster$cluster, 2,1)
      
      misrate <- min(sum(perm1!=data.tm$group)/length(data.tm$group),
                     sum(perm2!=data.tm$group)/length(data.tm$group))
      mis_j <- c(mis_j, misrate)
    }
    mis_i <- rbind(mis_i, mis_j)
  }
  #mis_i
  cat(paste0("m:", m, ", t:",t, "\n"))
  min_mis <- apply(mis_i, 1, min)
  print(min_mis)
  # mini change from 0.33 to 0.32
  cat("-------------------------------\n")
  PC1_1 <- den.score[data.tm$group==1,1]
  PC1_2 <- den.score[data.tm$group==2,1]
  if(var.test(PC1_1,PC1_2)$p.value<0.05){
    PC.pvalue <- c(PC.pvalue, t.test(PC1_1,PC1_2, var.equal=TRUE, paired=FALSE)$p.value)
  }else{
    PC.pvalue <- c(PC.pvalue, t.test(PC1_1,PC1_2, var.equal=F, paired=FALSE)$p.value)
  }
}
PC.pvalue  









fitControl <- trainControl(method = "cv",
                           number = 5,
                           #repeats = 3,
                           ## Estimate class probabilities
                           classProbs = TRUE
                           ## Evaluate performance using
                           ## the following function
                           #summaryFunction = twoClassSummary
                           )



#Gradient Boosting
library(caret)
training <- data.frame(Y=as.factor(data.tm$group),den.score)
levels(training$Y) <- c("group1","group2")

# Train the model using adaboost
set.seed(825)
model_adaboost = train(Y ~ ., data=training, method='adaboost', tuneLength=2, trControl = fitControl)
model_adaboost

#Train the model using xgboost
# extreme gradient boosting
model_xgbDART = train(Y ~ ., data=training, method='xgbDART', tuneLength=4, trControl = fitControl)
model_xgbDART
plot(model_xgbDART)
a<-predict(model_xgbDART, training)
plot(model_xgbDART, metric = "Accuracy")
confusionMatrix(reference = testData$Purchase, data = predicted, mode='everything', positive='MM')

#Stochastic Gradient Boosting 
gbmGrid <-  expand.grid(interaction.depth = c(2, 3, 4, 5, 6),
                        n.trees = (1:20)*50,
                        shrinkage = 0.1,
                        n.minobsinnode = 1)

model_gbm <- train(Y ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 ## Specify which metric to optimize
                 #metric = "ROC",
                 tuneGrid = gbmGrid)
model_gbm
summary(model_gbm$finalModel) #better

# Train the model using ranger
rfgrid <- expand.grid(mtry=2:(dim(training[,1:3])[2]-1),splitrule=c("gini","extratrees"), min.node.size=1)
model_rf <- train(Y ~ ., data = training[,1:3],
               method = "ranger",
               trControl = fitControl,
               tuneGrid=rfgrid,
               #tunelength=5,
               num.tree=800)
model_rf


# Train the model using svm
model_svmRadial = train(Y ~ ., data=training, method='svmRadial', tuneLength=15, trControl = fitControl)
model_svmRadial
model_svmRadial$finalModel


#Lasso
tunegrid <- expand.grid(alpha=0.1*c(1:10), lambda=c(0.004,0.005,0.006))
model_lasso <- train(Y ~ ., data = training,
                  method = "glmnet", 
                  trControl = fitControl, 
                  preProc = c("center", "scale"),
                  tuneGrid=tunegrid)
model_lasso
plot(model_lasso)


# Compare model performances using resample()
models_compare <- resamples(list(ADABOOST=model_adaboost, RF=model_rf, GB=model_gbm, XGBDART=model_xgbDART, SVM=model_svmRadial, lasso=model_lasso))

# Summary of the models performances
summary(models_compare)

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(models_compare, layout = c(3, 1))



########################################
## quantile function ##
## logistic regression ##
para[72, ]
t <- "ml_l"
m <- "Da"
data <- alldata.fb_q
data.tm <- data %>% filter(tract==t, measure==m, group != 3) #86x204
den.tm <- select(data.tm, den1:den200)    #86x200
#den.tm <- den.tm - as.vector(rep(1, dim(den.tm)[1])) %*% t(colMeans(den.tm))
den.fpca <- refund::fpca.face(data.matrix(den.tm), center = TRUE, argvals = (1:200)/200, knots = 7, pve = 0.95, p = 5, lambda = NULL)
den.score <- den.fpca$scores/sqrt(dim(den.tm)[2]) #73x8
den.efun <- den.fpca$efunctions*sqrt(dim(den.tm)[2])
training <- data.frame(Y=as.factor(data.tm$group),den.score)
mylogit <- glm(Y ~ ., data = training, family = "binomial")
summary(mylogit)
preprob <- mylogit$fitted.values
training$rankP <- ifelse(preprob>0.5, 2, 1)
sum(training$rankP == training$Y)/73
coeff <- mylogit$coefficients
beta_t <- den.efun %*% as.vector(coeff[2:4])
plot(den.quantiles/100, beta_t, type="l",
     xlab="t", ylab=expression(beta(t)),
     main="tract: ml_l, measure: Da") 
abline(h=0, lty=2, lwd=1.5)
  
  
draw_plot2(alldata.fb_q, 49, 171)

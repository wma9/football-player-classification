
### use quantiles to cluster
#load("/Users/mawanying/Dropbox/Luo_Jarek_YC/alldata_q.Rdata")
#load("/Users/mawanying/Dropbox/football project/alldata_qdtg1.Rdata")
#fulldir <- list.files(path="/Users/mawanying/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=TRUE, recursive=FALSE)
#files <- list.files(path="/Users/mawanying/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)
#player.info <- read.csv("/Users/mawanying/Dropbox/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9
load("C:/Users/wma9/Dropbox/Luo_Jarek_YC/summary_data.Rdata")
load("C:/Users/wma9/Dropbox/Luo_Jarek_YC/alldata_q.Rdata")
load("C:/Users/wma9/Dropbox/Luo_Jarek_YC/alldata_qdt.Rdata")
load("C:/Users/wma9/Dropbox/football project/alldata_qdtg.Rdata")
fulldir <- list.files(path="C:/Users/wma9/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=TRUE, recursive=FALSE)
files <- list.files(path="C:/Users/wma9/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)
player.info <- read.csv("C:/Users/wma9/Dropbox/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9, there are 4 types: 88football, 30soccer, 10ice hockey, 8lacrosse

# there are 195 players
subname <- gsub(pattern=".csv", replacement="", files)
# there are 27 tracts 
tractnames = c("ar_l",  "ar_r",  "atr_l", "atr_r", "cgc_l", "cgc_r",
               "cgh_l", "cgh_r", "cst_l", "cst_r", "fma",   
               "fmi", "ifo_l", "ifo_r", "ilf_l", "ilf_r", "mcp",  
               "ml_l",  "ml_r",  "ptr_l", "ptr_r", "slf_l", "slf_r", 
               "str_l" ,"str_r", "unc_l", "unc_r")
# 200 quantiles we want to use
den.quantiles = seq(0.25, 99.75, length=200)

library(dplyr)
library(data.table)

basic.info <- player.info %>% select(SUBJECTSTUDYNUM, INJURYSPORTVARSITY, CONTACTSPORTQGID, NONCONTACTSPORTQGID) #basic.info is 136x4
basic.info <- basic.info %>% filter(SUBJECTSTUDYNUM!='', CONTACTSPORTQGID!='', NONCONTACTSPORTQGID !='') #delete lines which has empty values: 109x4
basic.info <- unique(basic.info) #delete duplite lines: 94x4
football.info <- basic.info %>% filter(INJURYSPORTVARSITY=='Football') #football.info is 55x4
#first in second
d1 <- football.info$SUBJECTSTUDYNUM %in% football.info$CONTACTSPORTQGID
#first in third
d2 <- football.info$SUBJECTSTUDYNUM %in% football.info$NONCONTACTSPORTQGID
#second in first
d3 <- football.info$CONTACTSPORTQGID %in% football.info$SUBJECTSTUDYNUM
#second in third
d4 <- football.info$CONTACTSPORTQGID %in% football.info$NONCONTACTSPORTQGID
#third in first
d5 <- football.info$NONCONTACTSPORTQGID %in% football.info$SUBJECTSTUDYNUM
#third in second
d6 <- football.info$NONCONTACTSPORTQGID %in% football.info$CONTACTSPORTQGID
dupli <- cbind(d1, d2, d3, d4, d5, d6) #88x6
football.noDup <- football.info[rowSums(dupli)<1, ] # football.noDup is 47x4

#############################################################################
############## construct group index for football players ###################
##############      group1: SUBJECTSTUDYNUM                 #################
##############      group2: CONTACTSPORTQGID                #################
##############      group3: NONCONTACTSPORTQGID             #################
#############################################################################
group1 <- unique(as.character(football.noDup$SUBJECTSTUDYNUM))     #SUBJECTSTUDYNUM 47x1
group2 <- unique(as.character(football.noDup$CONTACTSPORTQGID))    #CONTACTSPORTQGID 47x1
group3 <- unique(as.character(football.noDup$NONCONTACTSPORTQGID)) #NONCONTACTSPORTQGID 47x1

# after merge, alldata_fb will have 28 players in group 1, 31players in group 2, 29 players in group 3
#############################################################################
alldata.player <- as.character(alldata_q$player) #21052x1
alldata.fb.player <- alldata.player %in% c(group1, group2, group3) #indicator vector: 9496x1
# after merge, alldata_fb will have 28 players in group 1, 31players in group 2, 29 players in group 3
#############################################################################
### add group index restriced to football players to alldata
addgroupindex <- function(alldata, groups){
  alldata.player <- as.character(alldata$player) #21052x1
  alldata.fb.player.ind <- alldata.player %in% groups #indicator vector: 9280x1
  alldata.fb.player <- alldata.player[alldata.fb.player.ind]
  groupindex <- c()
  for(i in alldata.fb.player){
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
  fbdata <- alldata[alldata.fb.player.ind, ]
  data.fb <- fbdata %>% mutate(group=groupindex)
  colnames(data.fb)[grep("ij", colnames(data.fb))] <- paste0("den", 1:200)
  
  return(data.fb)
}
#alldata.fb <- addgroupindex(alldata, alldata.fb.player, groupindex) #9496x204
#alldata.fb_qdt <- addgroupindex(alldata_qdt, alldata.fb.player, groupindex) #9496x204
alldata.fb_q <- addgroupindex(alldata_q,c(group1, group2, group3)) #9496x204
alldata.fb_qdtg <- addgroupindex(alldata_qdtg,c(group1, group2, group3)) #9496x204
alldata.fb_qdt <- addgroupindex(alldata_qdt,c(group1, group2, group3)) #9496x204
#table(alldata.fb$group)
#1    2    3 
#3016 3348 3132 
#length(unique(alldata.fb_q$player[alldata.fb_q$group==1])) #28
#length(unique(alldata.fb_q$player[alldata.fb_q$group==2])) #31
#length(unique(alldata.fb_q$player[alldata.fb_q$group==3])) #29
# there are 88 different players in total for alldata.fb
#alldata_fb will have 28 players in group 1, 31 players in group 2, 29 players in group 3



### 
para <- expand.grid(tractnames, c("FA","MD","Da","Dr"))
# permute function
permute <- function(vec, p1, p2, p3){
  for(i in 1:length(vec)){
    if(vec[i]==1){
      vec[i] <- p1
    }else{
      if(vec[i]==2){vec[i] <- p2}else{
        vec[i] <- p3
      }
    }
  }
  return(vec)  
}

threecluster <- function(alldata, iter){
  # alldata is the dataset merged with football information: alldata.fb_q
  # iter: 1~108
  m <- para[iter, 2]
  t <- para[iter, 1]
  alldata_mt <- alldata  %>% filter(measure==m, tract==t) %>% select(group, den1:den200)
  group_mt <- alldata_mt$group
  alldata_mt <- select(alldata_mt, -group)

  calcu_misrate <- lapply(1:200, function(i){
    x <- alldata_mt[,i]
    den.cluster <- kmeans(x, centers = 3)
    perm1 <- permute(den.cluster$cluster, 1,2,3)
    perm2 <- permute(den.cluster$cluster, 1,3,2)
    perm3 <- permute(den.cluster$cluster, 2,1,3)
    perm4 <- permute(den.cluster$cluster, 2,3,1)
    perm5 <- permute(den.cluster$cluster, 3,1,2)
    perm6 <- permute(den.cluster$cluster, 3,2,1)
    perm <- cbind(perm1, perm2, perm3, perm4, perm5, perm6)
    misrate <- apply(perm, 2, function(x){
      sum(x!=group_mt)/length(group_mt)
    })
    min_perm <- order(misrate)[1]
    min_group <- perm[,min_perm]
    return(list(misrate=misrate[min_perm], group=min_group))
  })
  return(calcu_misrate)
}

twocluster <- function(alldata, iter){
  # alldata is the dataset merged with football information: alldata.fb_q
  # iter: 1~108
  m <- para[iter, 2]
  t <- para[iter, 1]
  alldata_mt <- alldata  %>% filter(measure==m, tract==t, group!=3) %>% select(group, den1:den200)
  group_mt <- alldata_mt$group
  alldata_mt <- select(alldata_mt, -group)
  
  calcu_misrate <- lapply(1:200, function(i){
    x <- alldata_mt[,i]
    den.cluster <- kmeans(x, centers = 2)
    perm1 <- den.cluster$cluster
    perm2 <- permute(den.cluster$cluster, 2, 1, NA)
    perm <- cbind(perm1, perm2)
    misrate <- apply(perm, 2, function(x){
      sum(x!=group_mt)/length(group_mt)
    })
    min_perm <- order(misrate)[1]
    min_group <- perm[,min_perm]
    return(list(misrate=misrate[min_perm], group=min_group))
  })
  return(calcu_misrate)
}

result_3cl <- lapply(1:108, function(x){threecluster(alldata.fb_q, x)})
result_2cl <- lapply(1:108, function(x){twocluster(alldata.fb_q, x)})




#find which measure and which tract give the smallest cluster error
#3 clusters
misrate3 <- sapply(result_3cl, function(x){
  sapply(x, function(i){i$misrate})
})
misrate2 <- sapply(result_2cl, function(x){
  sapply(x, function(i){i$misrate})
})

mt_select3 <- which(apply(misrate3, 2, min)==min(misrate3))
q_select3 <- lapply(mt_select3, function(x){
 data_mt <- misrate3[,x]
 q.temp  <- which(data_mt==min(misrate3))
 cbind(rep(x, length(q.temp)), q.temp)
 })
select3 <- do.call(rbind, q_select3)
colnames(select3) <- c("mt", "q")

#2 clusters
mt_select2 <- which(apply(misrate2, 2, min)==min(misrate2))
q_select2 <- lapply(mt_select2, function(x){
  data_mt <- misrate2[,x]
  q.temp  <- which(data_mt==min(misrate2))
  cbind(rep(x, length(q.temp)), q.temp)
})
select2 <- do.call(rbind, q_select2)
colnames(select2) <- c("mt", "q")




draw_plot3 <- function(data, mt, q){
  #mt is the selected optimal para, q is the corresponding quantile
  t <- para[mt,1]
  m <- para[mt,2]
  alldata_mt <- data  %>% filter(measure==m, tract==t) %>% select(group, den1:den200)
  group_mt <- alldata_mt$group
  alldata_mt <- select(alldata_mt, -group)
  
  alldata_mtq <- alldata_mt[,q]
  min_group <- result_3cl[[mt]][[q]]$group
  min_misrate <- result_3cl[[mt]][[q]]$misrate
  
  matplot(den.quantiles/100, t(alldata_mt), type="l",
          main=paste0("Quantile plot across 88 subjects\n measure:", m, ", tract:", t,
                     ", q:", round(den.quantiles[q], digits=2), "%\n misrate:", round(min_misrate, digits=2)),
          xlab="t", ylab="q(t)",
          col=ifelse(min_group==1, "red", ifelse(min_group==2, "green", "blue")),
          lty=1)
  legend("bottomright", c("cluster 1", "cluster 2", "cluster 3"), bty='n', cex=0.8,
         lty=c(1,1,1), col=c("red","green","blue"))
  abline(v=den.quantiles[q]/100, col="grey", lwd=1.5, lty=2)
  
  # the lines that are correctly clustered
  matplot(den.quantiles/100, t(alldata_mt[min_group==group_mt,]), type="l",
          main=paste0("Quantile plot across ",  dim(alldata_mt[min_group==group_mt,])[1]," correctly clustered subjects\n measure:", m, ", tract:", t,
                      ", q:", round(den.quantiles[q], digits=2), "%\n misrate:", round(min_misrate, digits=2)),
          xlab="t", ylab="q(t)",
          col=ifelse(min_group[min_group==group_mt]==1, "red", ifelse(min_group[min_group==group_mt]==2, "green", "blue")),
          lty=1)
  legend("bottomright", c("cluster 1", "cluster 2", "cluster 3"), bty='n', cex=0.8,
         lty=c(1,1,1), col=c("red","green","blue"))
  abline(v=den.quantiles[q]/100, col="grey", lwd=1.5, lty=2)
  
  # the lines that are incorrectly clustered
  matplot(den.quantiles/100, t(alldata_mt[min_group!=group_mt,]), type="l",
          main=paste0("Quantile plot across ", dim(alldata_mt[min_group!=group_mt,])[1], " incorrectly clustered subjects\n measure:", m, ", tract:", t,
                      ", q:", round(den.quantiles[q], digits=2), "%\n misrate:", round(min_misrate, digits=2)),
          xlab="t", ylab="q(t)",
          col=ifelse(min_group[min_group!=group_mt]==1, "red", ifelse(min_group[min_group!=group_mt]==2, "green", "blue")),
          lty=1)
  legend("bottomright", c("cluster 1", "cluster 2", "cluster 3"), bty='n', cex=0.8,
         lty=c(1,1,1), col=c("red","green","blue"))
  abline(v=den.quantiles[q]/100, col="grey", lwd=1.5, lty=2)
}

for (i in 1:dim(select3)[1]){
  draw_plot3(alldata.fb_q, select3[i,1], select3[i,2])
}
for (i in 1:dim(select3)[1]){
  draw_plot3(alldata.fb_q, select3[i,1], 100)
}

draw_plot2 <- function(data, mt, q){
  #mt is the selected optimal para, q is the corresponding quantile
  t <- para[mt,1]
  m <- para[mt,2]
  alldata_mt <- data  %>% filter(measure==m, tract==t, group!=3) %>% select(group, den1:den200)
  group_mt <- alldata_mt$group
  alldata_mt <- select(alldata_mt, -group)
  
  alldata_mtq <- alldata_mt[,q]
  min_group <- result_2cl[[mt]][[q]]$group
  min_misrate <- result_2cl[[mt]][[q]]$misrate
  
  matplot(den.quantiles/100, t(alldata_mt), type="l",
          main=paste0("Quantile plot across 73 subjects (True label)\n measure:", m, ", tract:", t,
                      ", q:", round(den.quantiles[q], digits=2), "%\n misrate:", round(min_misrate, digits=2)),
          xlab="t", ylab="q(t)",
          col=ifelse(group_mt==1, "red", "green"),
          lty=1)
  legend("bottomright", c("cluster 1", "cluster 2"), bty='n', cex=0.8,
         lty=c(1,1), col=c("red","green"))
  abline(v=den.quantiles[q]/100, col="grey", lwd=1.5, lty=2)
  
  # the lines that are correctly clustered
  matplot(den.quantiles/100, t(alldata_mt[min_group==group_mt,]), type="l",
          main=paste0("Quantile plot across ",  dim(alldata_mt[min_group==group_mt,])[1], " correctly clustered subjects\n measure:", m, ", tract:", t,
                      ", q:", round(den.quantiles[q], digits=2), "%\n misrate:", round(min_misrate, digits=2)),
          xlab="t", ylab="q(t)",
          col=ifelse(min_group[min_group==group_mt]==1, "red", "green"),
          lty=1)
  legend("bottomright", c("cluster 1", "cluster 2"), bty='n', cex=0.8,
         lty=c(1,1), col=c("red","green"))
  abline(v=den.quantiles[q]/100, col="grey", lwd=1.5, lty=2)
  
  # the lines that are incorrectly clustered
  matplot(den.quantiles/100, t(alldata_mt[min_group!=group_mt,]), type="l",
          main=paste0("Quantile plot across ",  dim(alldata_mt[min_group!=group_mt,])[1], " incorrectly clustered subjects\n measure:", m, ", tract:", t,
                      ", q:", round(den.quantiles[q], digits=2), "%\n misrate:", round(min_misrate, digits=2)),
          xlab="t", ylab="q(t)",
          col=ifelse(min_group[min_group!=group_mt]==1, "red", "green"),
          lty=1)
  legend("bottomright", c("cluster 1", "cluster 2"), bty='n', cex=0.8,
         lty=c(1,1), col=c("red","green"))
  abline(v=den.quantiles[q]/100, col="grey", lwd=1.5, lty=2)
}

for (i in 1:dim(select2)[1]){
  draw_plot2(alldata.fb_q, select2[i,1], select2[i,2])
}
for (i in 1:dim(select2)[1]){
  draw_plot2(alldata.fb_q, select2[i,1], 100)
}


## random forest classifcation
mt <- select3[5,1]
q <- select3[5,2]
t <- para[mt,1]
m <- para[mt,2]
library(randomForest)
library(caret)

rf_class<- function(data, groupnum){
  misrate <- c()
  for (i in 1:108){
    m <- para[i, 2]
    t <- para[i, 1]
    if(groupnum==3){
      alldata_mt <- data  %>% filter(measure==m, tract==t) %>% select(group, den1:den100)
    }else{
      alldata_mt <- data  %>% filter(measure==m, tract==t) %>% select(group, den1:den100) %>% filter(group!=3)
    }
    
    alldata_mt$group <- as.factor(alldata_mt$group)
    group_mt <- alldata_mt$group
    set.seed(1000)
    ## create 70% data as training data, 30% data as validating dataset, 1 is for training, 2 is for validate
    data_ind <-sample(2, nrow(alldata_mt), replace=T, prob=c(0.7, 0.3))
    
    data.tr = alldata_mt[data_ind==1,]  
    data.val = alldata_mt[data_ind==2,] 
    #table(group_mt[data_ind==1])/sum(data_ind==1)
    #table(group_mt[data_ind==2])/sum(data_ind==2)
    #orginal proportion, expect to see the proportion similar
    #table(group_mt)/length(group_mt)
    
  
    #Fit Random Forest Model
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = 5,
      ## repeated ten times
      repeats = 3)
    #gbmGrid <-  expand.grid(mtry=seq(10,200, length=20))
    rfgrid <- expand.grid(mtry=seq(5,40,by=5), splitrule=c("gini","extratrees"), min.node.size=1)
    rfFit <- train(group ~ ., data = data.tr,
                   method = "ranger",
                   trControl = fitControl,
                   tuneGrid=rfgrid,
                   num.tree=1000)
    data.val$predicted.response = predict(rfFit, data.val)
    misrate <- c(misrate, sum(data.val$predicted.response!=data.val$group)/length(data.val$group))
  }
  return(misrate)
}
mis2 <- rf_class(alldata.fb, 2)
mis3 <- rf_class(alldata.fb, 3)
mis2.q <- rf_class(alldata.fb_q, 2)
mis3.q <- rf_class(alldata.fb_q, 3)
mis2.qdt <- rf_class(alldata.fb_qdt, 2)
mis3.qdt <- rf_class(alldata.fb_qdt, 3)















misrate2 <- c()
for (i in 1:108){
  m <- para[i, 2]
  t <- para[i, 1]
  alldata_mt <- alldata.fb_qdt  %>% filter(measure==m, tract==t) %>% select(group, den1:den100) %>% filter(group!=3)
  alldata_mt$group <- as.factor(alldata_mt$group)
  group_mt <- alldata_mt$group
  ## create 70% data as training data, 30% data as validating dataset, 1 is for training, 2 is for validate
  data_ind <-sample(2, nrow(alldata_mt), replace=T, prob=c(0.7, 0.3))
  
  data.tr = alldata_mt[data_ind==1,]  
  data.val = alldata_mt[data_ind==2,] 
  #table(group_mt[data_ind==1])/sum(data_ind==1)
  #table(group_mt[data_ind==2])/sum(data_ind==2)
  #orginal proportion, expect to see the proportion similar
  #table(group_mt)/length(group_mt)
  
  library(randomForest)
  #Fit Random Forest Model
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)
  rf = randomForest(group ~ .,  
                    data=data.tr,
                    method = "rf", 
                    trControl = fitControl, verbose = FALSE)
  data.val$predicted.response = predict(rf, data.val)
  misrate2 <- c(misrate2, sum(data.val$predicted.response!=data.val$group)/length(data.val$group))
  
}

plot(misrate2, xlab="Model condition", ylab="Out of sample(30%) misrate",  main="2 groups classification")
plot(misrate, xlab="Model condition", ylab="Out of sample(30%) misrate",  main="3 groups classification")

plot(rf)
print(rf)

# Variable Importance
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")
#Variable Importance
var.imp = data.frame(importance(rf,  
                                type=2))

library(caret)
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
set.seed(825)
gbmFit1 <- train(group ~ ., data = data.tr, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1



# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])
var.imp <- var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]
data.tr.imp <- select(data.tr, group, var.imp$Variables[1:5])
rf = randomForest(group ~ .,  
                  ntree = 100, data=data.tr)
plot(rf)
print(rf)
# Predicting response variable
data.val$predicted.response = predict(rf, data.val)

# Create Confusion Matrix
confusion3 <- t(sapply(1:3, function(x){as.numeric(table(data.val$predicted.response[data.val$group==x]))}))
confusion3 <- cbind(confusion3, (rowSums(confusion3)-diag(confusion3))/rowSums(confusion3))
rownames(confusion3) <- paste0("true", 1:3)
colnames(confusion3) <- c(paste0("claim", 1:3), "class.error" )
confusion3
misrate3 <- (sum(confusion3)-sum(diag(confusion3)))/sum(confusion3)
misrate <- c(misrate, misrate3)
}

## gradient boosting classification

library(gbm)
library(xgboost)
library(e1071)
Boston.boost=gbm(group ~ . ,data = data.tr,distribution = "gaussian",n.trees = 1000,
                 shrinkage = 0.01, interaction.depth = 4)
Boston.boost

predmatrix<-predict(Boston.boost,data.val,n.trees = 1000)


# Fit the model on the training set
set.seed(123)
model <- train(
  group ~., data = data.tr, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter
model$bestTune
# Make predictions on the test data
predicted.classes <- model %>% predict(data.val)
head(predicted.classes)
# Compute model prediction accuracy rate
mean(predicted.classes == data.val$group)

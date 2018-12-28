fulldir <- list.files(path="/Users/mawanying/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=TRUE, recursive=FALSE)
files <- list.files(path="/Users/mawanying/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)
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

persub <- function(sub.index, measure){
  #sub.index: 1~195
  #measure:any measurement subset from c("FA", "MD", "Da", "Dr")
  sub_dir <- fulldir[sub.index]
  sub.i <- read.csv(sub_dir)
  sub.i.measure <- sub.i %>% select(Tract, one_of(measure)) 
  # den.est is a list of length(tracts in sub.index), each element is a matrix of length(measure)x203
  tractnames <- unique(sub.i.measure$Tract)
  den.est <- lapply(tractnames, function(x){
    tract.j <- sub.i.measure %>% filter(Tract==x) %>% select(-Tract)
    den <- apply(tract.j, 2, function(x){
      x <- (x-min(x))/(max(x)-min(x))
      den.col <- density(x, n=length(x), na.rm=TRUE)
      points <- round(quantile(1:length(x), den.quantiles/100))
      den.col$y[points] #den:200xlength(measure)
      })
    den.est.ij <- data.frame(player=rep(subname[sub.index], length(measure)), tract=rep(x, length(measure)), measure = measure, den.ij=t(den))
    den.est.ij})
  persub_data <- do.call(rbind,den.est) #persub_data: player, tract, measure, density1~200
  rownames(persub_data) <- NULL
  return(persub_data)
} 
  

persub_q <- function(sub.index, measure){
  #sub.index: 1~195
  #measure:any measurement subset from c("FA", "MD", "Da", "Dr")
  sub_dir <- fulldir[sub.index]
  sub.i <- fread(sub_dir)
  sub.i.measure <- sub.i %>% select(Tract, one_of(measure)) 
  # den.est is a list of length(tracts in sub.index), each element is a matrix of length(measure)x203
  tractnames <- unique(sub.i.measure$Tract)
  den.est <- lapply(tractnames, function(x){
    tract.j <- sub.i.measure %>% filter(Tract==x) %>% select(-Tract)
    den <- apply(tract.j, 2, function(x){
      quantile(x, den.quantiles/100)
    })
    den.est.ij <- data.frame(player=rep(subname[sub.index], length(measure)), tract=rep(x, length(measure)), measure = measure, quan.ij=t(den))
    den.est.ij})
  persub_data <- do.call(rbind,den.est) #persub_data: player, tract, measure, density1~200
  rownames(persub_data) <- NULL
  return(persub_data)
} 


persub_qd <- function(sub.index, measure){
  #sub.index: 1~195
  #measure:any measurement subset from c("FA", "MD", "Da", "Dr")
  sub_dir <- fulldir[sub.index]
  sub.i <- read.csv(sub_dir)
  sub.i.measure <- sub.i %>% select(Tract, one_of(measure)) 
  # den.est is a list of length(tracts in sub.index), each element is a matrix of length(measure)x203
  tractnames <- unique(sub.i.measure$Tract)
  den.est <- lapply(tractnames, function(x){
    tract.j <- sub.i.measure %>% filter(Tract==x) %>% select(-Tract)
    den <- apply(tract.j, 2, function(x){
      q <- quantile(x, den.quantiles/100)
      den.col <- density(x, n=length(x), na.rm=TRUE)
      qd <- sapply(q, function(k){
        interpl <- approx(den.col$x, den.col$y, k)
        interpl$y
        })
      qd
    })
    den.est.ij <- data.frame(player=rep(subname[sub.index], length(measure)), tract=rep(x, length(measure)), measure = measure, qd.ij=t(den))
    den.est.ij})
  persub_data <- do.call(rbind,den.est) #persub_data: player, tract, measure, density1~200
  rownames(persub_data) <- NULL
  return(persub_data)
} 

persub_qdt <- function(sub.index, measure){
  #sub.index: 1~195
  #measure:any measurement subset from c("FA", "MD", "Da", "Dr")
  sub_dir <- fulldir[sub.index]
  sub.i <- fread(sub_dir)
  sub.i.measure <- sub.i %>% select(Tract, one_of(measure)) 
  # den.est is a list of length(tracts in sub.index), each element is a matrix of length(measure)x203
  tractnames <- unique(sub.i.measure$Tract)
  den.est <- lapply(tractnames, function(x){
    tract.j <- sub.i.measure %>% filter(Tract==x) %>% select(-Tract)
    den <- apply(tract.j, 2, function(x){
      x <- (x-min(x))/(max(x)-min(x))
      q <- quantile(x, den.quantiles/100)
      den.col <- density(x, n=length(x), na.rm=TRUE)
      qd <- sapply(q, function(k){
        interpl <- approx(den.col$x, den.col$y, k)
        -log(interpl$y)
      })
      qd
    })
    den.est.ij <- data.frame(player=rep(subname[sub.index], length(measure)), tract=rep(x, length(measure)), measure = measure, qdt.ij=t(den))
    den.est.ij})
  persub_data <- do.call(rbind,den.est) #persub_data: player, tract, measure, density1~200
  rownames(persub_data) <- NULL
  return(persub_data)
} 
#alldata <- lapply(1:length(fulldir), function(x)persub(x, c("FA", "MD", "Da", "Dr")))
#combine.data <- do.call(rbind, alldata)

alldata<-c()
for(i in 1:length(fulldir)){
  print(i)
  alldata.i <- persub(i, c("FA", "MD", "Da", "Dr"))
  alldata <- rbind(alldata, alldata.i)
}
# alldata is 21052x203
# every player but the 46th player, has 27 tracts; the 46th player has 25 tracts

alldata_q <- lapply(1:length(fulldir), function(x)persub_q(x, c("FA", "MD", "Da", "Dr")))
alldata_q <- do.call(rbind, alldata_q)

alldata_qd <- lapply(1:length(fulldir), function(x)persub_qd(x, c("FA", "MD", "Da", "Dr")))
alldata_qd <- do.call(rbind, alldata_qd)

alldata_qdt <- lapply(1:length(fulldir), function(x)persub_qdt(x, c("FA", "MD", "Da", "Dr")))
alldata_qdt <- do.call(rbind, alldata_qdt)

tractnumber <- c()
tract.info <- list()
for(i in 1:length(fulldir)){
  print(i)
  sub.i <- read.csv(fulldir[i])
  sub.i.measure <- sub.i %>% select(Tract)  
  tractnames <- unique(sub.i.measure$Tract)
  tractnumber <- c(tractnumber, length(tractnames))
  tract.info[[i]] <- tractnames
}
table(tractnumber)

save(alldata, tractnumber, tract.info, file="/Users/mawanying/Dropbox/Luo_Jarek_YC/summary_data.Rdata")
save(alldata_q, file="/Users/mawanying/Dropbox/Luo_Jarek_YC/alldata_q.Rdata")
save(alldata_qd, file="/Users/mawanying/Dropbox/Luo_Jarek_YC/alldata_qd.Rdata")
save(alldata_qdt, file="/Users/mawanying/Dropbox/Luo_Jarek_YC/alldata_qdt.Rdata")

load("/Users/mawanying/Dropbox/Luo_Jarek_YC/summary_data.Rdata")
load("/Users/mawanying/Dropbox/Luo_Jarek_YC/alldata_q.Rdata")
load("/Users/mawanying/Dropbox/Luo_Jarek_YC/alldata_qd.Rdata")
#############################################################################
######################### players information table #########################
######################### create group index ################################
######################### delete duplicates #################################
#############################################################################

player.info <- read.csv("/Users/mawanying/Dropbox/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9
basic.info <- player.info %>% select(SUBJECTSTUDYNUM, INJURYSPORTVARSITY, CONTACTSPORTQGID, NONCONTACTSPORTQGID) #basic.info is 136x4
football.info <- basic.info %>% filter(INJURYSPORTVARSITY=='Football') #football.info is 88x4
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
football.noDup <- football.info[rowSums(dupli)<1, ] # football.noDup is 53x4

#############################################################################
############## construct group index for football players ###################
##############      group1: SUBJECTSTUDYNUM                 ############
##############      group2: CONTACTSPORTQGID                ############
##############      group3: NONCONTACTSPORTQGID             ############
#############################################################################
group1 <- unique(as.character(football.noDup$SUBJECTSTUDYNUM))     #SUBJECTSTUDYNUM 43x1
group2 <- unique(as.character(football.noDup$CONTACTSPORTQGID))    #CONTACTSPORTQGID 43x1
group3 <- unique(as.character(football.noDup$NONCONTACTSPORTQGID)) #NONCONTACTSPORTQGID 43x1

# after merge, alldata_fb will have 28 players in group 1, 30players in group 2, 28 players in group 3
#############################################################################
alldata.player <- as.character(alldata$player) #21052x1
alldata.fb.player <- alldata.player %in% c(group1, group2, group3) #indicator vector: 9280x1
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

addgroupindex <- function(data, football_index, groupindex){
  fbdata <- data[football_index, ]
  data.fb <- fbdata %>% mutate(group=groupindex)
  return(data.fb)
}
alldata.fb <- addgroupindex(alldata, alldata.fb.player, groupindex) #9280x204
alldata.fb_q <- addgroupindex(alldata_q, alldata.fb.player, groupindex) #9280x204
alldata.fb_qd <- addgroupindex(alldata_qd, alldata.fb.player, groupindex) #9280x204
alldata.fb_qdt <- addgroupindex(alldata_qdt, alldata.fb.player, groupindex) #9280x204

colnames(alldata.fb)[4:203] <- colnames(alldata.fb_q)[4:203] <- colnames(alldata.fb_qd)[4:203] <- colnames(alldata.fb_qdt)[4:203] <- paste0("den", 1:200)
table(alldata.fb$group)
#1    2    3 
#3016 3240 3024 
# there are 86 different players in total for alldata.fb
save(alldata.fb, file="/Users/mawanying/Dropbox/Luo_Jarek_YC/football_Withindex.Rdata")

#############################################################################
alldata.fb

m.tract.cluster <- function(data, iter){
  # data: input dataset
  # m: measurement, in {"FA", "MD", "Da", "Dr"}
  # t: tract
  t <- as.character(para[iter, 1])
  m <- as.character(para[iter, 2])
  data.tm <- data %>% filter(tract==t, measure==m) #86x204
  den.tm <- select(data.tm, den1:den200)    #86x200
  #den.tm <- den.tm - as.vector(rep(1, dim(den.tm)[1])) %*% t(colMeans(den.tm))
  den.fpca <- refund::fpca.face(data.matrix(den.tm), center = TRUE, argvals = (1:200)/200, knots = 7, pve = 0.9999, p = 5, lambda = NULL)
  den.score <- den.fpca$scores[,1:5]/sqrt(dim(den.tm)[2]) #86x5
  den.cluster <- kmeans(den.score, centers = 3)
  permute <- function(vec, p1, p2, p3){
    for(i in 1:length(vec)){
      if(vec[i]==1){vec[i] <- p1}
      if(vec[i]==2){vec[i] <- p2}
      if(vec[i]==3){vec[i] <- p3}
    }
    return(vec)
  }
  perm1 <- permute(den.cluster$cluster, 1,2,3)
  perm2 <- permute(den.cluster$cluster, 1,3,2)
  perm3 <- permute(den.cluster$cluster, 2,1,3)
  perm4 <- permute(den.cluster$cluster, 2,3,1)
  perm5 <- permute(den.cluster$cluster, 3,1,2)
  perm6 <- permute(den.cluster$cluster, 3,2,1)
  misrate <- min(sum(perm1!=data.tm$group)/length(data.tm$group),
      sum(perm2!=data.tm$group)/length(data.tm$group),
      sum(perm3!=data.tm$group)/length(data.tm$group),
      sum(perm4!=data.tm$group)/length(data.tm$group),
      sum(perm5!=data.tm$group)/length(data.tm$group),
      sum(perm6!=data.tm$group)/length(data.tm$group))
  data.tm.comb = cbind(data.tm, den.score)
  return(list(misrate=misrate,
              data.tm.comb=data.tm.comb, 
              den.fpca=den.fpca))
}

para <- expand.grid(tractnames, c("FA","MD","Da","Dr"))
## equally spaced density 3 clusters 
misrate_FA <- sapply(1:27, function(x) m.tract.cluster(alldata.fb, x)$misrate)
misrate_MD <- sapply(28:54, function(x) m.tract.cluster(alldata.fb, x)$misrate)
misrate_Da <- sapply(55:81, function(x) m.tract.cluster(alldata.fb, x)$misrate)
misrate_Dr <- sapply(82:108, function(x) m.tract.cluster(alldata.fb, x)$misrate)
misrate_FA[order(misrate_FA)[1:5]]
misrate_MD[order(misrate_MD)[1:5]]
misrate_Da[order(misrate_Da)[1:5]]
misrate_Dr[order(misrate_Dr)[1:5]]

tractnames[order(misrate_FA)[1:5]]
tractnames[order(misrate_MD)[1:5]]
tractnames[order(misrate_Da)[1:5]]
tractnames[order(misrate_Dr)[1:5]]
## use quantiles 3 clusters
misrate_FA_q <- sapply(1:27, function(x) m.tract.cluster(alldata.fb_q, x)$misrate)
misrate_MD_q <- sapply(28:54, function(x) m.tract.cluster(alldata.fb_q, x)$misrate)
misrate_Da_q <- sapply(55:81, function(x) m.tract.cluster(alldata.fb_q, x)$misrate)
misrate_Dr_q <- sapply(82:108, function(x) m.tract.cluster(alldata.fb_q, x)$misrate)
# mini 53%
misrate_FA_q[order(misrate_FA_q)[1:5]]
misrate_MD_q[order(misrate_MD_q)[1:5]]
misrate_Da_q[order(misrate_Da_q)[1:5]]
misrate_Dr_q[order(misrate_Dr_q)[1:5]]

tractnames[order(misrate_FA_q)[1:5]]
tractnames[order(misrate_MD_q)[1:5]]
tractnames[order(misrate_Da_q)[1:5]]
tractnames[order(misrate_Dr_q)[1:5]]

## use density corresponding to quantiles 3 clusters
misrate_FA_qd <- sapply(1:27, function(x) m.tract.cluster(alldata.fb_qd, x)$misrate)
misrate_MD_qd <- sapply(28:54, function(x) m.tract.cluster(alldata.fb_qd, x)$misrate)
misrate_Da_qd <- sapply(55:81, function(x) m.tract.cluster(alldata.fb_qd, x)$misrate)
misrate_Dr_qd <- sapply(82:108, function(x) m.tract.cluster(alldata.fb_qd, x)$misrate)

misrate_FA_qd[order(misrate_FA_qd)[1:5]]
misrate_MD_qd[order(misrate_MD_qd)[1:5]]
misrate_Da_qd[order(misrate_Da_qd)[1:5]]
misrate_Dr_qd[order(misrate_Dr_qd)[1:5]]

tractnames[order(misrate_FA_qd)[1:5]]
tractnames[order(misrate_MD_qd)[1:5]]
tractnames[order(misrate_Da_qd)[1:5]]
tractnames[order(misrate_Dr_qd)[1:5]]

## use quantile-density transformation corresponding to quantiles 3 clusters
misrate_FA_qdt <- sapply(1:27, function(x) m.tract.cluster(alldata.fb_qdt, x)$misrate)
misrate_MD_qdt <- sapply(28:54, function(x) m.tract.cluster(alldata.fb_qdt, x)$misrate)
misrate_Da_qdt <- sapply(55:81, function(x) m.tract.cluster(alldata.fb_qdt, x)$misrate)
misrate_Dr_qdt <- sapply(82:108, function(x) m.tract.cluster(alldata.fb_qdt, x)$misrate)

misrate_FA_qdt[order(misrate_FA_qdt)[1:5]]
misrate_MD_qdt[order(misrate_MD_qdt)[1:5]]
misrate_Da_qdt[order(misrate_Da_qdt)[1:5]]
misrate_Dr_qdt[order(misrate_Dr_qdt)[1:5]]

tractnames[order(misrate_FA_qdt)[1:5]]
tractnames[order(misrate_MD_qdt)[1:5]]
tractnames[order(misrate_Da_qdt)[1:5]]
tractnames[order(misrate_Dr_qdt)[1:5]]


fb_score_qdt <- lapply(1:dim(para)[1], function(x) m.tract.cluster(alldata.fb_qdt, x)$data.tm.comb)
fb_score_qdt <- do.call(rbind, fb_score_qdt) #9280x209
fb_score_qdt.fpca <- lapply(1:dim(para)[1], function(x) m.tract.cluster(alldata.fb_qdt, x)$den.fpca)


loc_m <- which(para[,2]=='Da')
loc <- loc_m[which(para[loc_m,1]=='ml_l')] #loc in the para that is tract=='ml_l', measure=='Da'
eigenf <- fb_score_qdt.fpca[[loc]]$efunctions[,1:5]*sqrt(200)
eigens <- fb_score_qdt.fpca[[loc]]$scores[,1:5]/sqrt(200)
mean <- fb_score_qdt.fpca[[loc]]$mu
pc1_score_q <- quantile(eigens[,1], c(0.1, 0.25, 0.75, 0.95))
modes1 <- t(as.vector(pc1_score_q)%*% t(eigenf[,1])) + mean #200x4
pc2_score_q <- quantile(eigens[,2], c(0.1, 0.25, 0.75, 0.95))
modes2 <- t(as.vector(pc2_score_q)%*% t(eigenf[,2])) + mean #200x4
matplot(den.quantiles/100, exp(-modes1), main="1st PC modes", col=c("red","pink","green","blue"), type=c("l","l","l","l"))
lines(den.quantiles/100, exp(-mean), col="black", ltw=1.5)
matplot(den.quantiles/100, exp(-modes2), main="2nd PC modes", col=c("red","pink","green","blue"), type="l")
lines(den.quantiles/100, exp(-mean), col="black", ltw=1.5)


fb_score <- lapply(1:dim(para)[1], function(x) m.tract.cluster(alldata.fb, x)$data.tm.comb)
fb_score <- do.call(rbind, fb_score) #9280x209
fb_scorefpca <- lapply(1:dim(para)[1], function(x) m.tract.cluster(alldata.fb, x)$den.fpca)


loc_m <- which(para[,2]=='Da')
loc <- loc_m[which(para[loc_m,1]=='ml_l')] #loc in the para that is tract=='ml_l', measure=='Da'
eigenf <- fb_scorefpca[[loc]]$efunctions[,1:5]*sqrt(200)
eigens <- fb_scorefpca[[loc]]$scores[,1:5]/sqrt(200)
mean <- fb_scorefpca[[loc]]$mu
pc1_score_q <- quantile(eigens[,1], c(0.1, 0.25, 0.75, 0.95))
modes1 <- t(as.vector(pc1_score_q)%*% t(eigenf[,1])) + mean #200x4
pc2_score_q <- quantile(eigens[,2], c(0.1, 0.25, 0.75, 0.95))
modes2 <- t(as.vector(pc2_score_q)%*% t(eigenf[,2])) + mean #200x4

matplot(den.quantiles/100, modes1, title="1st PC modes", col=c("red","pink","green","blue"), type="l")
lines(den.quantiles/100, mean, col="black", ltw=1.5)
matplot(den.quantiles/100, modes2, title="2nd PC modes", col=c("red","pink","green","blue"), type="l")
lines(den.quantiles/100, mean, col="black", ltw=1.5)



m.tract.cluster2 <- function(data, iter){
  # data: input dataset
  # m: measurement, in {"FA", "MD", "Da", "Dr"}
  # t: tract
  t <- as.character(para[iter, 1])
  m <- as.character(para[iter, 2])
  
  data.tm <- data %>% filter(tract==t, measure==m, group != 3) #86x204
  den.tm <- select(data.tm, den1:den200)    #86x200
  #den.tm <- den.tm - as.vector(rep(1, dim(den.tm)[1])) %*% t(colMeans(den.tm))
  den.fpca <- refund::fpca.face(data.matrix(den.tm), center = TRUE, argvals = (1:200)/200, knots = 7, pve = 0.9999, p = 5, lambda = NULL)
  den.score <- den.fpca$scores[,1:5]/sqrt(dim(den.tm)[2]) #86x5
  den.cluster <- kmeans(den.score, centers = 2)
  permute <- function(vec, p1, p2){
    for(i in 1:length(vec)){
      if(vec[i]==1){vec[i] <- p1}
      if(vec[i]==2){vec[i] <- p2}
    }
    return(vec)
  }
  perm1 <- permute(den.cluster$cluster, 1,2)
  perm2 <- permute(den.cluster$cluster, 2,1)
  misrate <- min(sum(perm1!=data.tm$group)/length(data.tm$group),
                 sum(perm2!=data.tm$group)/length(data.tm$group))
  data.tm.comb = cbind(data.tm, den.score)
  return(list(misrate=misrate,
              data.tm.comb=data.tm.comb))
}
# 2 cluster density
misrate_FA2 <- sapply(1:27,  function(x) m.tract.cluster2(alldata.fb, x)$misrate)
misrate_MD2 <- sapply(28:54, function(x) m.tract.cluster2(alldata.fb, x)$misrate)
misrate_Da2 <- sapply(55:81, function(x) m.tract.cluster2(alldata.fb, x)$misrate)
misrate_Dr2 <- sapply(82:108,function(x) m.tract.cluster2(alldata.fb, x)$misrate)
misrate_FA2[order(misrate_FA2)[1:5]]
misrate_MD2[order(misrate_MD2)[1:5]]
misrate_Da2[order(misrate_Da2)[1:5]]
misrate_Dr2[order(misrate_Dr2)[1:5]]

tractnames[order(misrate_FA2)[1:5]]
tractnames[order(misrate_MD2)[1:5]]
tractnames[order(misrate_Da2)[1:5]]
tractnames[order(misrate_Dr2)[1:5]]

## use quantiles 2 clusters
misrate_FA_q2 <- sapply(1:27, function(x) m.tract.cluster2(alldata.fb_q, x)$misrate)
misrate_MD_q2 <- sapply(28:54, function(x) m.tract.cluster2(alldata.fb_q, x)$misrate)
misrate_Da_q2 <- sapply(55:81, function(x) m.tract.cluster2(alldata.fb_q, x)$misrate)
misrate_Dr_q2 <- sapply(82:108, function(x) m.tract.cluster2(alldata.fb_q, x)$misrate)

misrate_FA_q2[order(misrate_FA_q2)[1:5]]
misrate_MD_q2[order(misrate_MD_q2)[1:5]]
misrate_Da_q2[order(misrate_Da_q2)[1:5]]
misrate_Dr_q2[order(misrate_Dr_q2)[1:5]]

tractnames[order(misrate_FA_q2)[1:5]]
tractnames[order(misrate_MD_q2)[1:5]]
tractnames[order(misrate_Da_q2)[1:5]]
tractnames[order(misrate_Dr_q2)[1:5]]

## use density corresponding to quantiles 2 clusters
misrate_FA_qd2 <- sapply(1:27, function(x) m.tract.cluster2(alldata.fb_qd, x)$misrate)
misrate_MD_qd2 <- sapply(28:54, function(x) m.tract.cluster2(alldata.fb_qd, x)$misrate)
misrate_Da_qd2 <- sapply(55:81, function(x) m.tract.cluster2(alldata.fb_qd, x)$misrate)
misrate_Dr_qd2 <- sapply(82:108, function(x) m.tract.cluster2(alldata.fb_qd, x)$misrate)

misrate_FA_qd2[order(misrate_FA_qd2)[1:5]]
misrate_MD_qd2[order(misrate_MD_qd2)[1:5]]
misrate_Da_qd2[order(misrate_Da_qd2)[1:5]]
misrate_Dr_qd2[order(misrate_Dr_qd2)[1:5]]

tractnames[order(misrate_FA_qd2)[1:5]]
tractnames[order(misrate_MD_qd2)[1:5]]
tractnames[order(misrate_Da_qd2)[1:5]]
tractnames[order(misrate_Dr_qd2)[1:5]]

## use quantile-density transformation corresponding to quantiles 2 clusters
misrate_FA_qdt2 <- sapply(1:27, function(x) m.tract.cluster2(alldata.fb_qdt, x)$misrate)
misrate_MD_qdt2 <- sapply(28:54, function(x) m.tract.cluster2(alldata.fb_qdt, x)$misrate)
misrate_Da_qdt2 <- sapply(55:81, function(x) m.tract.cluster2(alldata.fb_qdt, x)$misrate)
misrate_Dr_qdt2 <- sapply(82:108, function(x) m.tract.cluster2(alldata.fb_qdt, x)$misrate)

misrate_FA_qdt2[order(misrate_FA_qdt2)[1:5]]
misrate_MD_qdt2[order(misrate_MD_qdt2)[1:5]]
misrate_Da_qdt2[order(misrate_Da_qdt2)[1:5]]
misrate_Dr_qdt2[order(misrate_Dr_qdt2)[1:5]]

tractnames[order(misrate_FA_qdt2)[1:5]]
tractnames[order(misrate_MD_qdt2)[1:5]]
tractnames[order(misrate_Da_qdt2)[1:5]]
tractnames[order(misrate_Dr_qdt2)[1:5]]


fb_score2 <- lapply(1:dim(para)[1], function(x) m.tract.cluster2(alldata.fb, x)$data.tm.comb)
fb_score2 <- do.call(rbind, fb_score2) #9280x209



#tract = "cgc_r", measure = "Da", qd dataset
permute <- function(vec, p1, p2){
  for(i in 1:length(vec)){
    if(vec[i]==1){vec[i] <- p1}
    if(vec[i]==2){vec[i] <- p2}
  }
  return(vec)
}
data.tm <- alldata.fb_qd %>% filter(tract=="cgc_r", measure=="Da", group != 3) 
den.tm <- select(data.tm, den1:den200)    #86x200
den.tm <- den.tm - as.vector(rep(1, dim(den.tm)[1])) %*% t(colMeans(den.tm))
den.fpca <- refund::fpca.face(data.matrix(den.tm), center = TRUE, argvals = (1:200)/200, knots = 7, pve = 0.9999, p = 5, lambda = NULL)
den.score <- den.fpca$scores[,1:5]/sqrt(dim(den.tm)[2]) #86x5
den.cluster <- kmeans(den.score, centers = 2)
perm1 <- permute(den.cluster$cluster, 1,2)
perm2 <- permute(den.cluster$cluster, 2,1)
mean(perm1!=data.tm$group) #0.2931034
mean(perm2!=data.tm$group) #0.5172414
plot(den.score[,1],den.score[,2], 
     col=ifelse(den.cluster$cluster==1, "red", "blue"),
     xlab="score1", ylab="score2",
     main="tract(cgc_r), Da, football, 29%")
points(den.score[,1],den.score[,2], 
     col=ifelse(data.tm$group==1, "red", "blue"), pch=4)
legend("topleft",legend=c("1", "2"), pch=c(1,1), col=c("blue", " red"))


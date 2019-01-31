fulldir <- list.files(path="/Users/mawanying/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=TRUE, recursive=FALSE)
files <- list.files(path="/Users/mawanying/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)
# there are 195 players
subname <- gsub(pattern=".csv", replacement="", files)
player.info <- read.csv("/Users/mawanying/Dropbox/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9, there are 4 types: 88football, 30soccer, 10ice hockey, 8lacrosse


load("C:/Users/wma9/Dropbox/Luo_Jarek_YC/alldata_q.Rdata")
fulldir <- list.files(path="C:/Users/wma9/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=TRUE, recursive=FALSE)
files <- list.files(path="C:/Users/wma9/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)
player.info <- read.csv("C:/Users/wma9/Dropbox/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9, there are 4 types: 88football, 30soccer, 10ice hockey, 8lacrosse

# there are 195 players
subname <- gsub(pattern=".csv", replacement="", files)


# there are 27 tracts, 4 measures
tractnames = c("ar_l",  "ar_r",  "atr_l", "atr_r", "cgc_l", "cgc_r",
               "cgh_l", "cgh_r", "cst_l", "cst_r", "fma",   
               "fmi", "ifo_l", "ifo_r", "ilf_l", "ilf_r", "mcp",  
               "ml_l",  "ml_r",  "ptr_l", "ptr_r", "slf_l", "slf_r", 
               "str_l" ,"str_r", "unc_l", "unc_r")
measure = c("FA","MD","Da","Dr")
# 200 quantiles we want to use
den.quantiles = seq(0.25, 99.75, length=200)

require(dplyr)
require(data.table)


## exploration on the range(min, max) and length of observation for each players along each tract and each measure
summary_stat <- lapply(1:length(fulldir), function(x){
  sub.index <- x 
  subi_obs <- fread(fulldir[x])
  tractnames <- unique(subi_obs$Tract)
  summary.res.i <- lapply(tractnames, function(j){
    tract.j <- subi_obs %>% filter(Tract==j) %>% select(-Tract)
    sumy.stat <- sapply(measure, function(x){
      sumy.col <- select(tract.j, x)
      sumy.col <- sumy.col[!is.na(sumy.col)] 
      c(length(sumy.col), min(sumy.col), max(sumy.col),  
        quantile(sumy.col, 0.25), median(sumy.col), quantile(sumy.col, 0.75), quantile(sumy.col, 0.95))
    })
    sumy.stat <- t(sumy.stat)
    summary.res.ij <- data.frame(player=rep(subname[sub.index], length(measure)), tract=rep(j, length(measure)), measure=measure, 
                                 length=sumy.stat[,1], min=sumy.stat[,2], max=sumy.stat[,3],
                                 Qu_1st=sumy.stat[,4], median=sumy.stat[,5], Qu_3rd=sumy.stat[,6], Qu_95=sumy.stat[,7])
    summary.res.ij
    })
  summary_stat.i <- do.call(rbind, summary.res.i)
})
summary.stat <- do.call(rbind, summary_stat) #player,tract,measure,length,min,max,quantiles(0.25,0.5,0.75,0.95) 21052x10
save(summary.stat, file="C:/Users/wma9/Dropbox/football project/summary.stat.Rdata")

load("C:/Users/wma9/Dropbox/football project/summary.stat.Rdata") # 21052 x 10


### use 73 group1 and group2 football players only ###
#############################################################################

player.info <- read.csv("C:/Users/wma9/Dropbox/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9
files <- list.files(path="C:/Users/wma9/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)
subname <- gsub(pattern=".csv", replacement="", files)
player.info <- player.info %>% filter(INJURYSPORTVARSITY=='Football') #88x9

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
### use 73 group1 and group2 football players only ###
#############################################################################
newfb.ind <- subname %in% c(group1, group2) 
newfb.loc <- c(1:length(fulldir))[newfb.ind]

summary_stat <- lapply(newfb.loc, function(x){
  sub.index <- x 
  subi_obs <- fread(fulldir[x])
  tractnames <- unique(subi_obs$Tract)
  summary.res.i <- lapply(tractnames, function(j){
    tract.j <- subi_obs %>% filter(Tract==j) %>% select(-Tract)
    sumy.stat <- sapply(measure, function(x){
      sumy.col <- select(tract.j, x)
      sumy.col <- sumy.col[!is.na(sumy.col)] 
      c(length(sumy.col), min(sumy.col), max(sumy.col),  
        quantile(sumy.col, 0.25), median(sumy.col), quantile(sumy.col, 0.75), quantile(sumy.col, 0.95))
    })
    sumy.stat <- t(sumy.stat)
    summary.res.ij <- data.frame(player=rep(subname[sub.index], length(measure)), tract=rep(j, length(measure)), measure=measure, 
                                 length=sumy.stat[,1], min=sumy.stat[,2],max=sumy.stat[,3],
                                 Qu_1st=sumy.stat[,4], median=sumy.stat[,5], 
                                 Qu_3rd=sumy.stat[,6], Qu_95=sumy.stat[,7])
    summary.res.ij
  })
  summary_stat.i <- do.call(rbind, summary.res.i)
})
summary.stat.newfb <- do.call(rbind, summary_stat) #player,tract,measure,length,min,max,quantiles(0.25,0.5,0.75,0.95) #7876x10
save(summary.stat.newfb, file="//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/fromdropbox/summary.stat.newfb.Rdata")
write.csv(summary.stat.newfb, file="//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/fromdropbox/summary.stat.newfb.csv")

#when calculate variance, only focus on 73 football players group1 and group2
measure <- c("FA", "MD", "Da", "Dr")
library(Rfast)
#only consider the football players
var.lengthminmaxmed <- t(sapply(tractnames, function(x){
  data <- summary.stat.newfb %>% filter(tract==x)
  data.l <- data %>% filter(measure==measure[1]) %>% select(length)
  data1 <- data %>% filter(measure==measure[1]) %>% select(min, max, Qu_1st, median, Qu_3rd, Qu_95)
  data2 <- data %>% filter(measure==measure[2]) %>% select(min, max, Qu_1st, median, Qu_3rd, Qu_95)
  data3 <- data %>% filter(measure==measure[3]) %>% select(min, max, Qu_1st, median, Qu_3rd, Qu_95)
  data4 <- data %>% filter(measure==measure[4]) %>% select(min, max, Qu_1st, median, Qu_3rd, Qu_95)
  sqrt(Rfast::colVars(data.matrix(cbind(data.l, data1, data2, data3, data4))))}))

colnames(var.lengthminmaxmed) <- c("length.sd", 
                                   "FA.min.sd", "FA.max.sd", "FA.Qu_1st.sd", "FA.med.sd", "FA.Qu_3rd.sd", "FA.Qu_95.sd",
                                   "MD.min.sd", "MD.max.sd", "MD.Qu_1st.sd", "MD.med.sd", "MD.Qu_3rd.sd", "MD.Qu_95.sd",
                                   "Da.min.sd", "Da.max.sd", "Da.Qu_1st.sd", "Da.med.sd", "Da.Qu_3rd.sd", "Da.Qu_95.sd",
                                   "Dr.min.sd", "Dr.max.sd", "Dr.Qu_1st.sd", "Dr.med.sd", "Dr.Qu_3rd.sd", "Dr.Qu_95.sd")
var.lengthminmaxmed <- round(var.lengthminmaxmed, digits= 3)
var.lengthminmaxmed <- data.frame(var.lengthminmaxmed)
var.lengthminmaxmed$tract <- tractnames
write.csv(var.lengthminmaxmed, file="//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/fromdropbox/73newfb_summary_of_var.csv")
library(xlsx)
write.xlsx(var.lengthminmaxmed, file="//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/fromdropbox/73newfb_summary_of_var.xlsx", sheetName = "73football", 
           col.names = TRUE, row.names = FALSE, append = FALSE)


#######################################################################################################################
### now not caring about the variance, we look at the summary data for 27 tracts and 4 measurements ###
#######################################################################################################################
newfb.ind <- subname %in% c(group1, group2) 
newfb.loc <- c(1:length(fulldir))[newfb.ind] #newfb.loc[18]=46th player has only 25 tracts, all the rest 72 fb player has 27 tracts
para <- expand.grid(t=tractnames, m=measure)
summary.table.newfb <- lapply(tractnames, function(t){
  data.tm <- c()
  for(i in newfb.loc){
    data <- fread(fulldir[i])
    data.tm.i <- data %>% filter(Tract==t) %>% select(measure)
    if(dim(data.tm.i)[1]>0){
      data.tm <- rbind(data.tm, data.tm.i)
    }
  }
  data.tm <- t(data.tm)
  data.tm.s <- t(apply(data.tm, 1, function(x){
    c(quantile(x, 0.5/100), summary(x), quantile(x, 99.5/100), sd(x))
  }))
  data.tm.s <- data.frame(data.tm.s)
  data.tm.s <- data.tm.s %>% mutate(measure=measure, tract=t)
})
summary.table.newfb <- do.call(rbind, summary.table.newfb) #.5%, summarystat, 99.5%, sd: 108x11
### following: use keep the observed value if outside the [.5%, 99.5%] as .5% and 99.5% respectively ###
summary.table.newfb.99 <- lapply(tractnames, function(t){
  data.tm <- c()
  for(i in newfb.loc){
    data <- fread(fulldir[i])
    data.tm.i <- data %>% filter(Tract==t) %>% select(measure)
    if(dim(data.tm.i)[1]>0){
      data.tm <- rbind(data.tm, data.tm.i)
    }
  }
  data.tm <- t(data.tm)
  data.tm.s <- t(apply(data.tm, 1, function(x){
    range99 <- quantile(x, c(.5/100,99.5/100))
    non_outlier.x <- ifelse(x<range99[1], range99[1], ifelse(x>range99[2], range99[2], x))
    c(quantile(non_outlier.x, 1/100), summary(non_outlier.x), quantile(non_outlier.x, 99/100), sd(non_outlier.x))
  }))
  data.tm.s <- data.frame(data.tm.s)
  data.tm.s <- data.tm.s %>% mutate(measure=measure, tract=t)
})
summary.table.newfb.99 <- do.call(rbind, summary.table.newfb.99) #.5%, summarystat, 99.5%, sd: 108x11

### following: use exclude the observed value if outside the median+/- interquartile range (IQR) ###
summary.table.newfb.IQR <- lapply(tractnames, function(t){
  data.tm <- c()
  for(i in newfb.loc){
    data <- fread(fulldir[i])
    data.tm.i <- data %>% filter(Tract==t) %>% select(measure)
    if(dim(data.tm.i)[1]>0){
      data.tm <- rbind(data.tm, data.tm.i)
    }
  }
  data.tm <- t(data.tm)
  data.tm.s <- t(apply(data.tm, 1, function(x){
    IQR <- quantile(x, c(.25, .5, .75))
    non_outlier.ind <- x<=IQR[2]+1.5*(IQR[3]-IQR[1]) & x>=IQR[2]-1.5*(IQR[3]-IQR[1])
    non_outlier.x <- x[non_outlier.ind]
    c(quantile(non_outlier.x, 0.5/100), summary(non_outlier.x), quantile(non_outlier.x, 99.5/100), sd(non_outlier.x))
  }))
  data.tm.s <- data.frame(data.tm.s)
  data.tm.s <- data.tm.s %>% mutate(measure=measure, tract=t)
})
summary.table.newfb.IQR <- do.call(rbind, summary.table.newfb.IQR)
save(summary.table.newfb, summary.table.newfb.99, summary.table.newfb.IQR, file="//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/fromdropbox/73newfb_summarystat.Rdata")

library(xlsx)
write.xlsx(summary.table.newfb, file="//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/fromdropbox/73newfb_summarystat.xlsx", sheetName = "73football", 
           col.names = TRUE, row.names = FALSE, append = T)
write.xlsx(summary.table.newfb.99, file="//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/fromdropbox/73newfb_summarystat.xlsx", sheetName = "73football_99", 
           col.names = TRUE, row.names = FALSE, append = T)
write.xlsx(summary.table.newfb.IQR, file="//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/fromdropbox/73newfb_summarystat.xlsx", sheetName = "73football_afterIQR", 
           col.names = TRUE, row.names = FALSE, append = T)
#######################################################################################################################



#check that for the same tract, the observation length of 4 measures are the same
var.length <- sapply(tractnames, function(x){
  data1 <- summary.stat %>% filter(tract==x, measure==measure[1]) %>% select(length)
  data2 <- summary.stat %>% filter(tract==x, measure==measure[2]) %>% select(length)
  data3 <- summary.stat %>% filter(tract==x, measure==measure[3]) %>% select(length)
  data4 <- summary.stat %>% filter(tract==x, measure==measure[4]) %>% select(length)
  Rfast::rowVars(data.matrix(cbind(data1, data2, data3, data4)))})

#when calculate variance, only focus on football players 
measure <- c("FA", "MD", "Da", "Dr")
library(Rfast)
#only consider the football players
var.lengthminmaxmed <- t(sapply(tractnames, function(x){
  data <- summary.stat %>% filter(tract==x, player %in% c(group1, group2, group3)) 
  data.l <- data %>% filter(measure==measure[1]) %>% select(length)
  data1 <- data %>% filter(measure==measure[1]) %>% select(min, max, Qu_1st, median, Qu_3rd, Qu_95)
  data2 <- data %>% filter(measure==measure[2]) %>% select(min, max, Qu_1st, median, Qu_3rd, Qu_95)
  data3 <- data %>% filter(measure==measure[3]) %>% select(min, max, Qu_1st, median, Qu_3rd, Qu_95)
  data4 <- data %>% filter(measure==measure[4]) %>% select(min, max, Qu_1st, median, Qu_3rd, Qu_95)
  sqrt(Rfast::colVars(data.matrix(cbind(data.l, data1, data2, data3, data4))))}))
colnames(var.lengthminmaxmed) <- c("length.sd", 
                                   "FA.min.sd", "FA.max.sd", "FA.Qu_1st.sd", "FA.med.sd", "FA.Qu_3rd.sd", "FA.Qu_95.sd",
                                   "MD.min.sd", "MD.max.sd", "MD.Qu_1st.sd", "MD.med.sd", "MD.Qu_3rd.sd", "MD.Qu_95.sd",
                                   "Da.min.sd", "Da.max.sd", "Da.Qu_1st.sd", "Da.med.sd", "Da.Qu_3rd.sd", "Da.Qu_95.sd",
                                   "Dr.min.sd", "Dr.max.sd", "Dr.Qu_1st.sd", "Dr.med.sd", "Dr.Qu_3rd.sd", "Dr.Qu_95.sd")
var.lengthminmaxmed <- round(var.lengthminmaxmed, digits= 3)
var.lengthminmaxmed <- data.frame(var.lengthminmaxmed)
var.lengthminmaxmed$tract <- tractnames
write.csv(var.lengthminmaxmed, file="C:/Users/wma9/Dropbox/football project/fb_summary_of_var.csv")
library(xlsx)
write.xlsx(var.lengthminmaxmed, file="C:/Users/wma9/Dropbox/football project/fb_summary_of_var.xlsx", sheetName = "football", 
           col.names = TRUE, row.names = FALSE, append = FALSE)

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

#subject-level normalization: alldata_qdt
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

### redo the normalization process by using the global min and max within football players ###
select_football <- summary.stat$player %in% c(group1, group2, group3) #sum(select_football) = 9496
summary.stat <- summary.stat[select_football, ] #then summary.stat can be used in the persub_qltg function


footballsid <- subname %in% c(group1, group2, group3)
footballname <- subname[footballsid]
footballloc <- which(footballsid==TRUE)

summary_arlDr <- sapply(footballloc, function(x){
  sub.index <- x 
  subi_obs <- fread(fulldir[x])
  tract.j <- subi_obs %>% filter(Tract=="ar_l") %>% select(-Tract, "Dr")
  c(min(tract.j), max(tract.j))
})
arlDr_group <- c()
for(i in footballloc){
  if(subname[i] %in% group1){arlDr_group <- c(arlDr_group, 1)}else{
    if(subname[i] %in% group2){arlDr_group <- c(arlDr_group, 2)}else{
      arlDr_group <- c(arlDr_group, 3)
    }
  }
}

min(summary_arlDr[1,])
max(summary_arlDr[2,])
persub_arlDr <- sapply(footballloc, function(x){
  #sub.index: 1~195
  #measure:any measurement subset from c("FA", "MD", "Da", "Dr")
  sub.index <- x 
  sub_dir <- fulldir[sub.index]
  sub.i <- read.csv(sub_dir)
  sub.i.measure <- sub.i %>% select(Tract,"Dr") %>% filter(Tract=="ar_l")
  # den.est is a list of length(tracts in sub.index), each element is a matrix of length(measure)x203
  norm_subi <- (sub.i.measure$Dr - 0.0070379)/(2037.932 - 0.0070379)
  den.col <- density(norm_subi, n=length(norm_subi), na.rm=TRUE)
  interpl <- approx(den.col$x, den.col$y, den.quantiles/100, rule=2)
  interpl$y
}) 



# using global min-max: alldata_qdtg
persub_qdtg <- function(sub.index, measure){
  #sub.index: in this function, the sub.index should only be
  #measure:any measurement subset from c("FA", "MD", "Da", "Dr")
  sub_dir <- fulldir[sub.index]
  sub.i <- fread(sub_dir)
  sub.i.measure <- sub.i %>% select(Tract, one_of(measure)) 
  # den.est is a list of length(tracts in sub.index), each element is a matrix of length(measure)x203
  tractnames <- unique(sub.i.measure$Tract)
  den.est <- lapply(tractnames, function(x){
    tract.j <- sub.i.measure %>% filter(Tract==x) %>% select(-Tract)
    minmax <- sapply(measure, function(i){
      minmax_i <- summary.stat %>% filter(tract==x, measure==i) %>% select(player, min, max)
      min_xi <- min(minmax_i$min)
      max_xi <- max(minmax_i$max)
      c(min_xi, max_xi)
    })
    
    den <- sapply(1:4, function(k){
      tract.jm <- tract.j[,k]
      min_k <- minmax[1,k]
      max_k <- minmax[2,k]
      
      tract.jm.Nor <- (tract.jm-min_k)/(max_k-min_k)
      q <- quantile(tract.jm.Nor, den.quantiles/100)
      den.col <- density(tract.jm.Nor, n=length(tract.jm.Nor), na.rm=TRUE, from=0, to=1)
      qd <- sapply(q, function(l){
        interpl <- approx(den.col$x, den.col$y, l, rule=2)
        -log(interpl$y)
      })
      qd
    })
    
    den.est.ij <- data.frame(player=rep(subname[sub.index], length(measure)), tract=rep(x, length(measure)), measure = measure, qdtg.ij=t(den))
    den.est.ij})
  
  persub_data <- do.call(rbind,den.est) #persub_data: player, tract, measure, density1~200
  rownames(persub_data) <- NULL
  return(persub_data)
} 

# using only new group 1 (seperately de-missing: 39) and group 2(seperately de-missing: 34), save as alldata_qdtg3
persub_qdtgnew <- function(sub.index, measure){
  #sub.index: in this function, the sub.index should only be
  #measure:any measurement subset from c("FA", "MD", "Da", "Dr")
  sub_dir <- fulldir[sub.index]
  sub.i <- fread(sub_dir)
  sub.i.measure <- sub.i %>% select(Tract, one_of(measure)) 
  # den.est is a list of length(tracts in sub.index), each element is a matrix of length(measure)x203
  tractnames <- unique(sub.i.measure$Tract)
  den.est <- lapply(tractnames, function(x){
    tract.j <- sub.i.measure %>% filter(Tract==x) %>% select(-Tract)
    minmax <- sapply(measure, function(i){
      minmax_i <- summary.stat %>% filter(tract==x, measure==i, player %in% c(group1, group2)) %>% select(player, min, max)
      min_xi <- min(minmax_i$min)
      max_xi <- max(minmax_i$max)
      c(min_xi, max_xi)
    })
    
    den <- sapply(1:4, function(k){
      tract.jm <- tract.j[,k]
      min_k <- minmax[1,k]
      max_k <- minmax[2,k]
      
      tract.jm.Nor <- (tract.jm-min_k)/(max_k-min_k)
      q <- quantile(tract.jm.Nor, den.quantiles/100)
      den.col <- density(tract.jm.Nor, n=length(tract.jm.Nor), na.rm=TRUE, from=0, to=1)
      qd <- sapply(q, function(l){
        interpl <- approx(den.col$x, den.col$y, l, rule=2)
        -log(interpl$y)
      })
      qd
    })
    
    den.est.ij <- data.frame(player=rep(subname[sub.index], length(measure)), tract=rep(x, length(measure)), measure = measure, qdtg.ij=t(den))
    den.est.ij})
  
  persub_data <- do.call(rbind,den.est) #persub_data: player, tract, measure, density1~200
  rownames(persub_data) <- NULL
  return(persub_data)
} 
#alldata <- lapply(1:length(fulldir), function(x)persub(x, c("FA", "MD", "Da", "Dr")))
#combine.data <- do.call(rbind, alldata)



# using only new group 1 (seperately de-missing: 39) and group 2(seperately de-missing: 34), save as alldata_qdtg4, should be the same as alldata_qdtg3
# use the summary.table.newfb: 108x11
persub_qdtgnew <- function(sub.index, measure){
  #sub.index: in this function, the sub.index should only be
  #measure:any measurement subset from c("FA", "MD", "Da", "Dr")
  sub_dir <- fulldir[sub.index]
  sub.i <- fread(sub_dir)
  sub.i.measure <- sub.i %>% select(Tract, one_of(measure)) 
  # den.est is a list of length(tracts in sub.index), each element is a matrix of length(measure)x203
  tractnames <- unique(sub.i.measure$Tract)
  den.est <- lapply(tractnames, function(x){
    tract.j <- sub.i.measure %>% filter(Tract==x) %>% select(-Tract)
    minmax <- sapply(measure, function(i){
      minmax_i <- summary.table.newfb %>% filter(tract==x, measure==i) %>% select(Min., Max.)
      min_xi <- minmax_i$Min.
      max_xi <- minmax_i$Max.
      c(min_xi, max_xi)
    })
    
    den <- sapply(1:4, function(k){
      tract.jm <- tract.j[,k]
      min_k <- minmax[1,k]
      max_k <- minmax[2,k]
      
      tract.jm.Nor <- (tract.jm-min_k)/(max_k-min_k)
      q <- quantile(tract.jm.Nor, den.quantiles/100)
      den.col <- density(tract.jm.Nor, n=length(tract.jm.Nor), na.rm=TRUE, from=0, to=1)
      qd <- sapply(q, function(l){
        interpl <- approx(den.col$x, den.col$y, l, rule=2)
        -log(interpl$y)
      })
      qd
    })
    
    den.est.ij <- data.frame(player=rep(subname[sub.index], length(measure)), tract=rep(x, length(measure)), measure = measure, qdtg.ij=t(den))
    den.est.ij})
  
  persub_data <- do.call(rbind,den.est) #persub_data: player, tract, measure, density1~200
  rownames(persub_data) <- NULL
  return(persub_data)
} 



# using only new group 1 (seperately de-missing: 39) and group 2(seperately de-missing: 34), save as alldata_qdtg5, should be the same as alldata_qdtg3
# use the summary.table.newfb: 108x11
# greater than 99.5%, then be 1; less than .5%, then be 0
persub_qdtgnew <- function(sub.index, measure){
  #sub.index: in this function, the sub.index should only be
  #measure:any measurement subset from c("FA", "MD", "Da", "Dr")
  sub_dir <- fulldir[sub.index]
  sub.i <- fread(sub_dir)
  sub.i.measure <- sub.i %>% select(Tract, one_of(measure)) 
  # den.est is a list of length(tracts in sub.index), each element is a matrix of length(measure)x203
  tractnames <- unique(sub.i.measure$Tract)
  den.est <- lapply(tractnames, function(x){
    tract.j <- sub.i.measure %>% filter(Tract==x) %>% select(-Tract)
    minmax <- sapply(measure, function(i){
      minmax_i <- summary.table.newfb %>% filter(tract==x, measure==i) %>% select(X0.5., X99.5.)
      min_xi <- minmax_i$X0.5.
      max_xi <- minmax_i$X99.5.
      c(min_xi, max_xi)
    })
    
    den <- sapply(1:4, function(k){
      tract.jm <- tract.j[,k]
      min_k <- minmax[1,k]
      max_k <- minmax[2,k]
      
      tract.jm.Nor <- (tract.jm-min_k)/(max_k-min_k)
      tract.jm.Nor <- ifelse(tract.jm.Nor>1, 1, ifelse(tract.jm.Nor<0, 0, tract.jm.Nor))
      q <- quantile(tract.jm.Nor, den.quantiles/100)
      den.col <- density(tract.jm.Nor, n=length(tract.jm.Nor), na.rm=TRUE, from=0, to=1)
      qd <- sapply(q, function(l){
        interpl <- approx(den.col$x, den.col$y, l, rule=2)
        -log(interpl$y)
      })
      qd
    })
    
    den.est.ij <- data.frame(player=rep(subname[sub.index], length(measure)), tract=rep(x, length(measure)), measure = measure, qdtg.ij=t(den))
    den.est.ij})
  
  persub_data <- do.call(rbind,den.est) #persub_data: player, tract, measure, density1~200
  rownames(persub_data) <- NULL
  return(persub_data)
} 

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

alldata_qdtg <- lapply(footballloc, function(x)persub_qdtg(x, c("FA", "MD", "Da", "Dr")))
alldata_qdtg <- do.call(rbind, alldata_qdtg)
save(alldata_qdtg, file="/Users/mawanying/Dropbox/football project/alldata_qdtg2.Rdata") #alldata_qdtg2 only contains the 88 footballs

alldata_qdtg <- lapply(1:length(fulldir), function(x)persub_qdtgnew(x, c("FA", "MD", "Da", "Dr")))
alldata_qdtg <- do.call(rbind, alldata_qdtg)
save(alldata_qdtg, file="C:/Users/wma9/Dropbox/football project/alldata_qdtg3.Rdata") #alldata_qdtg3 21052, but normalized based on 73 group 1 and group 2

alldata_qdtg <- lapply(newfb.loc, function(x)persub_qdtgnew(x, c("FA", "MD", "Da", "Dr")))
alldata_qdtg <- do.call(rbind, alldata_qdtg)
save(alldata_qdtg, file="//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/fromdropbox/alldata_qdtg4.Rdata") #alldata_qdtg4 7876, but normalized based on 73 group 1 and group 2 max and min

alldata_qdtg <- lapply(newfb.loc, function(x)persub_qdtgnew(x, c("FA", "MD", "Da", "Dr")))
alldata_qdtg <- do.call(rbind, alldata_qdtg)
save(alldata_qdtg, file="//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/fromdropbox/alldata_qdtg5.Rdata") #alldata_qdtg5 7876, but normalized based on 73 group 1 and group 2 .5/100 and 99.5/100


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
#save(alldata_qdtg, file="/Users/mawanying/Dropbox/Luo_Jarek_YC/alldata_qdtg.Rdata")
save(alldata_qdtg, file="C:/Users/wma9/Dropbox/football project/alldata_qdtg1.Rdata") #use 88football max min to construct 

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


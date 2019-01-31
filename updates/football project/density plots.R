library(dplyr)
library(data.table)

files <- list.files(path="C:/Users/wma9/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)
fulldir <- list.files(path="C:/Users/wma9/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=TRUE, recursive=FALSE)
tractnames = c("ar_l",  "ar_r",  "atr_l", "atr_r", "cgc_l", "cgc_r",
               "cgh_l", "cgh_r", "cst_l", "cst_r", "fma",   
               "fmi", "ifo_l", "ifo_r", "ilf_l", "ilf_r", "mcp",  
               "ml_l",  "ml_r",  "ptr_l", "ptr_r", "slf_l", "slf_r", 
               "str_l" ,"str_r", "unc_l", "unc_r")
# there are 195 players
subname <- gsub(pattern=".csv", replacement="", files)


load("C:/Users/wma9/Dropbox/football project/summary.stat.Rdata") # 21052 x 6

#check for voxel measurement that if is in the order of "Tract","FA", "MD","Da", "Dr", namecheck is 195x5
nameCheck <- c()
for(i in 1:195){
  sub_dir <- fulldir[i]
  sub.i <- fread(sub_dir)
  nameCheck <- rbind(nameCheck, colnames(sub.i) == c("Tract","FA", "MD","Da", "Dr"))
}
#check if each subject has 27 tracts: tractCheck is 195x27
#lack of tract:  46th subject does not has the scan for the 18th and 19th tracts: "ml_l" and "ml_r"
#     row col
#[1,]  46  18
#[2,]  46  19

tractCheck <- c()
for(i in 1:195){
  sub_dir <- fulldir[i]
  sub.i <- fread(sub_dir)
  tractCheck <- rbind(tractCheck,  c("ar_l",  "ar_r",  "atr_l", "atr_r", "cgc_l", "cgc_r",
                                     "cgh_l", "cgh_r", "cst_l", "cst_r", "fma",   
                                     "fmi", "ifo_l", "ifo_r", "ilf_l", "ilf_r", "mcp",  
                                     "ml_l",  "ml_r",  "ptr_l", "ptr_r", "slf_l", "slf_r", 
                                     "str_l" ,"str_r", "unc_l", "unc_r") %in% unique(sub.i$Tract))
}
lack_of_tract.loc <- which(tractCheck==FALSE, arr.ind = T)

#add group information
player.info <- read.csv("C:/Users/wma9/Dropbox/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9
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
##############      group1: SUBJECTSTUDYNUM                 ############
##############      group2: CONTACTSPORTQGID                ############
##############      group3: NONCONTACTSPORTQGID             ############
#############################################################################
group1 <- unique(as.character(football.noDup$SUBJECTSTUDYNUM))     #SUBJECTSTUDYNUM 47x1
group2 <- unique(as.character(football.noDup$CONTACTSPORTQGID))    #CONTACTSPORTQGID 47x1
group3 <- unique(as.character(football.noDup$NONCONTACTSPORTQGID)) #NONCONTACTSPORTQGID 47x1


#calculate the raw density for all the subjects per measure per tract
# raw.density: 21052x517, player, tract, measure, den1:den512, x.min, x.max
persub_raw <- function(sub.index){
  measure <- c("FA","MD","Da","Dr")
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
      den.col <- density(x, na.rm=TRUE)
      c(den.col$x, den.col$y, min(den.col$x), max(den.col$x))
      #den:1026xlength(measure)
    })
    den.est.ij <- data.frame(player=rep(subname[sub.index], length(measure)), tract=rep(x, length(measure)), measure = measure, 
                             denX=t(den)[,1:512], denY=t(den)[,513:1024], 
                             x.min=t(den)[,1025], x.max=t(den)[,1026])
    den.est.ij})
  persub_data <- do.call(rbind,den.est) #persub_data: player, tract, measure, density1~200
  #rownames(persub_data) <- NULL
  return(persub_data)
}
raw.density.list <- lapply(1:195, persub_raw)
raw.density <- do.call(rbind, raw.density.list)
save(raw.density, file="C:/Users/wma9/Dropbox/football project/rawdensity.Rdata")
load("C:/Users/wma9/Dropbox/football project/rawdensity.Rdata")

# plot the density plot for 88 football players
para <- expand.grid(tractnames, c("FA","MD","Da","Dr"))
for(i in 1:dim(para)[1]){
  m <- para[i,2]
  t <- para[i,1]
  cat(i, ": measure:", as.character(m), "tract:", as.character(t),"\n")
  den.mt <- raw.density %>% filter(measure==m, tract==t, player %in% c(group1, group2, group3)) #88x1029
  denx.mt <- den.mt %>% select(x.min, x.max) #88x2
  denx.mt.range <- c(min(denx.mt$x.min), max(denx.mt$x.max))
  deny.mt.range <- c(0, max(den.mt %>% select(denY.1:denY.512), na.rm=T))
  den.mt.ppl <- den.mt$player
  figurepath <- paste0("//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/footplayer/1.17/density plot/", m,
                       "/", m, "+", t, "+football+density.png")
  png(figurepath)
  for(sub.index in 1:length(den.mt.ppl)){
    pplname <- den.mt.ppl[sub.index]
    sub.i <- den.mt %>% filter(player==pplname) 
    sub.i.x <- as.numeric(sub.i %>% select(denX.1:denX.512))
    sub.i.y <- as.numeric(sub.i %>% select(denY.1:denY.512))
    if(pplname %in% group1){
      color <- "red"
    }else{
      if(pplname %in% group2){color <- "green"}else{
        if(pplname %in% group3){color <- "blue"}
      }
    }
    if(sub.index==1){
      plot(sub.i.x, sub.i.y, type="l", xlab="x", ylab="density: f(x)", main=paste0("Raw density(Without normalization)\n measure:", m, ", tract:", t,"\n88 football players"), 
           xlim=denx.mt.range,
           ylim=deny.mt.range,
           col=color) 
    }else{
      lines(sub.i.x, sub.i.y, col=color) 
    }
  }
  legend("topright",c("fb group1", "fb group2", "fb group3"), 
         col=c("red","green","blue"), lty=c(1,1,1), bty='n')
  dev.off()
}

# plot the quantile plot for 88 football players
load("C:/Users/wma9/Dropbox/Luo_Jarek_YC/alldata_q.Rdata")
for(i in 1:dim(para)[1]){
  m <- para[i,2]
  t <- para[i,1]
  cat(i, "quantile: measure:", as.character(m), "tract:", as.character(t),"\n")
  den.mt <- alldata_q %>% filter(measure==m, tract==t, player %in% c(group1, group2, group3)) #88x203
  denx.mt <- den.quantiles/100
  quantiles.mt <- den.mt %>% select(quan.ij.0.25.:quan.ij.99.75.)   #88x200
  denx.mt.range <- c(0,1)
  deny.mt.range <- c(min(quantiles.mt),max(quantiles.mt))
  den.mt.ppl <- den.mt$player
  figurepath <- paste0("//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/footplayer/1.17/quantile plot/", m,
                       "/", m, "+", t, "+football+quantile.png")
  png(figurepath)
  for(sub.index in 1:length(den.mt.ppl)){
    pplname <- den.mt.ppl[sub.index]
    sub.i <- den.mt %>% filter(player==pplname) 
    sub.i.x <- denx.mt
    sub.i.y <- as.numeric(sub.i %>% select(quan.ij.0.25.:quan.ij.99.75.))
    if(pplname %in% group1){
      color <- "red"
    }else{
      if(pplname %in% group2){color <- "green"}else{
        if(pplname %in% group3){color <- "blue"}
      }
    }
    if(sub.index==1){
      plot(sub.i.x, sub.i.y, type="l", xlab="t", ylab="quantile: q(t)", main=paste0("Quantile plot (Without normalization)\n measure:", m, ", tract:", t,"\n88 football players"), 
           xlim=denx.mt.range,
           ylim=deny.mt.range,
           col=color) 
    }else{
      lines(sub.i.x, sub.i.y, col=color) 
    }
  }
  legend("topleft",c("fb group1", "fb group2", "fb group3"), 
         col=c("red","green","blue"), lty=c(1,1,1), bty='n')
  dev.off()
}


# plot the log-quantile trasnformation plot for 88 football players
load("C:/Users/wma9/Dropbox/Luo_Jarek_YC/alldata_qdt.Rdata")
for(i in 1:dim(para)[1]){
  m <- para[i,2]
  t <- para[i,1]
  cat(i, "qdt: measure:", as.character(m), "tract:", as.character(t),"\n")
  den.mt <- alldata_qdt %>% filter(measure==m, tract==t, player %in% c(group1, group2, group3)) #88x203
  denx.mt <- den.quantiles/100
  quantiles.mt <- den.mt %>% select(qdt.ij.0.25.:qdt.ij.99.75.)   #88x200
  denx.mt.range <- c(0,1)
  deny.mt.range <- c(min(quantiles.mt),max(quantiles.mt))
  den.mt.ppl <- den.mt$player
  figurepath <- paste0("//wolftech.ad.ncsu.edu/cos/stat/Redirect/wma9/Desktop/footplayer/1.17/log quantile plot (subject-specific)/", m,
                       "/", m, "+", t, "+football+qdt.png")
  png(figurepath)
  for(sub.index in 1:length(den.mt.ppl)){
    pplname <- den.mt.ppl[sub.index]
    sub.i <- den.mt %>% filter(player==pplname) 
    sub.i.x <- denx.mt
    sub.i.y <- as.numeric(sub.i %>% select(qdt.ij.0.25.:qdt.ij.99.75.))
    if(pplname %in% group1){
      color <- "red"
    }else{
      if(pplname %in% group2){color <- "green"}else{
        if(pplname %in% group3){color <- "blue"}
      }
    }
    if(sub.index==1){
      plot(sub.i.x, sub.i.y, type="l", xlab="t", ylab="log-quantile: -log(q(t))", main=paste0("log quantile transformation plot (Subject-specific normalization)\n measure:",
                                                                                              m, ", tract:", t,
                                                                                              "\n88 football players"), 
           xlim=denx.mt.range,
           ylim=deny.mt.range,
           col=color) 
    }else{
      lines(sub.i.x, sub.i.y, col=color) 
    }
  }
  legend("topleft",c("fb group1", "fb group2", "fb group3"), 
         col=c("red","green","blue"), lty=c(1,1,1), bty='n')
  dev.off()
}





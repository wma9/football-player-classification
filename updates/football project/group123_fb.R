library(dplyr)
player.info <- read.csv("C:/Users/wma9/Dropbox/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9
files <- list.files(path="C:/Users/wma9/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)
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
###################################################################################################################################################
# considering fb only
# and consideritng all the trt player which have no controls
# disregard the repeated 8 football players in both group1 and group2, we have the following number of players who are among the 195 DTI scannings
# group 1: 39 
# group 2: 34
# group 3: 34
###################################################################################################################################################



player.info <- read.csv("C:/Users/wma9/Dropbox/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9
files <- list.files(path="C:/Users/wma9/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)
subname <- gsub(pattern=".csv", replacement="", files)
player.info <- player.info %>% filter(INJURYSPORTVARSITY=='Football')

unique(player.info$SUBJECTSTUDYNUM[as.character(player.info$CONTACTSPORTQGID)==""]) #16 group 1 fb players who do not have corresponding group 2 information
sum(unique(player.info$SUBJECTSTUDYNUM[as.character(player.info$CONTACTSPORTQGID)==""]) %in% subname) # 10 of these are in the 195 scans
unique(player.info$SUBJECTSTUDYNUM[as.character(player.info$CONTACTSPORTQGID)=="" | as.character(player.info$NONCONTACTSPORTQGID)==""]) # 20 group 1 players who do not have corresponding group 2 or 3 information
sum(unique(player.info$SUBJECTSTUDYNUM[as.character(player.info$CONTACTSPORTQGID)=="" |as.character(player.info$NONCONTACTSPORTQGID)==""]) %in% subname) # 11 of these are in the 195 scans
unique(player.info$SUBJECTSTUDYNUM[as.character(player.info$NONCONTACTSPORTQGID)==""]) #17 trt group players who do not have corresponding group 3

###################################################################################################################################################
## considering group 1 and group 2, group 3, only football players
## group 1: 39 
## group 2: 34
## group 3: 34
## delete observations that does not have group 2(only considering group1 and group2)
## group 1: 29
## group 2: 32
## delete also observations that does not have group2 or group3
## group 1: 28
## group 2: 31
## group 3: 29
###################################################################################################################################################



player.info <- read.csv("C:/Users/wma9/Dropbox/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9
files <- list.files(path="C:/Users/wma9/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)
subname <- gsub(pattern=".csv", replacement="", files)
player.info <- player.info %>% filter(INJURYSPORTVARSITY=='Football')
player.info <- player.info[as.character(player.info$CONTACTSPORTQGID)!="", ] #115=136-21


group1 <- as.character(player.info$SUBJECTSTUDYNUM)  #72 # there is no empty in group 1
group2 <- as.character(player.info$CONTACTSPORTQGID) #72


d1 <- group1 %in% group2 #8 there are 8 football players that occurs in both group 1 and group 2
sum(d1)
d2 <- group2 %in% group1 #7
sum(d2)
d <- cbind(d1, d2) #72

group1 <- unique(group1[rowSums(d)<1]) #47
group2 <- unique(group2[rowSums(d)<1]) #47

sum(group1 %in% subname) #29
sum(group2 %in% subname) #32

#############################################################################
player.info <- read.csv("C:/Users/wma9/Dropbox/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9
files <- list.files(path="C:/Users/wma9/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)
subname <- gsub(pattern=".csv", replacement="", files)
player.info <- player.info %>% filter(INJURYSPORTVARSITY=='Football')
player.info <- player.info[(as.character(player.info$CONTACTSPORTQGID)!="") & (as.character(player.info$NONCONTACTSPORTQGID)!=""), ] #67

group1 <- as.character(player.info$SUBJECTSTUDYNUM)  #67 # there is no empty in group 1
group2 <- as.character(player.info$CONTACTSPORTQGID) #67
group3 <- as.character(player.info$NONCONTACTSPORTQGID) #67

d1 <- group1 %in% group2 #5 there are 5 football players that occurs in both group 1 and group 2
sum(d1)
d2 <- group1 %in% group3 #0
sum(d2)
d3 <- group2 %in% group1 #5 there are 5 football players that occurs in both group 1 and group 2
sum(d3)
d4 <- group2 %in% group3 #0
sum(d4)
d5 <- group3 %in% group1 #0
sum(d5)
d6 <- group3 %in% group2 #0
sum(d6)

d <- cbind(d1, d2, d3, d4, d5, d6) #67x6

group1 <- unique(group1[rowSums(d)<1]) #47
group2 <- unique(group2[rowSums(d)<1]) #47
group3 <- unique(group3[rowSums(d)<1]) #47

sum(group1 %in% subname) #28
sum(group2 %in% subname) #31
sum(group3 %in% subname) #29



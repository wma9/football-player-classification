player.info <- read.csv("C:/Users/wma9/Dropbox/Luo_Jarek_YC/MRI_Injury_Description_03OCT18_All_Concussions_ForLuo.csv") #player.info is 136x9
files <- list.files(path="C:/Users/wma9/Dropbox/Luo_Jarek_YC/csv_voxelwise", pattern="*.csv", full.names=FALSE, recursive=FALSE)
subname <- gsub(pattern=".csv", replacement="", files)

group1 <- as.character(unique(player.info$SUBJECTSTUDYNUM)) #118 # there is no empty in group1
group1 <- group1[group1!=''] #118
group2 <- as.character(unique(player.info$CONTACTSPORTQGID)) #100
group2 <- group2[group2!=''] #99
group3 <- as.character(unique(player.info$NONCONTACTSPORTQGID)) #98
group3 <- group3[group3!=''] #97

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

group1[which(d1==TRUE)]
group2[which(d3==TRUE)]

sum(group1 %in% subname) #67 - 8
sum(group2 %in% subname) #64 - 8 
sum(group3 %in% subname) #60
###################################################################################################################################################
# considering all the 4 types of players, fb, soccer, lacrosse, ice hockey
# and consideritng all the trt player which have no controls
# disregard the repeated 8 football players in both group1 and group2, we have the following number of players who are among the 195 DTI scannings
# group 1: 59 
# group 2: 56
# group 3: 60
###################################################################################################################################################

as.character(unique(player.info$SUBJECTSTUDYNUM[player.info$CONTACTSPORTQGID=='' | player.info$NONCONTACTSPORTQGID=='']))

### if only focus on 
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
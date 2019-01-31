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

twocluster <- function(alldata, iter){
  # alldata is the dataset merged with football information: alldata.fb_q
  # iter: 1~108
  m <- para[iter, 2]
  t <- para[iter, 1]
  alldata_mt <- alldata  %>% filter(measure==as.character(m), tract==as.character(t), group!=3) %>% select(group, den1:den200)
  group_mt <- alldata_mt$group
  alldata_mt <- select(alldata_mt, -group)
  
  calcu_misrate <- lapply(1:200, function(i){
    x <- alldata_mt[,i]
    den.cluster <- kmeans(x, centers = 2)
    perm1 <- den.cluster$cluster
    perm2 <- permute(den.cluster$cluster, 2,1, NA)
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

result_2cl <- lapply(1:108, function(x){twocluster(alldata.fb_qdtg, x)})
misrate2 <- sapply(result_2cl, function(x){
  sapply(x, function(i){i$misrate})
})

mt_select2 <- which(apply(misrate2, 2, min)==min(misrate2))
q_select2 <- lapply(mt_select2, function(x){
  data_mt <- misrate2[,x]
  q.temp  <- which(data_mt==min(misrate2))
  cbind(rep(x, length(q.temp)), q.temp)
})
select2 <- do.call(rbind, q_select2)
colnames(select2) <- c("mt", "q")

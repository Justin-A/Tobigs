rm(list=ls())

data <- iris[,1:4] # 라벨 제거한 iris 데이터
str(data)


k_means<-function(data,k) 
{ index <- sample(1:nrow(data),k)
  centroid <- data[index,] #dataframe
  combine <- rbind(centroid,data) #dataframe(중심점과 데이터 합친 것)
  while(TRUE){
    dis <- as.matrix(dist(combine)) #유클리드 거리(개체들끼리 거리 구하기)
    dis <- dis[,1:k]
    label <- as.factor(apply(dis,1,which.min)) #dis 중 중심점에서 가장 가까운 것 
    combine <- cbind(combine,label)
    
    center1 <- aggregate(combine[1:(ncol(combine)-1)],by=list(combine$label),mean) #중심점 다시 구하기 
    center2 <- as.integer(as.character(center1[,1])) 
    centroid[center2,] <- as.matrix(center1[,2:ncol(center1)])
    equal <- centroid == combine[1:nrow(centroid),1:ncol(centroid)] #중심점 비교 
    equal <- which(equal==F)
    
    if(TRUE){                                 #중심점이 같다면 
      levels(combine[,ncol(combine)]) <- c(1:k,"point")
      combine[1:k,ncol(combine)] <- "point"
      return(combine)
    }
      combine <- combine[,(ncol(combine)-1)] #중심점이 다르면 
      combine[1:k,] <- centroid
    }
}

k_means(data,5)


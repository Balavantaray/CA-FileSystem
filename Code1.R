#setwd("D:/CA_products/Cluster/Ravi_sir")

dataset=iris
number_cluster_you_need=8

cluster_data_prep<- function(dataset,number_cluster_you_need){
  # number_cluster_you_need=8
  # dataset=data1
  options(warn = -1)
  Program_start_time <- Sys.time()
  #getting required pakages
  list.of.packages <- c("sqldf","dplyr","dummies","e1071","clustMixType","clv","mclust","NbClust", "modeest", "mice", "factoextra", "dbscan", "fpc", "clustMixType")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages())]
  if(length(new.packages)) install.packages(new.packages)
  
  
  ## initiating  library
  library(NbClust)
  library(modeest)
  library(mice)
  #library(factoextra)
  library(dbscan)
  library(fpc)
  library(clustMixType)
  library(mclust)
  library(clv)
  library(clustMixType)
  library(e1071)
  library(dplyr)
  library(sqldf)
  library(dummies)
  library(plyr)
  library(ggplot2)
  
  mainDir=getwd()
  subDir<-paste("cluster_analysis_outputs")
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir,subDir))
  ##########  data inputation #####
  df <- dataset
  nc <- number_cluster_you_need
  
  #### missing value percentage
  miss <- (apply(df,2,function(x) sum(is.na(x)/length(x)*100)))
  miss <- data.frame(miss)
  miss$names <- names(df)
  n <- which(miss$miss>10)
  if(length(n)>=1)
  {
    df <- df[ ,-n]
  }
  
  
  # #taking numerical variable
  num <- df[which(sapply(df,function(df) is.numeric(df)))]
  char <- df[which(sapply(df,function(df) is.character(df)|is.factor(df)))]
  
  ########### missing values
  
  ##for numeric and cat
  
  
  if(sum(is.na(df))>=1)
  {
    cat("missing value treatment")
    tempData <- mice(num,m=5,maxit=50,meth='pmm',seed=500)
    num <- complete(tempData,1)
    ##for CHAR
    
    if(sum(is.na(char))>1)
    {
      if(ncol(char)>1)
      {
        tempData <- mice(char,m=5,maxit=50,meth='polyreg',seed=500)
        char <- complete(tempData,1)
      }
      if(ncol(char)==1)
      {
        c <- names(sort(-table(char)))[1]
        char[is.na(char)] <- c
      }
    }
  }
  
  cat("*************************************************************\n")
  cat("************  Missing value treatment completed  ************\n")
  cat("*************************************************************\n")
  ###########################
  input_data <- cbind(num,char)
  ########getting imp variable in cat list
  df_char <- char
  
  if(length(df_char)>=1)
  {
    op1 <- c()
    for(i in 1:ncol(char))
    {
      per <- data.frame(prop.table(table(char[i]))*100)
      per$cum <- cumsum(per$Freq)
      per$op <- ifelse(per$cum<=95,as.character(per$Var1),"other")
      per$Var1 <- as.character(per$Var1)
      op2 <- per$op[match(char[,i],per$Var1)]
      op1 <- cbind(op1,op2)
    }
    
    char1 <- data.frame(op1)
    names(char1) <- names(char)
    
    # imp <- (apply(char1,2,function(x) length(unique(x))))
    # imp <- data.frame(imp)
    # imp$names <- names(char)
    # n <- which(imp$imp>10)
    # if(length(n)>=1)
    # {
    #   char1 <- char1[ ,-n]
    # }
    
    imp2 <- c()
    for(i in 1:length(char))
    {
      imp1 <- dummy(char1[,i])
      imp2 <- cbind(imp2,imp1)
    }
    imp2 <- data.frame(imp2)
    df_char <- imp2
  }
  ####complete data set
  df_num <- num
  df_full <- cbind(df_num,df_char)
  ####to find number of cluster
  
  sc_df_full <- data.frame(scale(df_full))
  sc_df_num <- data.frame(scale(df_num))
  
  sc_df_num1 <- sc_df_num
  
  
  
  if(nrow(sc_df_num)>60000)
  { 
    
    set.seed(1234)
    sampl <- sample.int(n=nrow(sc_df_num1),size = 0.7*nrow(sc_df_num1))
    train1 <- sc_df_num[sampl,]
    test1 <- sc_df_num[-sampl,]
    sc_df_num1 <- test1
  }
  
  
  
  if(nrow(sc_df_num)>60000)
  {
    sc_df_num1 <- sc_df_num1[1:60000,]
  }
  
  
  cat("****************************************************************************\n")
  cat("*****************Identifying Optimal Number of Cluster**********************\n")
  cat("****************************************************************************\n")
  
  #"duda","pseudot2",
  index1 <- c("ccc","scott","marriot","trcovw","tracew","friedman","rubin","db","beale","ratkowsky","ball","sdindex","dindex","sdbw")
  
  e2 <- c()
  for(i in 1:length(index1))
  {
    x1 <- NbClust(sc_df_num,distance = "euclidean", min.nc=2, max.nc=7,method = "kmeans", index = index1[i])
    e1 <- rbind(x1$Best.nc[1],x1$Best.nc[2])
    e2 <- cbind(e2,e1)
    cat(paste(index1[i], ": # of Cluster =",e1[1],"\n"))
  }
  
  x2 <- data.frame(e2)
  pr <- x2
  x4 <- data.frame(table(t(x2[1,])))
  x4 <- x4[order(-x4$Freq),]
  nc1 <- as.numeric(as.character(x4$Var1[1]))
  
  cat("According Majority Rule, Best # of Cluster = ",nc1,"\n")
  write.csv(sc_df_full,"sc_df_full.csv",row.names = F)
  write.csv(sc_df_num,"sc_df_num.csv",row.names = F)
  clust <- data.frame(best_cluster=nc1,user_cluster=number_cluster_you_need,path=as.character(getwd()))
  write.csv(clust,"clust.csv",row.names = F)
  
  write.csv(dataset,"input.csv",row.names = F)
  
  if(ncol(df_char)==0){df_char <- c()}
  write.csv(df_char,"df_char.csv",row.names = F)
  write.csv(input_data,"input_data.csv",row.names = F)
}

#data1 <- read.csv("oj.csv")
cluster_data_prep(dataset = dataset,number_cluster_you_need = 8)








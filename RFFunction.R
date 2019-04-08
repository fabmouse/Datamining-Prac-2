randomForestTest <-
  function(MODEL,HOLDOUT) {
    y.name <- unlist(strsplit(as.character(MODEL$call),split=" ")[[2]])[1]
    y.pos <- which(names(HOLDOUT)==y.name)

    #Random forest
        L <- levels(HOLDOUT[,2])
        predicteds <- predict(MODEL,newdata=HOLDOUT)
        actuals <- HOLDOUT[,2]
        T<-table(actuals,predicteds,dnn=c())
        accuracy <- sum(diag(T))/(sum(T))
        misclass.holdout <- (T[1,2]+T[1,3]+T[1,4]+T[2,1]+T[2,3]+T[2,4]+T[3,1]+T[3,2]+T[3,4]+T[4,1]+T[4,2]+T[4,3])
        misclass.rate <- sum(misclass.holdout)/sum(T)
        
        T<-rbind(T,apply(T,2,sum))
        T<-cbind(T,apply(T,1,sum))
        colnames(T) <- c(paste(L),"Total")
        rownames(T) <- c(paste(L),"Total")
        
        return(list(CM.holdout=T,misclass.holdout=misclass.holdout,misclass.rate=misclass.rate,accuracy=accuracy)) 
  }

CVgroup <- function(k, dataset, seed){
  datasize <- nrow(dataset)
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k, ceiling(datasize/k))[1:datasize]
  temp <- sample(n, datasize)
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x, function(x) dataseq[temp == x])
  return(cvlist)
}


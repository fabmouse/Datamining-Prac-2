###############################
# FUNCTIONS FOR RANDOM FORESTS#
###############################

randomForestTest <-function(MODEL,HOLDOUT) {
    ## find the number of each rows and columns
    y.name <- unlist(strsplit(as.character(MODEL$call),split=" ")[[2]])[1]
    y.pos <- which(names(HOLDOUT)==y.name)

    #Random forest
    L <- levels(HOLDOUT[,1])
    predicteds <- predict(MODEL,newdata=HOLDOUT)
    actuals <- HOLDOUT[,1]
    T<-table(actuals,predicteds,dnn=c())
    accuracy <- sum(diag(T))/(sum(T))
    total <- sum(T)
    misclass.holdout <- (T[1,2]+T[1,3]+T[1,4]+T[2,1]+T[2,3]+T[2,4]+T[3,1]+T[3,2]+T[3,4]+T[4,1]+T[4,2]+T[4,3])
    misclass.rate <- sum(misclass.holdout)/sum(T)
        
    T<-rbind(T,apply(T,2,sum))
    T<-cbind(T,apply(T,1,sum))
    colnames(T) <- c(paste(L),"Total")
    rownames(T) <- c(paste(L),"Total")
        
    result <- list( 'Total' = total, 'Misclass' = misclass.holdout, 'Accuracy' = accuracy,'Misclass Rate' = misclass.rate)
    tablevalue <- data.frame(do.call(cbind,result))
    return(list('Confusion matrix:' = T, 'Summary:' = tablevalue)) 
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

findgoodmtry <- function(X,Y,N,SEED,DATASET){
    mtrylist <- list()
    errlist <- list()
    set.seed(SEED)
    for (i in 1:N) {
      mtry_fit <- randomForest(X,Y,data = DATASET,mtry = i)
      err <- mean(mtry_fit$err.rate)
      #    print(err)
      mtrylist[i] <- i
      errlist[i] <- err
  }
    result <- list('MTRY Number' = mtrylist, 'OOBErr' = errlist)
    tablevalue <- data.frame(do.call(cbind,result))
  
    mtry <- sort(as.numeric(names(errlist)))
    res <- unlist(errlist[as.character(mtrylist)])
    res <- cbind(mtry=mtrylist, OOBError=errlist)
  
    plot(res, xlab=expression(MTRY), ylab="OOB Error", type="o", log="x",xaxt="n")
    axis(1, at=res[,"mtry"])
  
    return(tablevalue)
}


findgoodntree <- function(X,Y,start,end,by,mtrynumber,DATASET){
    k = (end-start)/by
  
    treeNumber <- list()
    accuracyValue <- list()
    misclassValue <- list()
    errlist <- list()
  
  for (i in 1:(k+1)) {
    numtree = (start-by + i*by)
    model = randomForest(X,Y,data = DATASET,mtry = mtrynumber,ntree = numtree)
    prediction <- predict(model, newdata = DATASET)
    err <- mean(model$err.rate)
    
    actuals <- Y 

    T <- table(actuals, prediction)
    accuracy <- sum(diag(T))/(sum(T))
    #misclass.holdout <- (T[1,2]+T[1,3]+T[1,4]+T[2,1]+T[2,3]+T[2,4]+T[3,1]+T[3,2]+T[3,4]+T[4,1]+T[4,2]+T[4,3])
    misclass.rate <- 1 - accuracy
    #print(T)
#    cat("When the tree number is :", numtree,"\n")
#    cat("The accuracy is :", accuracy,"\n")
#    cat("The misclass.rate is :", misclass.rate,"\n")
#    cat("\n")

#    print(paste("Tree number:", numtree, " Accuracy:",accuracy, " misclass rate:", misclass.rate)) 
#    testtest <- data.frame(treenumber = numtree, Accuracy = accuracy, misclassrate = misclass.rate)
#    add value to list
    treeNumber[i] <- numtree
    accuracyValue[i] <- accuracy
    misclassValue[i] <- misclass.rate
    errlist[i] <- err
  }
    result <- list('Tree Number' = treeNumber, 'Accuarcy' = accuracyValue, 'Misclass Rate' = misclassValue, 'OOBError' = errlist)
    #print(treeNumber)
    #print(accuracyValue)
    #print(misclassValue)
    #print(result)
    do.call(cbind,result)
  
    tablevalue <- data.frame(do.call(cbind,result))
    return(tablevalue)
}





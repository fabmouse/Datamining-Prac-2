randomForestTest <-
  function(MODEL,HOLDOUT) {
    ## find the number of each rows and columns
    y.name <- unlist(strsplit(as.character(MODEL$call),split=" ")[[2]])[1]
    y.pos <- which(names(HOLDOUT)==y.name)

    
    #Random forest
        L <- levels(HOLDOUT[,2])
        predicteds <- predict(MODEL,newdata=HOLDOUT)
        actuals <- HOLDOUT[,2]
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


findgoodntree <- function(X,Y,start,end,by,mtrynumber,DATASET){
  k = (end-start)/by
  
  treeNumber <- list()
  accuracyValue <- list()
  misclassValue <- list()
  
  for (i in 1:(k+1)) {
    numtree = (start-by + i*by)
    model = randomForest(X,Y,data = DATASET,mtry = mtrynumber,ntree = numtree)
    prediction <- predict(model, newdata = holdout)
    actuals <- holdout[,2]
    T<-table(actuals,prediction,dnn=c())
    accuracy <- sum(diag(T))/(sum(T))
    misclass.holdout <- (T[1,2]+T[1,3]+T[1,4]+T[2,1]+T[2,3]+T[2,4]+T[3,1]+T[3,2]+T[3,4]+T[4,1]+T[4,2]+T[4,3])
    misclass.rate <- sum(misclass.holdout)/sum(T)
    #print(T)
#   cat("When the tree number is :", numtree,"\n")
#   cat("The accuracy is :", accuracy,"\n")
#   cat("The misclass.rate is :", misclass.rate,"\n")
#   cat("\n")

#    print(paste("Tree number:", numtree, " Accuracy:",accuracy, " misclass rate:", misclass.rate)) 
#    testtest <- data.frame(treenumber = numtree, Accuracy = accuracy, misclassrate = misclass.rate)
    # add value to list
    treeNumber[i] <- numtree
    accuracyValue[i] <- accuracy
    misclassValue[i] <- misclass.rate
    
  }
  result <- list('Tree Number' = treeNumber, 'Accuarcy' = accuracyValue, 'Misclass Rate' = misclassValue)
  #print(treeNumber)
  #print(accuracyValue)
  #print(misclassValue)
  #print(result)
  do.call(cbind,result)
  
  tablevalue <- data.frame(do.call(cbind,result))
  return(tablevalue)
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
  result <- list('MTRY Number' = mtrylist, 'Err' = errlist)
  tablevalue <- data.frame(do.call(cbind,result))
  return(tablevalue)
}

goodmtry <- function(x, y, mtryStart=if(is.factor(y)) floor(sqrt(ncol(x))) else
  floor(ncol(x)/3), ntreeTry=50, stepFactor=2,
  improve=0.05, trace=TRUE, plot=TRUE, doBest=FALSE, ...) {
  if (improve < 0) stop ("improve must be non-negative.")
  classRF <- is.factor(y)
  errorOld <- if (classRF) {
    randomForest(x, y, mtry=mtryStart, ntree=ntreeTry,
                 keep.forest=FALSE, ...)$err.rate[ntreeTry,1]
  } else {
    randomForest(x, y, mtry=mtryStart, ntree=ntreeTry,
                 keep.forest=FALSE, ...)$mse[ntreeTry]
  }
  if (trace) {
    cat("mtry =", mtryStart, " OOB error =",
        if (classRF) paste(100*round(errorOld, 4), "%", sep="") else
          errorOld, "\n")
  }
  
  oobError <- list()
  oobError[[1]] <- errorOld
  names(oobError)[1] <- mtryStart  
  
  for (direction in c("left", "right")) {
    if (trace) cat("Searching", direction, "...\n")
    Improve <- 1.1*improve
    mtryBest <- mtryStart
    mtryCur <- mtryStart
    while (Improve >= improve) {
      mtryOld <- mtryCur
      mtryCur <- if (direction == "left") {
        max(1, ceiling(mtryCur / stepFactor))
      } else {
        min(ncol(x), floor(mtryCur * stepFactor))
      }
      if (mtryCur == mtryOld) break
      errorCur <- if (classRF) {
        randomForest(x, y, mtry=mtryCur, ntree=ntreeTry,
                     keep.forest=FALSE, ...)$err.rate[ntreeTry,"OOB"]
      } else {
        randomForest(x, y, mtry=mtryCur, ntree=ntreeTry,
                     keep.forest=FALSE, ...)$mse[ntreeTry]
      }
      if (trace) {
        cat("mtry =",mtryCur, "\tOOB error =",
            if (classRF) paste(100*round(errorCur, 4), "%", sep="") else
              errorCur, "\n")
      }
      oobError[[as.character(mtryCur)]] <- errorCur
      Improve <- 1 - errorCur/errorOld
      cat(Improve, improve, "\n")
      if (Improve > improve) {
        errorOld <- errorCur
        mtryBest <- mtryCur
      }
    }
  }
  mtry <- sort(as.numeric(names(oobError)))
  res <- unlist(oobError[as.character(mtry)])
  res <- cbind(mtry=mtry, OOBError=res)
  
  if (plot) {
    plot(res, xlab=expression(m[try]), ylab="OOB Error", type="o", log="x",
         xaxt="n")
    axis(1, at=res[,"mtry"])
  }
  
  if (doBest) 
    res <- randomForest(x, y, mtry=res[which.min(res[,2]), 1], ...)
  
  res
}




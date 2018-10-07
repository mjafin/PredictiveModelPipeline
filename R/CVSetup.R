source("quantilenorm.R")
CVSetup = function(Internal,mySettings,Xtrain,ytrain){
  'Do some setup and return PrepareDataInfo
  '
  PrepareDataInfo = list()
  sett = mySettings$preProcessing
  timeString = format(Sys.time(), "%Y%m%d%H%M")
  N = dim(Xtrain)[1] # number of samples
  # check missing values
  if(any(is.na(Xtrain))){
    cat("Missing values detected in Xtrain. Imputing by median of values present in the sample(s).")
    tempf=function(x){
      if(any(is.na(x)))
        x[is.na(x)]=median(x, na.rm=TRUE)
      return(x)
    }
    Xtrain=t(apply(Xtrain,1,tempf)) # transpose required because apply sucks
  }
  if(any(is.na(ytrain))){
    stop("ytrain must not have any missing values.")
  }
  
  #pre process full data and store
  preProcout = preProc(settings=sett,Xtrain=Xtrain)
  Xtrain.norm = preProcout$xouttrain
  PrepareDataInfo$PreProcModel = preProcout$Model # data.frame with as many entries as there are steps
  rm(preProcout)
  PrepareDataInfo$fulltraindatafilename = file.path(mySettings$directory$intermediate,
  sprintf("%s_FullData_%s.Rdata", mySettings$projectname,timeString))
  save(Xtrain.norm, ytrain, file = PrepareDataInfo$fulltraindatafilename)
  # CV enabled?
  if(sett$crossValidation$CVEnable){
    if(!require("cvTools")){
      stop("Package cvTools is a requirement to run this pipeline. Please install yaml.")
    }
    if(sett$crossValidation$LOOCV){
      CVRepeats=1
      CVFolds=N
    }
    else{ # not LOOCV
      CVRepeats=sett$crossValidation$CVRepeats
      CVFolds=sett$crossValidation$CVFolds
    }
    # CHANGE drawing of folds into stratified:
    set.seed(sett$crossValidation$randomSeed)
    cvfolds = cvFolds(n=N, K = CVFolds, R = CVRepeats)
    #print(cvfolds)
    PrepareDataInfo$cvfoldfilenames = data.frame(matrix(NA, nrow = CVRepeats, ncol = CVFolds))
    PrepareDataInfo$cvInfo = cvfolds # store for later use
    
    # loop through repeats and folds
    for (iii in 1:cvfolds$R){ # repeats
      for(jjj in 1:cvfolds$K){ # folds
        trainInds = cvfolds$subsets[cvfolds$which!=jjj,iii]
        testInds = cvfolds$subsets[cvfolds$which==jjj,iii]
        CVX.train = Xtrain[trainInds,,drop=F]
        CVX.test = Xtrain[testInds,,drop=F]
        if(is.matrix(ytrain)){
          CVy.train = ytrain[trainInds,,drop=F]
          CVy.test = ytrain[testInds,,drop=F]
        }
        else{ 
          CVy.train = ytrain[trainInds]
          CVy.test = ytrain[testInds]
        }

        ## pre-process X
        normTemp = preProc(CVX.train,settings=sett,Xtest=CVX.test)
        CVX.train.norm = normTemp$xouttrain
        CVX.test.norm = normTemp$xouttest
        PrepareDataInfo$cvfoldfilenames[iii,jjj] = file.path(mySettings$directory$intermediate,
                                                             sprintf("%s_CVRepeat%dFold%d_%s.Rdata",mySettings$projectname,iii,jjj,timeString))
        save(CVX.train.norm, 
                  CVX.test.norm, 
                  CVy.train, 
                  CVy.test, 
                  trainInds, 
                  testInds, 
             file = PrepareDataInfo$cvfoldfilenames[iii,jjj])
      }
    } 
  }
  return(PrepareDataInfo)
}
    
preProc=function(settings,Xtrain=NULL,Xtest=NULL,Model=NULL){
  output=list()
  # if normalisation etc. steps disabled: 
  if(is.null(settings$preProcSteps)||length(settings$preProcSteps)<1){
    output$xouttrain=Xtrain
    output$xouttest=Xtest
    return(output)
  }
  NumPreProcSteps=length(settings$preProcSteps)
  NewModel=data.frame(matrix(nrow=1,ncol=NumPreProcSteps)) # temporary storage for preprocessing models
  # otherwise, loop through preProcSteps
  for(ii in 1:NumPreProcSteps){
    SubModel=Model[[ii]] # extract preprocessing model for this step (possibly NULL)
    funcHandle=get( paste(tolower(settings$preProcSteps[[ii]]$method),"Func",sep="") )
    outtemp=funcHandle(settings$preProcSteps[[ii]],Xtrain,Xtest,SubModel)
    Xtrain=outtemp$xouttrain # update Xtrain
    Xtest=outtemp$xouttest # update Xtest
    NewModel[[ii]]=outtemp$Model
    rm(outtemp)
  }
  output=list(xouttrain=Xtrain,xouttest=Xtest,Model=NewModel)
  return(output)
}

logFunc=function(stepSettings,Xtrain=NULL,Xtest=NULL,Model=NULL){
  myBase=stepSettings$value
  if(!is.null(Xtrain))
    Xtrain=log(Xtrain,myBase)
  if(!is.null(Xtest))
    Xtest=log(Xtest,myBase)
  output=list(xouttrain=Xtrain,xouttest=Xtest,Model=NA)
  return(output)
}

quantileFunc=function(stepSettings,Xtrain=NULL,Xtest=NULL,Model=NULL){
  output=list(xouttrain=NULL,xouttest=NULL,Model=NULL)
  if(!is.null(Model)){
    quantiles=Model$quantiles
    if(!is.null(Xtrain)){
      temp=quantilenorm(Xtrain,method="quant", refquant=quantiles)
      output$xouttrain=temp$xout
      rm(temp)
    }
  }else{
    temp=quantilenorm(Xtrain,method="quant", quantprob=stepSettings$value)
    quantiles=temp$quantiles
    output$xouttrain=temp$xout
    rm(temp)
  }
  if(!is.null(Xtest)){
    temp2=quantilenorm(Xtest,method="quant", refquant=quantiles)
    output$xouttest=temp2$xout
  }
  output$Model$quantiles = quantiles
  return(output)
}

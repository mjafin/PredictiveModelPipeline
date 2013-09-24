# internal functions
MLRun = function(Internal,mySettings){
  MachineLearningInfo=list() # this is the output
  # train on full data
  fulldata=load(Internal$PrepareDataInfo$fulltraindatafilename) # load full training data
  MachineLearningInfo$FeatSelType = tolower(mySettings$inference$featureSelection$FeatureSelType)
  P=dim(fulldata$Xtrain.norm)[2]
  N=dim(fulldata$Xtrain.norm)[1]
  if(MachineLearningInfo$FeatSelType=="backwardselimination")
    BESteps = CalcBESteps(P,mySettings$inference$featureSelection$variablesMin,mySettings$inference$featureSelection$fractionToRemove)
  else BESteps = NULL
  fulldatamodel = TrainModels(fulldata$Xtrain.norm,fulldata$ytrain,mySettings,BESteps=BESteps)
  MachineLearningInfo$FullData$Model=fulldatamodel$model
  MachineLearningInfo$FullData$Ranking=fulldatamodel$ranking
  rm(fulldata,fulldatamodel)
  # do CV rounds, if enabled
  if(mySettings$preprocessing$crossvalidation$CVEnable){
    cvfolds = Internal$PrepareDataInfo$cvInfo
    if(MachineLearningInfo$FeatSelType=="internal"){
      PredTable = matrix(NA, N, cvfolds$R)
      PredClassLabelTable = PredTable
    }else if(MachineLearningInfo$FeatSelType=="backwardselimination"){
      PredTable = array(NA, dim=c(N, cvfolds$R,length(BESteps)))
      PredClassLabelTable = PredTable
    }else
      stop("Unsupported feature selection type: ",MachineLearningInfo$FeatSelType)
    for (iii in 1:cvfolds$R){ # repeats
      for(jjj in 1:cvfolds$K){ # folds
        #print(jjj)
        ## load data from file
        cvdata = load(nternal$PrepareDataInfo$cvfoldfilenames[iii,jjj])
        # train, predict, collect results
        CVResults = TrainModels(cvdata$CVX.train,cvdata$CVy.train,mySettings,Xtest=cvdata$CVX.test,BESteps=BESteps)
        if(MachineLearningInfo$FeatSelType=="internal"){
          PredTable[cvdata$testInds,iii] = CVResults$continuousPreds # vector-matrix
          PredClassLabelTable[cvdata$testInds,iii] = CVResults$labelPreds # vector-matrix
        }else if(MachineLearningInfo$FeatSelType=="backwardselimination"){
          PredTable[cvdata$testInds,iii,] = CVResults$continuousPreds # matrix
          PredClassLabelTable[cvdata$testInds,iii,] = CVResults$labelPreds # matrix
        }else 
          stop("Unsupported feature selection type: ",MachineLearningInfo$FeatSelType)
      } # end folds
    } # end repeats
    MachineLearningInfo$CV$PredTable=PredTable
    MachineLearningInfo$CV$PredClassLabelTable=PredClassLabelTable
  }
  return(MachineLearningInfo)
}

TrainModels=function(CVX.train,CVy.train,mySettings,Xtest=NULL,BESteps=NULL){
  #fields: model, continuousPreds, labelPreds, ranking
  FeatSelType = tolower(mySettings$inference$featureSelection$FeatureSelType)
  PredModFunc = get( cat("PredictiveModel",toupper(mySettings$inference$machineLearning$algorithm),sep="") )
  if(FeatSelType=="internal"){
    out=PredModFunc(CVX.train,CVy.train,mySettings,Xtest=Xtest,internFeatSel=T)
  }else if(FeatSelType=="backwardselimination"){
    out=list(model=NULL, continuousPreds=NULL, labelPreds=NULL, ranking=NULL)
    N=dim(Xtest)[1]
    P=dim(Xtest)[2]
    if(!is.null(Xtest)){
      PredTable=matrix(NA,N,length(BESteps))
      PredClassLabelTable=PredTable
    }
    isInitialised=FALSE
    for (numFeat in BESteps){
      if(!isInitialised){
        # use full data
        tempout=PredModFunc(CVX.train,CVy.train,mySettings,Xtest=Xtest)
        ranking=tempout$ranking
        isInitialised = TRUE
      }else{
        idx=matrix(NA,P,1)
        idx[ranking]=1:P # ranks to idx
        idx=sort(idx[1:numFeat])# picks numFeat highest ranked features and sorts
        tempout=PredModFunc(CVX.train[,idx],CVy.train,mySettings,Xtest=Xtest[,idx])
        ranking[idx]=tempout$ranking # update ranks
      }
      if(!is.null(Xtest)){
        PredTable[,numFeat]=tempout$continuousPreds
        PredClassLabelTable[,numFeat]=tempout$labelPreds
      }
    }
    if(!is.null(Xtest)){
      out$continuousPreds = PredTable
      out$labelPreds = PredClassLabelTable
    }
    stop("feature names")
    out$ranking = ranking
    # do not return a model as the feature elimination rounds produce multiple
  }else
    stop("Unsupported feature selection type: ",FeatSelType)
  return(out)
}

# machine learning implementations
PredictiveModelSDA=function(CVX.train,CVy.train,mySettings,Xtest=NULL,internFeatSel=F){
  ##SDA
  out=list(model=NULL, continuousPreds=NULL, labelPreds=NULL, ranking=NULL)
  if(internFeatSel){
    sdaranking = sda.ranking(CVX.train, CVy.train, verbose=FALSE)
    out$ranking = 1:dim(CVX.train)[1] # dummy
    stop("add column names")
    out$ranking[sdaranking[,"idx"]] = out$ranking # ranks of features
    NumFeatUse = max(sum(sdaranking[,"lfdr"] < 0.8),1) # use 1 at least
    sda.fit = sda(CVX.train.norm[,sdaranking[1:NumFeatUse,"idx"]], CVy.train, verbose=FALSE)
    if(!is.null(Xtest)){
      sdapreds = predict(sda.fit, CVX.test.norm[,sdaranking[1:NumFeatUse,"idx"]])
      out$labelPreds = sdapreds$class
      if(length(unique(CVy.train))<3)
        out$continuousPreds=sdapreds$posterior[,2,drop=F]
    }
  }else{ # use all data to train a model and predict
    sda.fit = sda(CVX.train.norm, CVy.train, verbose=FALSE)
    out$model=sda.fit
    if(!is.null(Xtest)){
      sdapreds = predict(sda.fit, CVX.test.norm)
      out$labelPreds = sdapreds$class
      if(length(unique(CVy.train))<3)
        out$continuousPreds=sdapreds$posterior[,2,drop=F]
    }
  }
}

PredictiveModelPOICLACLU=function(CVX.train,CVy.train,mySettings,Xtest=NULL){
  out=list(model=NULL, continuousPreds=NULL, labelPreds=NULL, ranking=NULL)
  ##PoiClaClu
  temp = Classify(x=CVX.train,y=CVy.train,
                  xte=CVX.test,rhos=c(0,5,10))
  temp[[2]]$ytehat
  return(out)
}

PredictiveModelSPLSDA=function(CVX.train,CVy.train,mySettings,Xtest=NULL){
  out=list(model=NULL, continuousPreds=NULL, labelPreds=NULL, ranking=NULL)
  return(out)
}

PredictiveModelPARTIALCOX=function(){
  
}

CalcBESteps=function(P,variablesMin,fractionToRemove){
  BESteps = c(P)
  varnumtemp=P
  while(varnumtemp>=variablesMin){
    varnumtemp=varnumtemp*(1-fractionToRemove)
    BESteps=c(BESteps,round(varnumtemp))
  }
  return(unique(BESteps))
}
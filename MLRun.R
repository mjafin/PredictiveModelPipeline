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
  MachineLearningInfo$FullDataModels=TrainModels(fulldata$Xtrain.norm,fulldata$ytrain,mySettings,BESteps=BESteps)$models
  rm(fulldata)
  # do CV rounds, if enabled
  if(mySettings$preprocessing$crossvalidation$CVEnable){
    cvfolds = Internal$PrepareDataInfo$cvInfo
    if(MachineLearningInfo$FeatSelType=="internal"){
      PredTable = matrix(NA, N, cvfolds$R)
      PredClassLabelTable = matrix(NA, N, cvfolds$R)
    }else if(MachineLearningInfo$FeatSelType=="backwardselimination"){
      PredTable = array(NA, dim=c(N, cvfolds$R,length(BESteps)))
      PredClassLabelTable = array(NA, dim=c(N, cvfolds$R,length(BESteps)))
    }else
      stop("Unsupported feature selection type: ",MachineLearningInfo$FeatSelType)
    for (iii in 1:cvfolds$R){ # repeats
      for(jjj in 1:cvfolds$K){ # folds
        #print(jjj)
        ## load data from file
        cvdata = load(nternal$PrepareDataInfo$cvfoldfilenames[iii,jjj])
        # train, predict, collect results
        CVResults = TrainModels(cvdata$CVX.train,cvdata$CVy.train,mySettings,Xtest=cvdata$CVX.test,BESteps=BESteps)
        PredTable[cvdata$testInds,iii] = CVResults$continuousPreds
        PredClassLabelTable[cvdata$testInds,iii] = CVResults$labelPreds
      } # end folds
    } # end repeats
    MachineLearningInfo$PredTable=PredTable
    MachineLearningInfo$PredClassLabelTable=PredClassLabelTable
  }
  return(MachineLearningInfo)
}

TrainModels=function(CVX.train,CVy.train,mySettings,Xtest=NULL,BESteps=NULL){
  #fields: models, continuousPreds, labelPreds, ranking
  FeatSelType = tolower(mySettings$inference$featureSelection$FeatureSelType)
  PredModFunc = get( cat("PredictiveModel",toupper(mySettings$inference$machineLearning$algorithm),sep="") )
  if(FeatSelType=="internal"){
    out=PredModFunc(CVX.train,CVy.train,mySettings,Xtest=Xtest,internFeatSel=T)
  }else if(FeatSelType=="backwardselimination"){
    for (numFeat in BESteps){
      out=PredModFunc(CVX.train,CVy.train,mySettings,Xtest=Xtest)
      stop("FIXME")
    }
  }else
    stop("Unsupported feature selection type: ",FeatSelType)
  return(out)
}

# machine learning implementations
PredictiveModelSDA=function(CVX.train,CVy.train,mySettings,Xtest=NULL,internFeatSel=F){
  ##SDA
  out=list(models=NULL, continuousPreds=NULL, labelPreds=NULL, ranking=NULL)
  if(internFeatSel){
    sdaranking = sda.ranking(CVX.train, CVy.train, verbose=FALSE)
    out$ranking = 1:1:dim(CVX.train)[1] # dummy
    ranking[sdaranking[,"idx"]] = 1:dim(CVX.train)[1] # ranks of features
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
    out$models=sda.fit
    if(!is.null(Xtest)){
      sdapreds = predict(sda.fit, CVX.test.norm)
      out$labelPreds = sdapreds$class
      if(length(unique(CVy.train))<3)
        out$continuousPreds=sdapreds$posterior[,2,drop=F]
    }
  }
}

PredictiveModelPOICLACLU=function(CVX.train,CVy.train,mySettings,Xtest=NULL){
  ##PoiClaClu
  temp = Classify(x=CVX.train,y=CVy.train,
                  xte=CVX.test,rhos=c(0,5,10))
  temp[[2]]$ytehat
}

PredictiveModelSPLSDA=function(CVX.train,CVy.train,mySettings,Xtest=NULL){
  
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
# internal functions
MLRun = function(Internal,mySettings){
  MachineLearningInfo=list() # this is the output
  # train on full data
  fulldata=load(Internal$PrepareDataInfo$fulltraindatafilename) # load full training data
  
  N=dim(fulldata$Xtrain.norm)[1]
  MachineLearningInfo$FullDataModel=TrainModels(fulldata$Xtrain.norm,fulldata$ytrain,mySettings)
  rm(fulldata)
  # do CV rounds, if enabled
  if(mySettings$preprocessing$crossvalidation$CVEnable){
    cvfolds = Internal$PrepareDataInfo$cvInfo
    PredTable = matrix(NA, N, cvfolds$R)
    
    for (iii in 1:cvfolds$R){ # repeats
      for(jjj in 1:cvfolds$K){ # folds
        #print(jjj)
        ## load data from file
        cvdata = load(nternal$PrepareDataInfo$cvfoldfilenames[iii,jjj])
        # train, predict, collect results
        CVResults = TrainModels(cvdata$CVX.train,cvdata$CVy.train,mySettings,Xtest=cvdata$CVX.test)
        PredTable[cvdata$testInds,iii] = CVResults$continuous        
      } # end folds
    } # end repeats
    MachineLearningInfo$PredTable=PredTable
  }
  return(MachineLearningInfo)
}

TrainModels=function(CVX.train,CVy.train,mySettings,Xtest=NULL){
  FeatSelType = tolower(mySettings$inference$featureSelection$FeatureSelType)
  PredModFunc = get(toupper(mySettings$inference$machineLearning$algorithm))
  if(FeatSelType=="internal"){
    out=PredModFunc(CVX.train,CVy.train,mySettings,Xtest=NULL)
  }else if(FeatSelType=="backwardselimination"){
    #write a loop for feature elimination
    out=PredModFunc(CVX.train,CVy.train,mySettings,Xtest=NULL)
  }else{
    stop("Unsupported feature selection type: ",FeatSelType)
  } 
}

# machine learning implementations
PredictiveModelSDA=function(){
  ##SDA
  sda.fit = sda(CVX.train.norm, CVy.train)
  predict(sda.fit, CVX.test.norm)$posterior[,2,drop=F]
}

PredictiveModelPOICLACLU=function(){
  ##PoiClaClu
  temp = Classify(x=CVX.train,y=CVy.train,
                  xte=CVX.test,rhos=c(0,5,10))
  temp[[2]]$ytehat
}

PredictiveModelSPLSDA=function(){
  
}

PredictiveModelPARTIALCOX=function(){
  
}

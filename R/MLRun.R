require(parallel)
require(doMC)
require(foreach)
registerDoMC(max(1,detectCores()-1)) # leave one core unutilised
# internal functions
MLRun = function(Internal,mySettings){
  MachineLearningInfo=list() # this is the output
  # train on full data
  load(Internal$PrepareDataInfo$fulltraindatafilename) # load full training data: Xtrain.norm, ytrain
  MachineLearningInfo$FeatSelType = tolower(mySettings$inference$featureSelection$featureSelType)
  P=dim(Xtrain.norm)[2]
  N=dim(Xtrain.norm)[1]

  Xtrain.norm.filt = filterData(Xtrain=Xtrain.norm,ytrain=ytrain,filterSettings=mySettings$inference$filtering)
  Pfilt=dim(Xtrain.norm.filt)[2] # number of features post filtering
  if(tolower(MachineLearningInfo$FeatSelType)=="backwardselimination"){
    BESteps = CalcBESteps(Pfilt,mySettings$inference$featureSelection$variablesMin,mySettings$inference$featureSelection$fractionToRemove)
    MachineLearningInfo$BESteps=BESteps
  }
  else BESteps = NULL
  # filtering step for the full data
  # training step for the full data
  fulldatamodel = TrainModels(Xtrain.norm.filt,ytrain,mySettings,BESteps=BESteps)
  MachineLearningInfo$FullData$Model=fulldatamodel$model
  # assign rank P to all variables first:
  MachineLearningInfo$FullData$FeatureRanks = rep(P,P)
  names(MachineLearningInfo$FullData$FeatureRanks) = colnames(Xtrain.norm)
  # use feature ranks from training to update the actual ranks
  MachineLearningInfo$FullData$FeatureRanks[colnames(Xtrain.norm.filt)]=fulldatamodel$featureRanks
  #if (!is.null(MachineLearningInfo$FullData$FeatureRanks))
  #  names(MachineLearningInfo$FullData$FeatureRanks)=Internal$SampleInfo$VarNames
  rm(fulldatamodel)
  # do CV rounds, if enabled
  if(mySettings$preProcessing$crossValidation$CVEnable){
    cvfolds = Internal$PrepareDataInfo$cvInfo
    if(tolower(MachineLearningInfo$FeatSelType)=="internal"){
      PredTable = matrix(NA, N, cvfolds$R)
      PredClassLabelTable = PredTable
    }else if(tolower(MachineLearningInfo$FeatSelType)=="backwardselimination"){
      PredTable = array(NA, dim=c(N, cvfolds$R,length(BESteps)))
      PredClassLabelTable = PredTable
    }else
      stop("Unsupported feature selection type: ",MachineLearningInfo$FeatSelType)
    
    for (iii in 1:cvfolds$R){ # repeats
      cat("\n")
      cat("*")
      cat("\n")
      myFoldsData = foreach(jjj=1:cvfolds$K) %dopar% { # folds
        cat("-")
        #print(jjj)
        ## load data from file
        load(Internal$PrepareDataInfo$cvfoldfilenames[iii,jjj])
        # train, predict, collect results
				CVX.train.norm.filt = filterData(Xtrain=CVX.train.norm,ytrain=CVy.train,filterSettings=mySettings$inference$filtering)
        CVResults = TrainModels(CVX.train.norm.filt,CVy.train,mySettings,Xtest=CVX.test.norm[,colnames(CVX.train.norm.filt)],BESteps=BESteps)
        if(tolower(MachineLearningInfo$FeatSelType)=="internal"){
          #PredTable[testInds,iii] = CVResults$continuousPreds # vector-matrix
          #PredClassLabelTable[testInds,iii] = CVResults$labelPreds # vector-matrix
        }else if(tolower(MachineLearningInfo$FeatSelType)=="backwardselimination"){
          #PredTable[testInds,iii,] = CVResults$continuousPreds # matrix
          #PredClassLabelTable[testInds,iii,] = CVResults$labelPreds # matrix

        }else 
          stop("Unsupported feature selection type: ",MachineLearningInfo$FeatSelType)
        list(testInds=testInds, continuousPreds=CVResults$continuousPreds, labelPreds=CVResults$labelPreds) # return value from parallel for
      } # end folds
			# collect results of parallel execution into a table
      for (myFoldData in myFoldsData){ #
				testInds=myFoldData$testInds
				if(tolower(MachineLearningInfo$FeatSelType)=="internal"){
          PredTable[testInds,iii] = myFoldData$continuousPreds # vector-matrix
          PredClassLabelTable[testInds,iii] = myFoldData$labelPreds # vector-matrix
        }else if(tolower(MachineLearningInfo$FeatSelType)=="backwardselimination"){
          PredTable[testInds,iii,] = myFoldData$continuousPreds # matrix
          PredClassLabelTable[testInds,iii,] = myFoldData$labelPreds # matrix
				}
      }
    } # end repeats
    MachineLearningInfo$CV$PredTable=PredTable
    MachineLearningInfo$CV$PredClassLabelTable=PredClassLabelTable
  }
  return(MachineLearningInfo)
}

TrainModels=function(CVX.train,CVy.train,mySettings,Xtest=NULL,BESteps=NULL){
  ## Train models either in backwards elimination or using internal feature selection. When training the final model with reduced CVX.train, provide a single numerical value in BESteps, e.g. the number of features to use. This enables returning the predictive model.
  #fields: model, continuousPreds, labelPreds, featureRanks
  
  FeatSelType = tolower(mySettings$inference$featureSelection$featureSelType)
  PredModFunc = get( paste("PredictiveModel",toupper(mySettings$inference$machineLearning$algorithm),sep="") )
  if(tolower(FeatSelType)=="internal"){
    internFeatSel=T
    if (!is.null(BESteps) && length(BESteps)==1) # this condition is true when finalising models and no feature selection is performed
      internFeatSel=F 
    out=PredModFunc(CVX.train,CVy.train,mySettings,Xtest=Xtest,internFeatSel=internFeatSel)
  }else if(tolower(FeatSelType)=="backwardselimination"){
    out=list(model=NULL, continuousPreds=NULL, labelPreds=NULL, featureRanks=NULL)
    N=dim(Xtest)[1]
    P=dim(CVX.train)[2]
    if(!is.null(Xtest)){
      PredTable=matrix(NA,N,length(BESteps))
      PredClassLabelTable=PredTable
    }
    isInitialised=FALSE
    for (numFeat in BESteps){
      if(!isInitialised){
        # use full data
        tempout=PredModFunc(CVX.train,CVy.train,mySettings,Xtest=Xtest)
        featureRanks=tempout$featureRanks
        isInitialised = TRUE
      }else{
        idx=matrix(NA,P,1)
        idx[featureRanks]=1:P # ranks to idx
        idx=sort(idx[1:numFeat])# picks numFeat highest ranked features and sorts
        tempout=PredModFunc(CVX.train[,idx,drop=FALSE],CVy.train,mySettings,Xtest=Xtest[,idx,drop=FALSE])
        featureRanks[idx]=tempout$featureRanks # update ranks
      }
      if(!is.null(Xtest)){
        PredTable[,numFeat==BESteps]=tempout$continuousPreds
        PredClassLabelTable[,numFeat==BESteps]=tempout$labelPreds
      }
    }
    if(!is.null(Xtest)){
      out$continuousPreds = PredTable
      out$labelPreds = PredClassLabelTable
    }
    out$featureRanks = featureRanks
    # do not return a model as the feature elimination rounds produce multiple
		# unless only one round is run (likely model finalisation)
    if (length(BESteps)==1) out$model = tempout$model
    #END else if(tolower(FeatSelType)=="backwardselimination")
  }else
    stop("Unsupported feature selection type: ",FeatSelType)
  return(out)
}

# Predict test samples:
PredictTestSamples=function(Xtest, Internal, mySettings){
  #fields: model, continuousPreds, labelPreds, featureRanks 
  PredModFunc = get( paste("PredictiveModel",toupper(mySettings$inference$machineLearning$algorithm),sep="") )
	# normalise test data
  # Internal$PrepareDataInfo$PreProcModel
	Xtest.norm = preProc(settings=mySettings$preProcessing,Xtrain=NULL,Xtest=Xtest,Model=Internal$PrepareDataInfo$PreProcModel)$xouttest 
	# predict test data using only the features in the final mode 
	out=PredModFunc(CVX.train=NULL,CVy.train=NULL,mySettings,Xtest=Xtest.norm[,Internal$FinalModelInfo$finalFeat],model=Internal$FinalModelInfo$model)
  out$Xtest.norm = Xtest.norm
  return(out)
}

# machine learning implementations
PredictiveModelSDA=function(CVX.train,CVy.train,mySettings,Xtest=NULL,internFeatSel=F,model=NULL){
  ##SDA
  out=list(model=NULL, continuousPreds=NULL, labelPreds=NULL, featureRanks=NULL)
  ## If 'model' is provided, merely predict Xtest:
  if(!is.null(model)){
		out$model = model
    sdapreds = predict(model, Xtest, verbose=FALSE)
    out$labelPreds = sdapreds$class
    if(length(unique(CVy.train))<3) # if two-class problem
      out$continuousPreds=sdapreds$posterior[,2,drop=F]
		return(out)
  }
  # otherwise do all sorts of training
  P=dim(CVX.train)[2]
  if(internFeatSel){
    sdaranking = sda.ranking(CVX.train, CVy.train, verbose=FALSE)
    out$featureRanks = 1:P # dummy
    out$featureRanks[sdaranking[,"idx"]] = out$featureRanks # ranks of features
    #names(out$featureRanks)=colnames(CVX.train)
    NumFeatUse = max(sum(sdaranking[,"lfdr"] < 0.8),1) # use 1 at least
    sda.fit = sda(CVX.train[,sdaranking[1:NumFeatUse,"idx"],drop=FALSE], CVy.train, verbose=FALSE)
    out$model=sda.fit
    out$NumFeatUse=NumFeatUse
    if(!is.null(Xtest)){
      sdapreds = predict(sda.fit, Xtest[,sdaranking[1:NumFeatUse,"idx"],drop=FALSE],verbose=FALSE)
      out$labelPreds = sdapreds$class
      if(length(unique(CVy.train))<3)
        out$continuousPreds=sdapreds$posterior[,2,drop=F]
    }
  }else{ # use all data to train a model and predict
    sda.fit = sda(CVX.train, CVy.train, verbose=FALSE)
    out$model=sda.fit
    sdaranking = sda.ranking(CVX.train, CVy.train, fdr=FALSE, verbose=FALSE)
    out$featureRanks = 1:P # dummy
    out$featureRanks[sdaranking[,"idx"]] = out$featureRanks # ranks of features
    #names(out$featureRanks)=colnames(CVX.train)
    if(!is.null(Xtest)){
      sdapreds = predict(sda.fit, Xtest, verbose=FALSE)
      out$labelPreds = sdapreds$class
      if(length(unique(CVy.train))<3)
        out$continuousPreds=sdapreds$posterior[,2,drop=F]
    }
  }
  return(out)
}

PredictiveModelPOICLACLU=function(CVX.train,CVy.train,mySettings,Xtest=NULL,internFeatSel=F,model=NULL){
  out=list(model=NULL, continuousPreds=NULL, labelPreds=NULL, featureRanks=NULL)
  ##PoiClaClu
  temp = Classify(x=CVX.train,y=CVy.train,
                  xte=CVX.test,rhos=c(0,5,10))
  temp[[2]]$ytehat
  return(out)
}

PredictiveModelSPLSDA=function(CVX.train,CVy.train,mySettings,Xtest=NULL,internFeatSel=F,model=NULL){
  out=list(model=NULL, continuousPreds=NULL, labelPreds=NULL, featureRanks=NULL)
  return(out)
}

PredictiveModelPARTIALCOX=function(CVX.train,CVy.train,mySettings,Xtest=NULL,internFeatSel=F,model=NULL){
  
}

CalcBESteps=function(P,variablesMin,fractionToRemove){
  BESteps = c(P)
  varnumtemp=P*(1-fractionToRemove)
  while(varnumtemp>=variablesMin){
    BESteps=c(BESteps,round(varnumtemp))
    varnumtemp=varnumtemp*(1-fractionToRemove)
  }
  return(unique(BESteps))
}

filterData=function(Xtrain,ytrain,filterSettings){
  # filter data based on settings
  N=dim(Xtrain)[1]
  P=dim(Xtrain)[2]
  for (counter in 1:length(filterSettings)){
    if(tolower(filterSettings[[counter]]$filterType) == "var"){
      NumRem = min(P-1,trunc(filterSettings[[counter]]$fractionToRemove*P)) # at most P-1
      NumKeep = P-NumRem
      vars = apply(Xtrain,2,var)
      # remove lowest variance features but keep order of columns
      Xtrain = Xtrain[,sort(order(vars,decreasing=TRUE)[1:NumKeep]),drop=FALSE]
    }else{
      stop(paste("Unsupported filter: ",filterSettings$filterType))
    }
  }
  return(Xtrain)
}

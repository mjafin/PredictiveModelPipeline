## Reference class based signature discovery and prediction

##temporary sourcing:
source("CVSetup.R")
source("MLRun.R")
source("helperFunctions.R")
#require("plotrix")
require("Hmisc")

if(!require("yaml")){
  stop("Package yaml is a requirement to run this pipeline. Please install yaml.")
}

# function that returns an object of class myMachineLearningClass
PredictiveModel = function(inputYamlFile){
  cat ("~~~~~ PredictiveModel: constructor ~~~~~ \n")
  mySettings=yaml.load_file(inputYamlFile)
  #lapply(mySettings$libraries,require,character.only = T) # load libraries
  Internal=list()
  Internal$AnalysisSteps=list(PreProcDone=F,MLDone=F,ModelFinalised=F)
  return(myMachineLearningClass$new(mySettings=mySettings,Internal=Internal))
  #return(myMachineLearningClass$new(yamlFile=inputYamlFile))
}

myMachineLearningClass <- setRefClass("PredictiveModel",
  fields = list( Internal = "list", mySettings = "list"),
  methods = list(
    ReloadSettings = function(inputYamlFile) {
      ## the following string documents the edit method
      'Reload settings from a file.
       '
      mySettings <<- yaml.load_file(inputYamlFile)
      invisible(1) # return 1 for success
    },
    PrepareData = function(Xtrain, ytrain) {
      'Prepare data for cross validation and model building.
       '
      if(Internal$AnalysisSteps$PreProcDone){
        cat("PrepareData has already been run for this object. Do you want to rerun (y/n)?")
        yesno=scan(what=character(),n=1)
        if(!length(grep("^[y,Y]",yesno))) # grep returns integer(0) if it doesn't match
          stop("Stopping.")
      }
      if (!is.matrix(Xtrain)){
        stop("Xtrain must be a matrix!")
      }
      Internal$SampleInfo <<- list(VarNames=colnames(Xtrain),
                                   SampleNames=rownames(Xtrain),
                                   N=dim(Xtrain)[1],
                                   P=dim(Xtrain)[2])
      ## Perform some sanity checks on input data
      if (is.null(Internal$SampleInfo$VarNames) || is.null(Internal$SampleInfo$VarNames)){
        stop("Input training matrix does not contain sample and/or variable names. Please provide some column and row names")
      }
      if(tolower(mySettings$preProcessing$inferenceType) == "classification"){
        if(!is.vector(ytrain) && !is.factor(ytrain))
          stop("In classification analysis, ytrain must be a vector or a factor")
        if(is.vector(ytrain))
           ytrain=factor(ytrain)
      }else if (tolower(mySettings$preProcessing$inferenceType) == "regression"){
        if(!is.vector(ytrain) && !is.matrix(ytrain))
          stop("In regression analysis, ytrain must be a vector or a matrix.")
      }else if (tolower(mySettings$preProcessing$inferenceType) == "timetoevent"){
        if (!is.Surv(ytrain))
          stop("In time to event analysis, ytrain must be a Surv object.")
      }else stop("Incorrect inference type selected.")
      
      Internal$SampleInfo$EndPointTrain <<- ytrain
      if (is.matrix(ytrain)){
        if(Internal$SampleInfo$N != dim(ytrain)[1])
          stop("Number of (row) values in ytrain does not agree with number of rows (samples) in Xtrain")
      }else {
        if(Internal$SampleInfo$N != length(ytrain))
          stop("Number of values in ytrain does not agree with number of rows (samples) in Xtrain")
      }
      # run CV setup
      Internal$PrepareDataInfo <<- CVSetup(Internal,mySettings,Xtrain,ytrain)
      Internal$AnalysisSteps$PreProcDone <<- T
      cat("PrepareData() finished successfully. Next run member function MachineLearning().")
      invisible(1)
    },
    MachineLearning = function() {
      'Evaluate models in cross validation. Performance can be plotted using the class method plot.
       '
      if(Internal$AnalysisSteps$MLDone){
        cat("MachineLearning has already been run for this object. Do you want to rerun (y/n)?")
        yesno=scan(what=character(),n=1)
        if(!length(grep("^[y,Y]",yesno))) # grep returns integer(0) if it doesn't match
          stop("Stopping.")
      }
      if(!Internal$AnalysisSteps$PreProcDone){
        stop("Please run the PrepareData() function for the object first, even if CV is disabled.")
      }
      # run actual CV machine learning (or just ranking on full data if CV disabled)
      Internal$MachineLearningInfo <<- MLRun(Internal,mySettings)
      Internal$AnalysisSteps$MLDone<<-T
      cat("MachineLearning() finished successfully. Next run member function Plot() followed by FinalModelBuild().")
      invisible(1)
    },
    FinalModelBuild = function() {
      'Build a predictor using the full training data set.
       '
      if(Internal$AnalysisSteps$ModelFinalised){
        cat("FinalModelBuild has already been run for this object. Do you want to rerun (y/n)?")
        yesno=scan(what=character(),n=1)
        if(!length(grep("^[y,Y]",yesno))) # grep returns integer(0) if it doesn't match
          stop("Stopping.")
      }
      if(!Internal$AnalysisSteps$MLDone){
        stop("Please run the MachineLearning() function for the object first, even if CV is disabled.")
      }
			# load data, preprocess and train
			load(Internal$PrepareDataInfo$fulltraindatafilename) # load full training data: Xtrain.norm, ytrain
			numFeat = mySettings$finalisation$NumFeatToUse
			cat("Number of features in the finalised model:", numFeat, "\n")
			finalFeat = names(sort(Internal$MachineLearningInfo$FullData$FeatureRanks)[1:numFeat])
      # Provide BESteps = numFeat to TrainModels in case BE was used as this triggers returning a model. Doesn't affect other (internal) feature selection methods
      Internal$FinalModelInfo <<- TrainModels(Xtrain.norm[,finalFeat],ytrain,mySettings, BESteps = numFeat)
      # store feature names
      Internal$FinalModelInfo$finalFeat<<-finalFeat
      Internal$AnalysisSteps$ModelFinalised<<-T
      cat("FinalModelBuild() finished successfully. Next run member function Predict() on external samples.")
      invisible(1)
    },
    Predict = function(Xtest) {
      'Predict new samples given a model trained in FinalModelBuild.
       '
			if(!Internal$AnalysisSteps$ModelFinalised){
        stop("Please run the FinalModelBuild() function for the object first before predicting new samples.")
      }
      if (!is.matrix(Xtest))
        stop("The input data must be a matrix!")
			#Normalise data and predict given model
      out=PredictTestSamples(Xtest, Internal, mySettings)
      return(out)
      invisible(1)
    },
    Plot = function() {
      'Plot performance measures for CV results.
       '
      if(!Internal$AnalysisSteps$MLDone)
        stop("Please run member function MachineLearning() before attempting to plot the results.")
      # check analysis type
      if(tolower(mySettings$preProcessing$inferenceType) == "classification"){
        if(length(levels(Internal$SampleInfo$EndPointTrain))>2)
          Measures=c("Accuracy")
        else
          Measures= c("Accuracy","Sensitivity","Specificity","AUC")
      }else if (tolower(mySettings$preProcessing$inferenceType) == "regression"){
        Measures=c("Concordance")
      }else if (tolower(mySettings$preProcessing$inferenceType) == "timetoevent"){
        Measures=c("Concordance","HR")
      }else stop("Incorrect inference type selected.")
      # how many x-axis values:
      if(tolower(Internal$MachineLearningInfo$FeatSelType)=="backwardselimination"){
        xAxis=Internal$MachineLearningInfo$BESteps
        xlab="# selected features"
        showXaxis="s"
      }else if(tolower(Internal$MachineLearningInfo$FeatSelType)=="internal"){
        xAxis=0
        xlab="Internal feature selection"
        showXaxis="n"
      }
      else
        stop("Incorrect feature selection type selected.")
      Internal$PerformanceMeasures <<- list()
      Internal$PerformanceMeasures$xAxis <<- xAxis
      # plot
      for (measure in Measures){
        stats = fetchMeasures(measure,Internal$MachineLearningInfo$CV,Internal$SampleInfo$EndPointTrain)
        imageFileName=paste(mySettings$projectname,mySettings$inference$machineLearning$algorithm,measure,format(Sys.time(), "%Y%m%d%H%M"), sep = "_")
        pdf(paste(imageFileName,".pdf", sep = ""))
        errbar(xAxis, y=stats$average, yminus=stats$lowerInt, yplus=stats$upperInt,
               ylab=measure,
               xlab=xlab,
               xaxt=showXaxis,
               log="x") # errbar from Hmisc
        title("Cross validation performance")
        Internal$PerformanceMeasures[[measure]]<<-stats
#        imageFileName=paste(mySettings$projectname,mySettings$inference$machineLearning$algorithm,measure,format(Sys.time(), "%Y%m%d%H%M"), sep = "_")
        
        #postscript(paste(imageFileName,".ps", sep = ""))
        dev.off()
        sprintf("Stored %s figure as %s.pdf and .ps",measure,imageFileName)
      }
      invisible(1)
    },
    show = function() {
      'Method for automatically printing contents of class instances'
      cat("Reference object of class",
          classLabel(class(.self)), "\n")
      #cat("Current settings: \n")
      #methods::show(mySettings) # no recursion here... but show() function on mySettings
      myMethods = myMachineLearningClass$methods()
      cat("Public methods: ",myMethods[grep("^[A-Z]",myMethods)]) # show methods that start with upper case
    }
  ))

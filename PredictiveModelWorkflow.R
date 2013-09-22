## a simple editor for matrix objects.  Method  $edit() changes some
## range of values; method $undo() undoes the last edit.
# 
# $usingMethods()?

##temporary sourcing:
source("CVSetup.R")
source("MLRun.R")

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
        if(length(grep("^[y,Y]",yesno))) # grep returns integer(0) if it doesn't match
          stop()
      }
      Internal$SampleInfo <<- list(VarNames=colnames(Xtrain),
                                   SampleNames=rownames(Xtrain),
                                   N=dim(X)[1],
                                   P=dim(X)[2])
      ## Perform some sanity checks on input data
      if (is.null(Internal$SampleInfo$VarNames) || is.null(Internal$SampleInfo$VarNames)){
        stop("Input training matrix does not contain sample and/or variable names. Please provide some column and row names")
      }
      if(!is.matrix(ytrain)){
        if(is.factor(ytrain)){
          if(tolower(mySettings$preProcessing$inferenceType) != "classification"){
            stop("ytrain may only be a factor in case of classification analysis.")
          }
          Internal$SampleInfo$FactorLevels <<- levels(ytrain)
          ytrain = matrix((1:length(ytrain))[ytrain],length(ytrain),1)
        }
        else if(is.vector(ytrain)){
          ytrain = matrix(ytrain,length(ytrain),1)
        }
        else{
          stop("Please provide ytrain as a column matrix, vector or factor")
        }
      }
      Internal$SampleInfo$EndPointTrain <<- ytrain
      if(Internal$SampleInfo$N != dim(ytrain)[1]){
        stop("Number of (row) values in ytrain does not agree with number of rows (samples) in Xtrain")
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
        if(length(grep("^[y,Y]",yesno))) # grep returns integer(0) if it doesn't match
          stop()
      }
      if(!Internal$AnalysisSteps$PreProcDone){
        stop("Please run the PrepareData() function for the object first, even if CV is disabled.")
      }
      # run actual CV machine learning (or just ranking on full data if CV disabled)
      Internal$MachineLearningInfo <<- MLRun(Internal,mySettings)
      Internal$AnalysisSteps$MLDone<<-T
      cat("MachineLearning() finished successfully. Next run member function FinalModelBuild().")
      invisible(1)
    },
    FinalModelBuild = function() {
      'Build a predictor using the full training data set.
       '
      if(Internal$AnalysisSteps$ModelFinalised){
        cat("FinalModelBuild has already been run for this object. Do you want to rerun (y/n)?")
        yesno=scan(what=character(),n=1)
        if(length(grep("^[y,Y]",yesno))) # grep returns integer(0) if it doesn't match
          stop()
      }
      if(!Internal$AnalysisSteps$MLDone){
        stop("Please run the MachineLearning() function for the object first, even if CV is disabled.")
      }
      
      Internal$AnalysisSteps$ModelFinalised<<-T
      cat("FinalModelBuild() finished successfully. Next run member function Predict() on external samples.")
      invisible(1)
    },
    Predict = function() {
      'Predict new samples given a model trained in FinalModelBuild.
       '
      invisible(1)
    },
    Plot = function() {
      'Plot performance measures for CV results.
       '
      invisible(1)
    },
    show = function() {
      'Method for automatically printing contents of class instances'
      cat("Reference object of class",
          classLabel(class(.self)), "\n")
      cat("Current settings: \n")
      methods::show(mySettings) # no recursion here... but show() function on mySettings
      myMethods = myMachineLearningClass$methods()
      cat("Public methods: ",myMethods[grep("^[A-Z]",myMethods)]) # show methods that start with upper case
    }
  ))

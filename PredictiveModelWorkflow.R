if(!require("yaml")){
  stop("Package yaml is a requirement to run this pipeline. Please install yaml.")
}
# 
# slotNames(new(Class="PredictiveModel"))
# getSlots("PredictiveModel")
# getClass
# showMethods(class="PredictiveModel")
# getMethod(f="plot",signature="PredictiveModel")
# 
# 

setClass(
  Class = "PredictiveModel",
  representation=representation( # required information
    mySettings = "list",
    PreProcDone = "logical",
    MLDone = "logical",
    ModelFinalised = "logical",
    Internal = "list"
  ),
  prototype=prototype( # default values
    mySettings = list(),
    PreProcDone = F,
    MLDone = F,
    ModelFinalised = F,
    Internal = list()
  ),
  validity=function(object){ # input validator at time of object creation
    # do some input checking
  }
)
## Constructor for the user to call (instead of new())
predictiveModel <- function(yamlfile){
  cat ("~~~~~ PredictiveModel: constructor ~~~~~ \n")
  mySettings=yaml.load_file(yamlfile)
  lapply(mySettings$libraries,require,character.only = T) # load libraries
  new (Class="PredictiveModel", mySettings=mySettings)
}

## Internal constructor
#setMethod (
#  f="initialize",
#  signature="Trajectories",
#  definition=function(.Object,times,traj){
#    cat ("~~~~~ Trajectories: initializator ~~~~~ \n")
#    if(!missing(traj)){
#      colnames(traj) <- paste("T",times,sep="")
#      rownames(traj) <- paste("I",1:nrow(traj),sep="")
#      .Object@times <- times
#      .Object@traj <- traj
#      validObject(.Object) # call of the inspector
#    }
#    return(.Object)
#  }
#)

## Getters:
#setGeneric("getTimes",function(object){standardGeneric ("getTimes")})
#setMethod("getTimes","PredictiveModel",
#          function(object){
#            return(object@times)
#          }
#)

## Setters:
#setGeneric("setTimes<-",function(object,value){standardGeneric("setTimes<-")})
#setReplaceMethod(
#  f="setTimes",
#  signature="PredictiveModel",
#  definition=function(object,value){
#    object@times <- value
#    validObject(object) # check validity of input!!
#    return(object)
#  }
#)


## Plot template
setMethod(
  f= "plot", # check args(plot) 
  signature= "PredictiveModel",
  definition=function (x,y,...){
    matplot(x@myPreProcSettings$CVFolds,t(x@mySettings$preprocessing$CVRepeats),xaxt="n",type="l",ylab= "",xlab="", pch=1)
    #axis(1,at=x@times)
  }
)

## Print template
setMethod ("print","PredictiveModel", # check args(print)
           function(x,...){
             cat("*** Class PredictiveModel, method Print *** \n")
             cat("* Pre-processing settings ="); print (x@mySettings$preprocessing)
             cat("* Filtering settings = \n"); print (x@mySettings$inference$filtering)
             cat("* Feature selection settings = \n"); print (x@mySettings$inference$featureselection)
             cat("* Machine learning settings = \n"); print (x@mySettings$inference$machinelearning)
             cat("******* End Print (PredictiveModel) ******* \n")
           }
)

## Show template
setMethod("show","PredictiveModel",
          function(object){
            cat("*** Class PredictiveModel, method Show *** \n")
            cat(slotNames(object))
          }
)

## Internal functions 
setGeneric (
  name= "ReloadSettings",
  def=function(object, yamlfile){standardGeneric("ReloadSettings")}
)
setGeneric (
  name= "PreProcessing",
  def=function(object, Xtrain, ytrain){standardGeneric("PreProcessing")}
)
setGeneric (
  name= "MachineLearning",
  def=function(object){standardGeneric("MachineLearning")}
)
setGeneric (
  name= "ModelFinalisation",
  def=function(object){standardGeneric("ModelFinalisation")}
)
setGeneric (
  name= "ModelApply",
  def=function(object){standardGeneric("ModelApply")}
)

## Internal function definitions (to be moved to separate files)
setMethod(
  f= "ReloadSettings",
  signature= "PredictiveModel",
  definition=function(object, yamlfile){
    mySettings=yaml.load_file(yamlfile)
    object@mySettings = mySettings
  }
)
setMethod(
  f= "PreProcessing",
  signature= "PredictiveModel",
  definition=function(object, Xtrain, ytrain){
    if(object@PreProcDone){
      cat("PreProcessing function already run for this object. \n")
      return
    }
    # CHANGE this
    source("quantilenorm.R")
    sett = object@mySettings$preprocessing
    if(sett$crossvalidation$CVEnable && !require("cvTools")){
      stop("Package yaml is a requirement to run this pipeline. Please install yaml.")
    }
    N = dim(Xtrain)[1]
    y=as.matrix(ytrain,N,1)
    timeString = format(Sys.time(), "%Y%m%d%H%M")
    object@Internal$preprocessing$fulltraindatafile = sprintf("fullData_%s_%s.RData",object@mySettings$projectname,timeString)
    if (sett$crossvalidation$CVEnable){
      object@Internal$preprocessing$cvfoldfilenames = data.frame(matrix(NA, nrow = sett$crossvalidation$CVRepeats, ncol = sett$crossvalidation$CVFolds))
      set.seed(sett$crossvalidation$randomSeed)
      # CHANGE drawing of folds to be more intelligent
      cvfolds = cvFolds(n=N, K = sett$crossvalidation$CVFolds, R = sett$crossvalidation$CVRepeats)
      object@Internal$preprocessing$cvfolds = cvfolds # store for later use
      for (iii in 1:cvfolds$R){ # repeats
        for(jjj in 1:cvfolds$K){ # folds
          trainInds = cvfolds$subsets[cvfolds$which!=jjj,iii]
          testInds = cvfolds$subsets[cvfolds$which==jjj,iii]
          CVX.train = Xtrain[trainInds,,drop=F]
          CVy.train = y[trainInds,,drop=F]
          CVX.test = Xtrain[testInds,,drop=F]
          CVy.test = y[testInds,,drop=F]
          ## pre-process X
          ## CHANGE to allow other normalisation
          normTemp = quantilenorm(CVX.train,method="quant", quantprob=0.75)
          CVX.train.norm = normTemp$xout
          CVX.test.norm = quantilenorm(CVX.test,refquant=normTemp$quantiles)$xout
          object@Internal$preprocessing$cvfoldfilenames[iii,jjj] = sprintf("CVRepeat%dFold%d_%s_%s.Rdata",iii,jjj,object@mySettings$projectname,timeString)
          save(CVX.train.norm, CVX.test.norm, CVy.train, CVy.test, trainInds, testInds, file = object@Internal$preprocessing$cvfoldfilenames[iii,jjj])
        }
      }
    }
    #pre process full data and store
    Xtrain.norm = quantilenorm(Xtrain,method="quant", quantprob=0.75)$xout
    save(Xtrain.norm, y, file = object@Internal$preprocessing$fulltraindatafile)
    object@PreProcDone = T
  }
)
setMethod(
  f= "MachineLearning",
  signature= "PredictiveModel",
  definition=function(object){
    object@MLDone = T
  }
)
setMethod(
  f= "ModelFinalisation",
  signature= "PredictiveModel",
  definition=function(object){
    object@ModelFinalised = T
  }
)
setMethod(
  f= "ModelApply",
  signature= "PredictiveModel",
  definition=function(object){
    return(NULL)
  }
)

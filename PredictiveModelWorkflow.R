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
    ModelFinalised = "logical"
  ),
  prototype=prototype( # default values
    mySettings = list(),
    PreProcDone = F,
    MLDone = F,
    ModelFinalised = F
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

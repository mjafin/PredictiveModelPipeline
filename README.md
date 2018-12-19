# PredictiveModelPipeline
High dimensional predictive modelling in R

Basic usage
```
require(sda)
data(singh2002)
colnames(singh2002$x) = paste("V",1:dim(singh2002$x)[2],sep="") # need unique column / variable names
rownames(singh2002$x) = paste("S",1:dim(singh2002$x)[1],sep="") # need unique row / sample names
names(singh2002$y) = rownames(singh2002$x)

source("PredictiveModelPipeline.R")
mypredmodel = PredictiveModel("/path/to/data/analysis_testing.yaml") # analysis template with settings
mypredmodel$PrepareData(Xtrain=singh2002$x , ytrain=singh2002$y) # prepare CV folds by preprocessing methods
mypredmodel$MachineLearning() # all settings specified in yaml # perform machine learning (parallelised)
require(epibasix)
mypredmodel$Plot() # make AUC, Sens, Spec, Acc plots
#mypredmodel$FinalModelBuild() # build a final model but first select number of features based on plots, specify in yaml and reload yaml
#myout = mypredmodel$Predict(testdata) # test on external data. dimensionality / number of columns must match training matrix
```

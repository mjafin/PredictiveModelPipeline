require("cvTools")
source("quantilenorm.R")
CrossValidate = function(Xtrain, y, CVRepeats=10, CVFolds=5, myRandomSeed=1234){
  N = dim(Xtrain)[1]
  y=as.matrix(y,N,1)
  set.seed(myRandomSeed)
  cvfolds = cvFolds(n=N, K = CVFolds, R = CVRepeats)
  PredTable = matrix(NA, N, cvfolds$R)
  for (iii in 1:cvfolds$R){ # repeats
    for(jjj in 1:cvfolds$K){ # folds
      print(jjj)
      trainInds = cvfolds$subsets[cvfolds$which!=jjj,iii]
      testInds = cvfolds$subsets[cvfolds$which==jjj,iii]
      CVX.train = Xtrain[trainInds,,drop=F]
      CVy.train = y[trainInds,,drop=F]
      CVX.test = Xtrain[testInds,,drop=F]
      CVy.test = y[testInds,,drop=F]
      # pre-process X
      normTemp = quantilenorm(CVX.train,method="quant", quantprob=0.75)
      CVX.train.norm = normTemp$xout
      CVX.test.norm = quantilenorm(CVX.test,refquant=normTemp$quantiles)$xout
      rm(CVX.train,CVX.test)
      # filter X
      # train and predict
      sda.fit = sda(CVX.train.norm, CVy.train)
      PredTable[testInds,iii] = predict(sda.fit, CVX.test.norm)$posterior[,2,drop=F]
      # collect results
    } # end folds
  } # end repeats
  return(PredTable)
}

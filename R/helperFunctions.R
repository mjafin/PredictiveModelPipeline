fetchMeasures=function(measure,CVpreds,EndPointTrain){
  # check if class labels required, otherwise assume continuous predictions
  if(is.element(measure,c("Accuracy","Sensitivity","Specificity","PPV","NPV")))
    myPreds = CVpreds$PredClassLabelTable
  else
    myPreds = CVpreds$PredTable
  # 
  FeatSelSteps=ifelse(!is.na(dim(myPreds)[3]),dim(myPreds)[3],1)
  Reps=dim(myPreds)[2] # CV repeats
  average=matrix(NA,FeatSelSteps,Reps)
  upperInt=average
  lowerInt=average
  myFunc=get(paste(measure,"Func",sep=""))
  for (iii in 1:Reps){
    for (jjj in 1:FeatSelSteps){
      if(FeatSelSteps>1)
        tempMet=myFunc(myPreds[,iii,jjj],EndPointTrain)
      else tempMet=myFunc(myPreds[,iii],EndPointTrain)
      average[jjj,iii]=tempMet$average
      upperInt[jjj,iii]=tempMet$CI[2]
      lowerInt[jjj,iii]=tempMet$CI[1]
    }
  }
  out=list()
  out$average=apply(X=average, MARGIN=1, FUN=mean) # average out "repeats"
  out$upperInt=apply(X=upperInt, MARGIN=1, FUN=median) # average out "repeats"
  out$lowerInt=apply(X=lowerInt, MARGIN=1, FUN=median) # average out "repeats"
  return(out)
}

AccuracyFunc=function(preds,EndPointTrain){
  out=list(average=NA,CI=c(NA,NA))
  # form a table but flip the columns and rows, so that sensitivity for the higher level get estimated
  if (length(unique(EndPointTrain))<=2){
  temp=sensSpec(table(preds,EndPointTrain)[2:1,2:1], alpha=0.05, CL=TRUE, digits=3)
  out$average = temp$PA
  out$CI = c(temp$PA.CIL,temp$PA.CIU)
  }else{ # multiclass, use simple proportions
    temp=table(preds,EndPointTrain)
    temp2 = binom.test(x=sum(diag(temp)),n=sum(temp))
    out$average = temp2$estimate
    out$CI = temp2$conf.int
  }
  return(out)
}
AUCFunc=function(preds,EndPointTrain){
  'AUC estimation
   '
  out=list(average=NA,CI=c(NA,NA))
  w = rcorr.cens(preds, EndPointTrain)
  C  = w['C Index']
  se = w['S.D.']/2
  low = C-1.96*se 
  hi <- C+1.96*se
  out$average=C
  out$CI=c(low,hi)
  return(out)
}
SensitivityFunc=function(preds,EndPointTrain){
  'Sensitivity estimation
   '
  out=list(average=NA,CI=c(NA,NA))
  # form a table but flip the columns and rows, so that sensitivity for the higher level get estimated
  temp=sensSpec(table(preds,EndPointTrain)[2:1,2:1], alpha=0.05, CL=TRUE, digits=3)
  out$average = temp$sens
  out$CI = c(temp$sens.CIL,temp$sens.CIU)
  return(out)
}
SpecificityFunc=function(preds,EndPointTrain){
  'Specificity estimation
   '
  out=list(average=NA,CI=c(NA,NA))
  # form a table but flip the columns and rows, so that sensitivity for the higher level get estimated
  temp=sensSpec(table(preds,EndPointTrain)[2:1,2:1], alpha=0.05, CL=TRUE, digits=3)
  out$average = temp$spec
  out$CI = c(temp$spec.CIL,temp$spec.CIU)
  return(out)
}
ConcordanceFunc=function(preds,EndPointTrain){
  'Concordance index estimation
   '
  out=list(average=NA,CI=c(NA,NA))
  if(is.matrix(EndPointTrain))
    if(dim(EndPointTrain)[2]==2) # assume input is a survival information matrix
      EndPointTrain=Surv(time=EndPointTrain[,1],event=EndPointTrain[,2])
  w = rcorr.cens(preds, EndPointTrain)
  C  = w['C Index']
  se = w['S.D.']/2
  low = C-1.96*se 
  hi <- C+1.96*se
  out$average=C
  out$CI=c(low,hi)
  return(out)
}
HRFunc=function(preds,EndPointTrain){
  'Hazard ratio estimation
   '
  out=list(average=NA,CI=c(NA,NA))
  m <- coxph(Surv(time=EndPointTrain[,1],event=EndPointTrain[,2]) ~ preds>0)
  beta <- coef(m)
  se <- sqrt(vcov(m))
  HR <- exp(beta)
  out$CI[1] = exp(beta - qnorm(.975, 0, 1) * se)
  out$CI[2] = exp(beta + qnorm(.975, 0, 1) * se)
  return(out)
}
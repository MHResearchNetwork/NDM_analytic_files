## RStudio version - 1.1.463
## R version - 3.5.3 (2019-03-11) -- "Great Truth"

## set large memory limit for virtual machine (~64GB)
gc()
memory.limit(size=500000)

## data.table version 1.12.2
library(data.table)
## bit64 version 0.9-7
library(bit64)
## ranger version 0.11.2
library(ranger)
## ROCR version 1.0.7
library(ROCR)

#############################################################################################################
### This function was written by Rod Walker, August 2019
### It relies on the CRAN packages data.table and ROCR, and the function "prediction" within ROCR
### It calls the commands - library(data.table) and library(ROCR) - to make sure the pacakges are installed
### UPDATE: March 2019 by Rod Walker
###   --- changed name of function to perf.results.id 
###   --- added Mean Square Error (which is Brier score as we are using a binary outcome) 
###   --- added function parameter 'outcomeID' for user to provide unique identifiers for events
###   --- arranged for calibration table to compute number of visits, events, and unique events
### UPDATE: March 2021 by Rod Walker
###   --- removed unique events parameter (commented out corresponding code)
###   --- this allows function to not require extra eventID info (easier to use)
###   --- changed name of function back to perf.results
#############################################################################################################

#### perf.results - function to provide calibration and performance tables, metrics, and plots
####
perf.results    <- function(pred, # predicted probability of outcome=1 for each observation in the test set
                            outcome, # 0/1 numeric outcome for each observation in the test set
                            ##outcomeID, # numeric code for each unique outcome in the test set (non-events need to be coded as NA)
                            train.pred, # predicted probability of outcome=1 for each observation in the training set
                            strata.pctile=c(0.50, 0.75, 0.90, 0.95, 0.99, 0.995), # vector of percentiles to use for calibration/performance strata
                            # (provide values from 0-1, not including 0 or 1)
                            inc.plots=FALSE # produce plots? 
                            # (plots can be slow to create)
) {    
  ## data.table version 1.12.2
  library(data.table)
  ## ROCR version 1.0.7
  library(ROCR)
  
  # identify cutpoints in training data that correspond to provided strata percentiles
  train.pctile <- quantile(train.pred, strata.pctile)   

  # create calibration table
  calib.tab <- data.table(t(mapply(function(x,y){rbind(mean(pred[pred>=x & pred<y]),
                                                       mean(outcome[pred>=x & pred<y]),
                                                       length(outcome[pred>=x & pred<y]),
                                                       sum(outcome[pred>=x & pred<y]))},
                                                       ##length(unique(outcomeID[pred>=x & pred<y])))},
                                   c(0,train.pctile), c(train.pctile,1.01))))
  
  calib.tab <- data.table(paste(c("0%",names(train.pctile)), c(names(train.pctile),"100%"), sep="-"), calib.tab)
  names(calib.tab) <- c("Pctile","AvgPredRisk","ObsRisk","Vists","Events")
                        ##,"UniqueEvents")
  
  # create ROCR prediction object and table of values (needed to construct performance table and plots)
  # prediction is a function in the ROCR package
  obj.rocr <- prediction(pred, outcome)
  tab.rocr <- data.table(RiskCutpoint=obj.rocr@cutoffs[[1]],
                         Sens=performance(obj.rocr,"sens")@y.values[[1]],
                         Spec=performance(obj.rocr,"spec")@y.values[[1]],
                         PPV=performance(obj.rocr,"ppv")@y.values[[1]],
                         NPV=performance(obj.rocr,"npv")@y.values[[1]],
                         FPR=performance(obj.rocr,"fpr")@y.values[[1]],
                         FNR=performance(obj.rocr,"fnr")@y.values[[1]],
                         Fscore=performance(obj.rocr,"f")@y.values[[1]],
                         TP=obj.rocr@tp[[1]],
                         FP=obj.rocr@fp[[1]],
                         FN=obj.rocr@fn[[1]],
                         TN=obj.rocr@tn[[1]])
  
  # create performance table
  perf.tab <- rbindlist(lapply(train.pctile, function(x){last(tab.rocr[RiskCutpoint>=x])}))
  perf.tab$RiskCutpoint <- train.pctile
  perf.tab <- data.table(Pctile=names(train.pctile), perf.tab)
  
  # compute auc
  auc.rocr <- performance(obj.rocr,"auc")
  
  # compute Brier Score (MSE in our binary case)
  brier.sc <- mean((pred-outcome)^2)
  
  # construct plots, if requested (change range of x and y-axes, as needed)
  if(inc.plots==TRUE){
    
    # plot of ROC curve
    roc.rocr <- performance(obj.rocr,"tpr","fpr")
    plot(roc.rocr, main=paste("AUC =", round(auc.rocr@y.values[[1]],4)), cex.main=1)
    abline(a=0,b=1)
    
    # plot of Precision-Recall curve
    prec.rocr <- performance(obj.rocr,"prec","rec")
    plot(prec.rocr, main="Precision-Recall curve", cex.main=1, xlim=c(0,0.15), ylim=c(0,1))
    
    # plot of PPV, NPV, FPR, FNR curves using an x-axis defined by the percentile cutpoints in training data
    pctile.plot <- seq(0, 1, 0.005)
    train.pctile.plot <- quantile(train.pred, pctile.plot)
    tab.rocr.plot <- rbindlist(lapply(train.pctile.plot, function(x){last(tab.rocr[RiskCutpoint>=x])}))
    plot(x=pctile.plot*100, y=tab.rocr.plot$PPV, xlab="Risk Cutpoint Percentile", ylab="PPV", main="PPV-curve", type='l', lwd=1, cex.main=1, xlim=c(0,100), ylim=c(0,0.3))
    plot(x=pctile.plot*100, y=tab.rocr.plot$NPV, xlab="Risk Cutpoint Percentile", ylab="NPV", main="NPV-curve", type='l', lwd=1, cex.main=1, xlim=c(0,100), ylim=c(0.99,1))
    plot(x=pctile.plot*100, y=tab.rocr.plot$FPR, xlab="Risk Cutpoint Percentile", ylab="FPR", main="FPR-curve", type='l', lwd=1, cex.main=1, xlim=c(0,100), ylim=c(0,1))
    plot(x=pctile.plot*100, y=tab.rocr.plot$FNR, xlab="Risk Cutpoint Percentile", ylab="FNR", main="FNR-curve", type='l', lwd=1, cex.main=1, xlim=c(0,100), ylim=c(0,1))
  }
  
  # return calibration table, preformance table, and AUC 
  res.list <- list(calib.tab, perf.tab, auc.rocr@y.values[[1]], brier.sc)
  names(res.list) <- c("Calibration","Performance","AUC", "Brier Score")
  return(res.list)
  
}
####
#### end perf.results function

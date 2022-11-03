#### perf.boot - function to provide bootstrap for performance metrics ####

perf.boot <- function(pred, # predicted probability of outcome=1 for each observation in the test set
                      outcome, # 0/1 numeric outcome for each observation in the test set
                      train.pctile # cutpoints in training data that correspond to desired percentiles
) {    
  library(data.table)
  library(ROCR)
  
  # create ROCR prediction object and table of values (needed to construct performance table)
  # prediction is a function in the ROCR package
  obj.rocr <- prediction(pred, outcome)
  tab.rocr <- data.table(RiskCutpoint=obj.rocr@cutoffs[[1]],
                         Sens=performance(obj.rocr,"sens")@y.values[[1]],
                         Spec=performance(obj.rocr,"spec")@y.values[[1]],
                         PPV=performance(obj.rocr,"ppv")@y.values[[1]],
                         NPV=performance(obj.rocr,"npv")@y.values[[1]],
                         Fscore=performance(obj.rocr,"f")@y.values[[1]])
  
  # create performance table
  perf.tab <- rbindlist(lapply(train.pctile, function(x){last(tab.rocr[RiskCutpoint>=x])}))
  perf.tab$RiskCutpoint <- NULL
  perf.tab <- data.table(Pctile=names(train.pctile), perf.tab)
  
  # compute auc
  auc.rocr <- performance(obj.rocr,"auc")
  
  # compute Brier Score (MSE in our binary case)
  brier.sc <- mean((pred-outcome)^2, na.rm=TRUE)
  
  # return preformance table and AUC 
  res.list <- list(perf.tab, auc.rocr@y.values[[1]], brier.sc)
  names(res.list) <- c("Performance","AUC", "Brier Score")
  return(res.list)
  
}



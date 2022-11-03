
## clear memory
rm(list=ls())
gc()

## load packages
library(data.table)
library(bit64)
library(ranger)
library(ROCR)

## get session information
sessionInfo()

## set working directory
data_dir <- "C:/Users/Administrator/Downloads"
setwd(data_dir)

## source program that provides function perf.results for computing performance metrics
source("RF_ComputePerformance.R")

### create summary objects of IN-SAMPLE performance from tuning parameter search with cross-validation
### i indicates model, j indicates fold
for(i in 1:13){
  for(j in 1:5){
    print(c(i,j))
    ## load random forest tuning parameter model results
    load(file=paste("tune_mh_per_mod", i, "_", j, "_e90.Rdata", sep=""))
    ## random forest used factor outcome but change back to numeric here
    res_train$OUTCOME <- as.numeric(levels(res_train$OUTCOME))[res_train$OUTCOME]
    ## apply perf.results function to compute performance
    res.mod <- perf.results(pred=res_train$PRED, outcome=res_train$OUTCOME, train.pred=res_train$PRED, inc.plots=FALSE)
    ## store tuning parameters
    res.mod[["mtry"]] <- rf_mod$mtry
    res.mod[["num.trees"]] <- rf_mod$num.trees
    res.mod[["min.node.size"]] <- rf_mod$min.node.size
    ## save summary object
    assign(paste("res.mod", i, "_", j, sep=""), res.mod)
    save(list=paste("res.mod", i, "_", j, sep=""), file=paste("tune_mh_per_trainmod", i, "_", j, "_e90.Rdata", sep=""))
    rm(list=paste("res.mod", i, "_", j, sep=""))
    rm(res.mod,res_train,res_test,rf_mod,mod.time,ptrain.time,ptest.time)
    gc()
  }
}

### create summary objects of OUT-OF-SAMPLE performance from tuning parameter search with cross-validation
### i indicates model, j indicates fold
for(i in 1:13){
  ## combine out-of-sample predictions (i.e., from each held out fold) into single dataset before computing performance
  ## also store cumulative run times for descriptive purposes
  for(j in 1:5){
    ## load random forest tuning parameter model results
    load(file=paste("tune_mh_per_mod", i, "_", j, "_e90.Rdata", sep=""))
    if(j==1){
      test.dat <- res_test
      mod.hrs <- mod.time[3]/60/60
      ptrain.hrs <- ptrain.time[3]/60/60
      ptest.hrs <- ptest.time[3]/60/60
      rm(rf_mod,res_train,res_test,mod.time,ptest.time,ptrain.time)
      gc()
    }
    else{
      test.dat <- rbindlist(list(test.dat,res_test))
      mod.hrs <- mod.time[3]/60/60 + mod.hrs
      ptrain.hrs <- ptrain.time[3]/60/60 + ptrain.hrs
      ptest.hrs <- ptest.time[3]/60/60 + ptest.hrs
      rm(res_train,res_test,mod.time,ptest.time,ptrain.time)
      gc()
    }
  }
  ## random forest used factor outcome but change back to numeric here
  test.dat$OUTCOME <- as.numeric(levels(test.dat$OUTCOME))[test.dat$OUTCOME]
  ## apply perf.results function to compute performance
  res.mod <- perf.results(pred=test.dat$PRED, outcome=test.dat$OUTCOME, train.pred=test.dat$PRED, inc.plots=FALSE)
  ## store tuning parameters
  res.mod[["mtry"]] <- rf_mod$mtry
  res.mod[["num.trees"]] <- rf_mod$num.trees
  res.mod[["min.node.size"]] <- rf_mod$min.node.size
  ## store cumulative run time statistics
  res.mod[["mod.hrs"]] <- mod.hrs
  res.mod[["ptrain.hrs"]] <- ptrain.hrs
  res.mod[["ptest.hrs"]] <- ptest.hrs
  ## save summary object
  assign(paste("res.mod", i, sep=""), res.mod)
  save(list=paste("res.mod", i, sep=""), file=paste("tune_mh_per_resmod", i, "_e90.Rdata", sep=""))
  rm(list=paste("res.mod", i, sep=""))
  rm(rf_mod,res.mod,test.dat,mod.hrs,ptrain.hrs,ptest.hrs)
  gc()
}


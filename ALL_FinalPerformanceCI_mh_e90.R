
## clear memory
rm(list=ls())
gc()

## load packages
library(data.table)
library(bit64)
library(ROCR)
library(doParallel)
library(doRNG)

## get session information
sessionInfo()

## set working directory
data_dir <- "C:/Users/Administrator/Downloads"
setwd(data_dir)

## source program that provides function perf.boot for computing performance metrics (revised perf.results to facilitate bootstrap)
source("ALL_BootPerformance.R")

## load training and test data for suicide attempts
load("Train_mh_attempt.Rdata")
load("Test_mh_attempt.Rdata")
gc()

## limit to observations with ENR_VISIT==1 and that are not missing suicide attempt outcome of interest
Train_mh_attempt <- Train_mh_attempt[ENR_VISIT==1 & !is.na(EVENT90)]
Test_mh_attempt <- Test_mh_attempt[ENR_VISIT==1 & !is.na(EVENT90)]
gc()

## set keys which also will sort data per those keys
setkeyv(Train_mh_attempt, c("PERSON_ID","VISIT_SEQ"))
setkeyv(Test_mh_attempt, c("PERSON_ID","VISIT_SEQ"))

## create indices
train_indices <- 1:nrow(Train_mh_attempt)
test_indices <- 1:nrow(Test_mh_attempt)

## identify prediction cutpoints in training data that correspond to desired percentiles
cut.rf_sui_att_pred_90 <- quantile(Train_mh_attempt$rf_sui_att_pred_90, c(0.95, 0.99), na.rm=TRUE)
cut.nn_sui_att_pred_90 <- quantile(Train_mh_attempt$nn_sui_att_pred_90, c(0.95, 0.99), na.rm=TRUE)
cut.las_sui_att_pred_90 <- quantile(Train_mh_attempt$las_sui_att_pred_90, c(0.95, 0.99), na.rm=TRUE)
cut.eqwt_all_sui_att_pred_90 <- quantile(Train_mh_attempt$eqwt_all_sui_att_pred_90, c(0.95, 0.99), na.rm=TRUE)
cut.eqwt_rfnn_sui_att_pred_90 <- quantile(Train_mh_attempt$eqwt_rfnn_sui_att_pred_90, c(0.95, 0.99), na.rm=TRUE)
cut.eqwt_rflas_sui_att_pred_90 <- quantile(Train_mh_attempt$eqwt_rflas_sui_att_pred_90, c(0.95, 0.99), na.rm=TRUE)
cut.eqwt_nnlas_sui_att_pred_90 <- quantile(Train_mh_attempt$eqwt_nnlas_sui_att_pred_90, c(0.95, 0.99), na.rm=TRUE)

## register cluster for parallel computing
nodes <- detectCores()
cl <- makeCluster(nodes)
registerDoParallel(cl)

## set desired number of bootstrap samples, seed for reproducibility, and perform bootstrap
nboot <- 10000
set.seed(398)
boot.minutes <- system.time({
  boot.fin <- foreach(i=1:nboot, .combine=rbind) %dorng% {
    boot.data <- Test_mh_attempt[sample(test_indices, replace=TRUE), c("rf_sui_att_pred_90","nn_sui_att_pred_90","las_sui_att_pred_90","EVENT90",
                                                                       "eqwt_all_sui_att_pred_90",
                                                                       "eqwt_rfnn_sui_att_pred_90","eqwt_rflas_sui_att_pred_90","eqwt_nnlas_sui_att_pred_90")]
    boot.rf <- perf.boot(pred = boot.data[!is.na(rf_sui_att_pred_90),rf_sui_att_pred_90],
                         outcome = boot.data[!is.na(rf_sui_att_pred_90),EVENT90],
                         train.pctile = cut.rf_sui_att_pred_90)
    boot.nn <- perf.boot(pred = boot.data[!is.na(nn_sui_att_pred_90),nn_sui_att_pred_90],
                         outcome = boot.data[!is.na(nn_sui_att_pred_90),EVENT90],
                         train.pctile = cut.nn_sui_att_pred_90)
    boot.las <- perf.boot(pred = boot.data[!is.na(las_sui_att_pred_90),las_sui_att_pred_90],
                          outcome = boot.data[!is.na(las_sui_att_pred_90),EVENT90],
                          train.pctile = cut.las_sui_att_pred_90)
    boot.eqwt.all <- perf.boot(pred = boot.data[!is.na(eqwt_all_sui_att_pred_90),eqwt_all_sui_att_pred_90],
                               outcome = boot.data[!is.na(eqwt_all_sui_att_pred_90),EVENT90],
                               train.pctile = cut.eqwt_all_sui_att_pred_90)
    boot.eqwt.rfnn <- perf.boot(pred = boot.data[!is.na(eqwt_rfnn_sui_att_pred_90),eqwt_rfnn_sui_att_pred_90],
                                outcome = boot.data[!is.na(eqwt_rfnn_sui_att_pred_90),EVENT90],
                                train.pctile = cut.eqwt_rfnn_sui_att_pred_90)
    boot.eqwt.rflas <- perf.boot(pred = boot.data[!is.na(eqwt_rflas_sui_att_pred_90),eqwt_rflas_sui_att_pred_90],
                                 outcome = boot.data[!is.na(eqwt_rflas_sui_att_pred_90),EVENT90],
                                 train.pctile = cut.eqwt_rflas_sui_att_pred_90)
    boot.eqwt.nnlas <- perf.boot(pred = boot.data[!is.na(eqwt_nnlas_sui_att_pred_90),eqwt_nnlas_sui_att_pred_90],
                                 outcome = boot.data[!is.na(eqwt_nnlas_sui_att_pred_90),EVENT90],
                                 train.pctile = cut.eqwt_nnlas_sui_att_pred_90)
    boot.res <- rbind(cbind(Model="rf", boot.rf$Performance, AUC=boot.rf$AUC, Brier=boot.rf$'Brier Score'),
                      cbind(Model="nn", boot.nn$Performance, AUC=boot.nn$AUC, Brier=boot.nn$'Brier Score'),
                      cbind(Model="las", boot.las$Performance, AUC=boot.las$AUC, Brier=boot.las$'Brier Score'),
                      cbind(Model="eqwt_all", boot.eqwt.all$Performance, AUC=boot.eqwt.all$AUC, Brier=boot.eqwt.all$'Brier Score'),
                      cbind(Model="eqwt_rfnn", boot.eqwt.rfnn$Performance, AUC=boot.eqwt.rfnn$AUC, Brier=boot.eqwt.rfnn$'Brier Score'),
                      cbind(Model="eqwt_rflas", boot.eqwt.rflas$Performance, AUC=boot.eqwt.rflas$AUC, Brier=boot.eqwt.rflas$'Brier Score'),
                      cbind(Model="eqwt_nnlas", boot.eqwt.nnlas$Performance, AUC=boot.eqwt.nnlas$AUC, Brier=boot.eqwt.nnlas$'Brier Score'))
    return(boot.res)
  }
})[[3]]/60

## stop cluster
stopCluster(cl)
gc()

## compute point estimates
cur.data <- Test_mh_attempt[, c("rf_sui_att_pred_90","nn_sui_att_pred_90","las_sui_att_pred_90","EVENT90",
                                "eqwt_all_sui_att_pred_90",
                                "eqwt_rfnn_sui_att_pred_90","eqwt_rflas_sui_att_pred_90","eqwt_nnlas_sui_att_pred_90")]
cur.rf <- perf.boot(pred = cur.data[!is.na(rf_sui_att_pred_90),rf_sui_att_pred_90],
                    outcome = cur.data[!is.na(rf_sui_att_pred_90),EVENT90],
                    train.pctile = cut.rf_sui_att_pred_90)
cur.nn <- perf.boot(pred = cur.data[!is.na(nn_sui_att_pred_90),nn_sui_att_pred_90],
                    outcome = cur.data[!is.na(nn_sui_att_pred_90),EVENT90],
                    train.pctile = cut.nn_sui_att_pred_90)
cur.las <- perf.boot(pred = cur.data[!is.na(las_sui_att_pred_90),las_sui_att_pred_90],
                     outcome = cur.data[!is.na(las_sui_att_pred_90),EVENT90],
                     train.pctile = cut.las_sui_att_pred_90)
cur.eqwt.all <- perf.boot(pred = cur.data[!is.na(eqwt_all_sui_att_pred_90),eqwt_all_sui_att_pred_90],
                          outcome = cur.data[!is.na(eqwt_all_sui_att_pred_90),EVENT90],
                          train.pctile = cut.eqwt_all_sui_att_pred_90)
cur.eqwt.rfnn <- perf.boot(pred = cur.data[!is.na(eqwt_rfnn_sui_att_pred_90),eqwt_rfnn_sui_att_pred_90],
                           outcome = cur.data[!is.na(eqwt_rfnn_sui_att_pred_90),EVENT90],
                           train.pctile = cut.eqwt_rfnn_sui_att_pred_90)
cur.eqwt.rflas <- perf.boot(pred = cur.data[!is.na(eqwt_rflas_sui_att_pred_90),eqwt_rflas_sui_att_pred_90],
                            outcome = cur.data[!is.na(eqwt_rflas_sui_att_pred_90),EVENT90],
                            train.pctile = cut.eqwt_rflas_sui_att_pred_90)
cur.eqwt.nnlas <- perf.boot(pred = cur.data[!is.na(eqwt_nnlas_sui_att_pred_90),eqwt_nnlas_sui_att_pred_90],
                            outcome = cur.data[!is.na(eqwt_nnlas_sui_att_pred_90),EVENT90],
                            train.pctile = cut.eqwt_nnlas_sui_att_pred_90)
cur.res <- rbind(cbind(Model="rf", cur.rf$Performance, AUC=cur.rf$AUC, Brier=cur.rf$'Brier Score'),
                 cbind(Model="nn", cur.nn$Performance, AUC=cur.nn$AUC, Brier=cur.nn$'Brier Score'),
                 cbind(Model="las", cur.las$Performance, AUC=cur.las$AUC, Brier=cur.las$'Brier Score'),
                 cbind(Model="eqwt_all", cur.eqwt.all$Performance, AUC=cur.eqwt.all$AUC, Brier=cur.eqwt.all$'Brier Score'),
                 cbind(Model="eqwt_rfnn", cur.eqwt.rfnn$Performance, AUC=cur.eqwt.rfnn$AUC, Brier=cur.eqwt.rfnn$'Brier Score'),
                 cbind(Model="eqwt_rflas", cur.eqwt.rflas$Performance, AUC=cur.eqwt.rflas$AUC, Brier=cur.eqwt.rflas$'Brier Score'),
                 cbind(Model="eqwt_nnlas", cur.eqwt.nnlas$Performance, AUC=cur.eqwt.nnlas$AUC, Brier=cur.eqwt.nnlas$'Brier Score'))

## create final object with tidy estimate and 95% bootstrap CI output
fin.res <- cur.res
for(k in c("Sens","Spec","PPV","Fscore","AUC","Brier")){
  if(k=="Brier"){prec <- "%.5f"} else{prec <- "%.3f"}
  fin.res[[k]] <-       paste(sprintf(prec,cur.res[[k]]),
                              " (",
                              sprintf(prec,boot.fin[,lapply(.SD, quantile, 0.025), by=c("Model","Pctile"), .SDcols=k][[k]]),
                              ", ",
                              sprintf(prec,boot.fin[,lapply(.SD, quantile, 0.975), by=c("Model","Pctile"), .SDcols=k][[k]]),
                              ")",
                              sep="")
}

## save final object with tidy estimate and CI output
save(boot.fin, fin.res, boot.minutes, file="Test_mh_attempt90_boot.Rdata")

## display results
fin.res[,c("Model","Pctile","AUC","Brier","Fscore","Sens","Spec","PPV")]
fin.res[Pctile=="99%",c("Model","AUC","Brier","Fscore","Sens","Spec","PPV")]


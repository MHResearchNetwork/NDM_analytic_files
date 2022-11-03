
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

## load final fitted model
load(file="finalRF_mh_per_e90.Rdata")

## random forest used factor outcome but change back to numeric here
res_train$OUTCOME <- as.numeric(levels(res_train$OUTCOME))[res_train$OUTCOME]
res_test$OUTCOME <- as.numeric(levels(res_test$OUTCOME))[res_test$OUTCOME]

## apply perf.results function to compute performance
res.mod.train <- perf.results(pred=res_train$PRED, outcome=res_train$OUTCOME, train.pred=res_train$PRED, inc.plots=FALSE)
res.mod.test <- perf.results(pred=res_test$PRED, outcome=res_test$OUTCOME, train.pred=res_train$PRED, inc.plots=FALSE)

## display results
res.mod.train
res.mod.test

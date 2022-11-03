
## clear memory
rm(list=ls())
gc()

## load packages
library(data.table)
library(bit64)
library(ranger)

## get session information
sessionInfo()

## set working directory
data_dir <- "C:/Users/Administrator/Downloads"
setwd(data_dir)

## loading training and test data
load(file="tune_mh_per_train5fold_e90_fix.Rdata")
gc()
load(file="test_mh_per_e90_fix.Rdata")
gc()

## create vector containing features - training data
feat <- setdiff(colnames(tunedata),c("PERSON_ID","VISIT_SEQ","OUTCOME","CV_PER"))

## create vector containing features - test data
feat_test <- setdiff(colnames(testdata),c("PERSON_ID","VISIT_SEQ","OUTCOME"))

## set selected tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 100
cur.min.node.size <- 25000

## set seed
curseed <- 99282

## fit model
mod.time <- system.time({
  rf_mod <- ranger(dependent.variable.name="OUTCOME", data=tunedata[, c(feat,"OUTCOME"), with=FALSE],
                   num.trees=cur.num.trees, mtry=cur.mtry,
                   importance="none", write.forest=TRUE, probability=TRUE,
                   min.node.size=cur.min.node.size,
                   respect.unordered.factors="partition",
                   oob.error=FALSE, save.memory=FALSE, seed=curseed)
})
gc()

## generate predictions on training data
ptrain.time <- system.time({
  pred_train <- predict(rf_mod, tunedata[, c(feat), with=FALSE])
})
gc()

## generate predictions on test data
ptest.time <- system.time({
  pred_test <- predict(rf_mod, testdata[, c(feat_test), with=FALSE])
})
gc()

## store key fields, predictions, and outcomes
res_train <- cbind(tunedata[, c("PERSON_ID","VISIT_SEQ","OUTCOME","CV_PER"), with=FALSE], PRED=pred_train$predictions[,2])
res_test <- cbind(testdata[, c("PERSON_ID","VISIT_SEQ","OUTCOME"), with=FALSE], PRED=pred_test$predictions[,2])
gc()

## save model and results
save(rf_mod, res_train, res_test, mod.time, ptrain.time, ptest.time,
     file = "finalRF_mh_per_e90.Rdata")
gc()


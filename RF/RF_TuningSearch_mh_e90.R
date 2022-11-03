
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

## load tuning data
load(file="tune_mh_per_train5fold_e90_fix.Rdata")
gc()

## create vector containing features
feat <- setdiff(colnames(tunedata),c("PERSON_ID","VISIT_SEQ","OUTCOME","CV_PER"))


#### Tuning Search - Model 1 ####

## set naming convention for current model
cur.mod.name <- "tune_mh_per_mod1"
cur.mod.outcome <- "e90"

## set tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 10
cur.min.node.size <- 100000

## source program that performs cross-validation and saves results
source("RF_CrossValidation.R")


#### Tuning Search - Model 2 ####

## set naming convention for current model
cur.mod.name <- "tune_mh_per_mod2"
cur.mod.outcome <- "e90"

## set tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 10
cur.min.node.size <- 50000

## source program that performs cross-validation and saves results
source("RF_CrossValidation.R")


#### Tuning Search - Model 3 ####

## set naming convention for current model
cur.mod.name <- "tune_mh_per_mod3"
cur.mod.outcome <- "e90"

## set tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 10
cur.min.node.size <- 25000

## source program that performs cross-validation and saves results
source("RF_CrossValidation.R")


#### Tuning Search - Model 4 ####

## set naming convention for current model
cur.mod.name <- "tune_mh_per_mod4"
cur.mod.outcome <- "e90"

## set tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 10
cur.min.node.size <- 10000

## source program that performs cross-validation and saves results
source("RF_CrossValidation.R")


#### Tuning Search - Model 5 ####

## set naming convention for current model
cur.mod.name <- "tune_mh_per_mod5"
cur.mod.outcome <- "e90"

## set tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 10
cur.min.node.size <- 1000

## source program that performs cross-validation and saves results
source("RF_CrossValidation.R")


#### Tuning Search - Model 6 ####

## set naming convention for current model
cur.mod.name <- "tune_mh_per_mod6"
cur.mod.outcome <- "e90"

## set tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 100
cur.min.node.size <- 100000

## source program that performs cross-validation and saves results
source("RF_CrossValidation.R")


#### Tuning Search - Model 7 ####

## set naming convention for current model
cur.mod.name <- "tune_mh_per_mod7"
cur.mod.outcome <- "e90"

## set tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 100
cur.min.node.size <- 50000

## source program that performs cross-validation and saves results
source("RF_CrossValidation.R")


#### Tuning Search - Model 8 ####

## set naming convention for current model
cur.mod.name <- "tune_mh_per_mod8"
cur.mod.outcome <- "e90"

## set tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 100
cur.min.node.size <- 25000

## source program that performs cross-validation and saves results
source("RF_CrossValidation.R")


#### Tuning Search - Model 9 ####

## set naming convention for current model
cur.mod.name <- "tune_mh_per_mod9"
cur.mod.outcome <- "e90"

## set tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 100
cur.min.node.size <- 10000

## source program that performs cross-validation and saves results
source("RF_CrossValidation.R")


#### Tuning Search - Model 10 ####

## set naming convention for current model
cur.mod.name <- "tune_mh_per_mod10"
cur.mod.outcome <- "e90"

## set tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 100
cur.min.node.size <- 1000

## source program that performs cross-validation and saves results
source("RF_CrossValidation.R")


#### Tuning Search - Model 11 ####

## set naming convention for current model
cur.mod.name <- "tune_mh_per_mod11"
cur.mod.outcome <- "e90"

## set tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 500
cur.min.node.size <- 50000

## source program that performs cross-validation and saves results
source("RF_CrossValidation.R")


#### Tuning Search - Model 12 ####

## set naming convention for current model
cur.mod.name <- "tune_mh_per_mod12"
cur.mod.outcome <- "e90"

## set tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 500
cur.min.node.size <- 25000

## source program that performs cross-validation and saves results
source("RF_CrossValidation.R")


#### Tuning Search - Model 13 ####

## set naming convention for current model
cur.mod.name <- "tune_mh_per_mod13"
cur.mod.outcome <- "e90"

## set tuning parameters: split variables, number of trees, and minimum node size
cur.mtry <- floor(sqrt(length(feat)))
cur.num.trees <- 500
cur.min.node.size <- 10000

## source program that performs cross-validation and saves results
source("RF_CrossValidation.R")


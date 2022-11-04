

# Clearing environment
rm(list=ls())
gc()

# Packages 
library(keras)
library(tictoc)

# Working Directory
setwd("C:/Users/Administrator/Documents")
outdir <- getwd()
b <- 2^10 


load("Test_mh_event90.RData")

model <- load_model_hdf5("MH_event_1024_175.0175.hdf5")
predictions <- model %>% predict_proba(x_test, batch_size = b)
save(predictions, file=paste0("MH_event_predict_test.Rdata"))







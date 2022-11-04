

# Clearing environment
rm(list=ls())
gc()

# Packages 
library(keras)
library(tictoc)
install.packages("writexl")
library("writexl")

# Working Directory
setwd("C:/Users/Administrator/Documents")
outdir <- getwd()

# Functions 
source('~/perf.results.id.R')

## Train 

load(paste0(outdir, "/MH_event__1024_175_predict.Rdata"))
load(paste0(outdir, "/Logit_Preds_MH_event.RData"))


in.perf.0 <- perf.results.id(pred = predictions, outcome = y_train, train.pred = predictions)
Perf <- cbind(in.perf.0$Performance[5, -c(1:2)], in.perf.0$AUC, in.perf.0$`Brier Score`)
colnames(Perf)[12:13] <- c("AUC", "Brier Score")      

Perf <- as.data.frame(Perf)
write_xlsx(Perf, "C:/In-sample-Train-Final-Logit-MH.xlsx")

rm(x_train, predictions, y_train, Perf, in.perf.0, splitn)
gc()

## Test 

predictions <- readRDS("MH_event_predict_test.rds")
load(paste0(outdir, "/Test_mh_per_AWS_event.RData"))


in.perf.0 <- perf.results.id(pred = predictions, outcome = y_test$EVENT90, train.pred = predictions)
Perf <- cbind(in.perf.0$Performance[5, -c(1:2)], in.perf.0$AUC, in.perf.0$`Brier Score`)
colnames(Perf)[12:13] <- c("AUC", "Brier Score")      

Perf <- as.data.frame(Perf)
write_xlsx(Perf, "C:/Out-of-sample-Validation-Final-Logit-MH.xlsx")

rm(x_test, predictions, y_test, Perf, in.perf.0)
gc()

## Top 50 predictors - need colnames and coefficients 

load(paste0(outdir, "/Logit_Preds_MH_event.RData"))

model <- load_model_hdf5("MH_event_1024_175.0175.hdf5")
weights.mod <- get_weights(model)[[1]]
ind.wt <- order(abs(weights.mod), decreasing=TRUE)[1:50]
top.wt <- round(weights.mod[ind.wt,,drop=FALSE], 4)

var.names <- colnames(x_train)[ind.wt]

df <- as.data.frame(cbind(var.names, top.wt))
colnames(df)[2] <- "coefs"

write_xlsx(df, "C:/Top50-Params-Final-Logit-MH.xlsx")









##
## File Created on 3/24/20 by Maricela Cruz
## Last Updated on: 5/28/20
##


rm(list=ls())
gc()


setwd("C:/Users/Administrator/Documents")

# Packages 
library(keras)
library(tictoc)
library(dplyr)
library(data.table)

# Source internally written functions 
source('~/LASSO_functions.R')

# Loading Data
tic()
load("mh_event90.RData")
toc()

lam <- 0.0000146

summary(y_train)
summary(splitn)

# Model Building 'Parameters'
e <- 175                  # epochs
b <- 2^10                 # batches
lr <- 0.001               # learning rate
shape <- dim(x_train)[[2]]  # Number of covariates included in design matrix
gc()


# Setting seed (different from Rod's)
set.seed(84207456)
setwd("C:/Users/Administrator/Documents/MH_Output_Final_Logit")

predictions <- NULL
model <- NULL

filepath <- file.path(getwd(), paste0("MH_event",
                                      "_", b ,
                                      "_", e,
                                      ".{epoch:04d}.hdf5"))


# Create checkpoint callback to store models after each epoch
cp_callback <- callback_model_checkpoint(
  filepath = filepath,
  save_weights_only = F, 
  verbose = 1,           # 0=quiet and 1 is 
  save_freq = "epoch"    # "epoch" saves every single one? can make 10, but then does not save the first one
)

cp_callback2 <- callback_reduce_lr_on_plateau(monitor = "loss", factor = 0.1,
                                              patience = 10,
                                              min_delta = 1e-06)

model <- model_gen_lasso(lambda=lam, shape=shape, lr=lr)

# Fit the actual Model
tic("Logit_MH_event")
model %>% fit(
  x_train, 
  y_train,
  epochs = e, batch_size = b,
  callbacks = list(cp_callback, cp_callback2))

# Make predictions
predictions <- model %>% predict_proba(x_train, batch_size = b)
toc(log=TRUE)


save(predictions, file=paste0("MH_event_",
                              "_", b ,
                              "_", e,
                              "_", lam,
                              "_predict.Rdata"))



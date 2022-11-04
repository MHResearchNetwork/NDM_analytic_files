##
## This file:
##   1. pulls data that has already been 'scrubbed' and standardized 
##   2. fits a 'LASSO' via a one-layer penalized neural network on all the data. 
##   
## In another document, we use this model to make predictions. 
## 
## The main goal: screen variables from the datasets (interactions and overall data)
##  reduce to about 100 and 400 variables, respectively. 
## Once we determine this, we will do a grid search for the lambdas
##  (the penalization param) to select 'optimal' lambda.
## This is for the mental health sample. 
## 

rm(list=ls())

# Packages 
library(keras)
library(tictoc)
library(dplyr)
library(data.table)


tic.clearlog()
gc()

# Source internally written functions 
source('~/LASSO_functions.R')

# Reading in outcome 
y_train <-  readRDS("EVENT90_w_NA.rds")
ind.rm <- which(is.na(y_train))
splitn <- y_train[-ind.rm, 2]
y_train <- y_train[-ind.rm, 1]


# Model Building 'Parameters'
e <- 175                  # epochs
l <- 2.06e-06             # lambda; (Rob's: 9.940568e-07) 
b <- 2^10                 # batches
lr <- 0.001               # learning rate

#######################################################################################

# Loading Training Data
tic()
load("Train_mh_per_AWS_event_Race_Diag2.RData")
toc()
tic()
front <- front[-ind.rm, ]
Interactions <- Interactions[-ind.rm, ]
x_train <- cbind(front, Interactions)
toc()

rm(front, Interactions)
gc()

# Model Building 'Parameters'
shape <- dim(x_train)[2]  # Number of covariates included in design matrix
gc()

set.seed(84207456)

model <- NULL

filepath <- file.path(getwd(), paste0("Race_Diag2_MH",
                                      "_", b ,
                                      "_", e,
                                      "_", l ,
                                      ".hdf5"))


# Create checkpoint callback to store models after each epoch
# cp_callback <- callback_model_checkpoint(
#   filepath = filepath,
#   save_weights_only = F, 
#   verbose = 1,           # 0=quiet and 1 is 
#   save_freq = "epoch"    # "epoch" saves every single one? can make 10, but then does not save the first one
# )

cp_callback2 <- callback_reduce_lr_on_plateau(monitor = "loss", factor = 0.1,
                                              patience = 10,
                                              min_delta = 1e-06)

# Setting up model
model <- model_gen_lasso(lambda=l, shape=shape, lr=lr)

# Fit the actual Model
tic(paste0("screening_Race_Diag2_MH_and_lambda", l))
model %>% fit(
  x_train, 
  y_train,
  epochs = e, batch_size = b,
  callbacks = list(cp_callback2))

save_model_hdf5(model, filepath = filepath)

rm(x_train)
gc()


#######################################################################################

# Loading Training Data
tic()
load("Train_mh_per_AWS_event_gender.RData")
toc()
tic()
front <- front[-ind.rm, ]
Interactions <- Interactions[-ind.rm, ]
x_train <- cbind(front, Interactions)
toc()

rm(front, Interactions)
gc()

# Model Building 'Parameters'
shape <- dim(x_train)[2]  # Number of covariates included in design matrix
gc()

set.seed(84207456)

model <- NULL

filepath <- file.path(getwd(), paste0("gender_MH",
                                      "_", b ,
                                      "_", e,
                                      "_", l ,
                                      ".hdf5"))


# Create checkpoint callback to store models after each epoch
# cp_callback <- callback_model_checkpoint(
#   filepath = filepath,
#   save_weights_only = F, 
#   verbose = 1,           # 0=quiet and 1 is 
#   save_freq = "epoch"    # "epoch" saves every single one? can make 10, but then does not save the first one
# )

cp_callback2 <- callback_reduce_lr_on_plateau(monitor = "loss", factor = 0.1,
                                              patience = 10,
                                              min_delta = 1e-06)

# Setting up model
model <- model_gen_lasso(lambda=l, shape=shape, lr=lr)

# Fit the actual Model
tic(paste0("screening_gender_MH_and_lambda", l))
model %>% fit(
  x_train, 
  y_train,
  epochs = e, batch_size = b,
  callbacks = list(cp_callback2))

save_model_hdf5(model, filepath = filepath)

rm(x_train)
gc()


#######################################################################################

# Loading Training Data
tic()
load("Train_mh_per_AWS_event_PHQ9_PriorSA.RData")
toc()
tic()
PHQ9.Mat2 <- PHQ9.Mat2[-ind.rm, ]
PHQ9.Mat <- PHQ9.Mat[-ind.rm, ]
Interactions <- Interactions[-ind.rm, ]
x_train <- cbind(PHQ9.Mat2, PHQ9.Mat, Interactions)
toc()

rm(front, Interactions)
gc()

# Model Building 'Parameters'
shape <- dim(x_train)[2]  # Number of covariates included in design matrix
gc()

set.seed(84207456)

model <- NULL

filepath <- file.path(getwd(), paste0("PHQ9_PriorSA_MH",
                                      "_", b ,
                                      "_", e,
                                      "_", l ,
                                      ".hdf5"))


# Create checkpoint callback to store models after each epoch
# cp_callback <- callback_model_checkpoint(
#   filepath = filepath,
#   save_weights_only = F, 
#   verbose = 1,           # 0=quiet and 1 is 
#   save_freq = "epoch"    # "epoch" saves every single one? can make 10, but then does not save the first one
# )

cp_callback2 <- callback_reduce_lr_on_plateau(monitor = "loss", factor = 0.1,
                                              patience = 10,
                                              min_delta = 1e-06)

# Setting up model
model <- model_gen_lasso(lambda=l, shape=shape, lr=lr)

# Fit the actual Model
tic(paste0("screening_PHQ9_PriorSA_MH_and_lambda", l))
model %>% fit(
  x_train, 
  y_train,
  epochs = e, batch_size = b,
  callbacks = list(cp_callback2))

save_model_hdf5(model, filepath = filepath)

rm(x_train)
gc()


#######################################################################################

# Loading Training Data
tic()
x_train <- readRDS("Train_mh_per_AWS_enr1.rds")
x_train <- as.matrix(x_train)
x_train <- x_train[-ind.rm, ]
toc()

# Model Building 'Parameters'
shape <- dim(x_train)[2]  # Number of covariates included in design matrix
gc()

set.seed(84207456)

model <- NULL

filepath <- file.path(getwd(), paste0("Main_MH",
                                      "_", b ,
                                      "_", e,
                                      "_", l ,
                                      ".hdf5"))


# Create checkpoint callback to store models after each epoch
# cp_callback <- callback_model_checkpoint(
#   filepath = filepath,
#   save_weights_only = F, 
#   verbose = 1,           # 0=quiet and 1 is 
#   save_freq = "epoch"    # "epoch" saves every single one? can make 10, but then does not save the first one
# )

cp_callback2 <- callback_reduce_lr_on_plateau(monitor = "loss", factor = 0.1,
                                              patience = 10,
                                              min_delta = 1e-06)

# Setting up model
model <- model_gen_lasso(lambda=l, shape=shape, lr=lr)

# Fit the actual Model
tic(paste0("screening_Main_MH_and_lambda", l))
model %>% fit(
  x_train, 
  y_train,
  epochs = e, batch_size = b,
  callbacks = list(cp_callback2))

save_model_hdf5(model, filepath = filepath)

rm(x_train)
gc()

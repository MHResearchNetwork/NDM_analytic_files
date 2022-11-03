rm(list=ls())

# Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
# library(tfruns)
library(keras)
# library(tensorflow)
library(tictoc)
# library(yardstick)
library(dplyr)
library(data.table)
library(glmnet)
library(doParallel)

tic.clearlog()

gc()
# One hidden layers, 4 nodes
setwd("C:/")
source("nnet_functions.R")
source("perf.results.id.R")
source("perf.results.R")

tic("Load Data")
mh_xtrain_90d_attempt_list = readRDS(file="mh_attempt_90D.RDS")

mh_ytrain_90d_attempt <- mh_xtrain_90d_attempt_list[[1]]

toc(log = TRUE)
splitn_mh <- rep(NA, dim(mh_ytrain_90d_attempt)[[1]])
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(0,1)] <- 0
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(2,3)] <- 1
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(4,5)] <- 2
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(6,7)] <- 3
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(8,9)] <- 4

xtrain_mh <- mh_xtrain_90d_attempt_list[[2]]
ytrain_mh <- as.matrix(mh_xtrain_90d_attempt_list[[1]]$EVENT90)
rm(list=c("mh_xtrain_90d_attempt_list", "mh_ytrain_90d_attempt"))
gc()

setwd("C:/Users/Administrator/Documents/")
source()

run_log_90d_attempt <- sapply(1:100, function(x) NULL)
run_count <- 1

set.seed(157987)
nfolds <- 5
e <- 100
l <- 4.457927e-07
b <- 2^10
lr <- 0.001
nnodes <- c(4,8,16)
shape <- dim(xtrain_mh)[[2]]
predictions <- sapply(1:nfolds, function(x) NULL)

for (i in c(0:(nfolds - 1))) {

  model <- NULL
  
  filepath <- file.path(getwd(), paste0("one_hidden_cv_", nfolds, "_", i,
                                        "_", nnodes[[1]] ,
                                        "_", lr ,
                                        "_", e,
                                        "_", b, ".{epoch:04d}.hdf5"))
  
  # Create checkpoint callback to store models after each epoch
  cp_callback <- callback_model_checkpoint(
    filepath = filepath,
    save_freq = 'epoch', save_weights_only = FALSE,
    verbose = 1
  )
  
  model <- model_gen_1hidden_sig(lambda=l, nnodes = nnodes[[1]], learn_rate = lr, shape = shape)
  
  tic(paste0("Model fit fold: ", i, "Nodes: ", nnodes[[1]], "One_hidden"))
  model %>% fit(
    xtrain_mh[splitn_mh != i, ], ytrain_mh[splitn_mh != i, ],
    epochs = e, batch_size = b,
    callbacks = list(cp_callback))
  
  
  predictions[[i + 1]] <- model %>% predict_proba(xtrain_mh, batch_size = b)
  toc(log=TRUE)
  run_log_90d_attempt[[run_count]] <- tic.log(format = TRUE)
  run_count <- run_count + 1
  rm(model)
  gc() 
  
}
save(run_log_90d_attempt, file="mh_run_log_90d_attempt.Rdata")
save(predictions, file=paste0("one_hidden_cv_", nfolds, "_",
                                                           "_", nnodes[[1]] ,
                                                           "_", lr ,
                                                           "_", e,
                                                           "_", b, "_predict.Rdata"))


rm(list=ls())

# Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
# library(tfruns)
library(keras)
# library(tensorflow)
library(tictoc)
# library(yardstick)
library(dplyr)
library(data.table)
# library(glmnet)

tic.clearlog()

gc()

# Two hidden layers, 4 nodes
setwd("C:/Users/Administrator/Documents/")
source("nnet_example_functions.R")

tic("Load Data")
mh_xtrain_90d_attempt_list = readRDS(file="mh_attempt_90D.RDS")
mh_ytrain_90d_attempt <- mh_xtrain_90d_attempt_list[[1]]
toc(log = TRUE)
splitn_mh <- rep(NA, dim(mh_ytrain_90d_attempt)[[1]])
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(0,1)] <- 0
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(2,3)] <- 1
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(4,5)] <- 2
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(6,7)] <- 3
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(8,9)] <- 4

xtrain_mh <- mh_xtrain_90d_attempt_list[[2]]
ytrain_mh <- as.matrix(mh_xtrain_90d_attempt_list[[1]]$EVENT90)
rm(list=c("mh_xtrain_90d_attempt_list", mh_ytrain_90d_attempt))
gc()

xtrain_mh <- mh_xtrain_90d_attempt_list[[2]]
ytrain_mh <- as.matrix(mh_xtrain_90d_attempt_list[[1]]$EVENT90)
rm(list=c("mh_xtrain_90d_attempt_list"))
gc()

run_log_90d_attempt <- sapply(1:100, function(x) NULL)
run_count <- 1

set.seed(928)
nfolds <- 5
e <- 100
l <- 4.457927e-07
b <- 2^10
lr <- 0.001
nnodes <- c(4, 8,16)
shape <- dim(xtrain_mh)[[2]]


predictions <- sapply(1:nfolds, function(x) NULL)
gc()

for (i in c(0:(nfolds - 1))) {
  model <- NULL
  
  filepath <- file.path(getwd(), paste0("two_hidden_cv_", nfolds,"_", i,
                                        "_", nnodes[[1]] ,
                                        "_", lr ,
                                        "_", e,
                                        "_", b, ".{epoch:04d}.hdf5"))
  
  # Create checkpoint callback to store models after each epoch
  cp_callback <- callback_model_checkpoint(
    filepath = filepath,
    save_freq = 'epoch', save_weights_only = FALSE,
    verbose = 1
  )
  
  model <- model_gen_2hidden_sig(lambda=l, nnodes = nnodes[[1]], learn_rate = lr, shape = shape)
  
  tic(paste0("Model fit fold: ", i, "Nodes: ", nnodes[[1]], "two_hidden"))
  model %>% fit(
    xtrain_mh[splitn_mh != i, ], ytrain_mh[splitn_mh != i, ],
    epochs = e, batch_size = b,
    callbacks = list(cp_callback))
  
  
  predictions[[i + 1]] <- model %>% predict_proba(xtrain_mh, batch_size = b)
  toc(log=TRUE)
  run_log_90d_attempt[[run_count]] <- tic.log(format = TRUE)
  run_count <- run_count + 1
  rm(model)
  gc() 
}
save(run_log_90d_attempt, file="mh_run_log_90d_attempt.Rdata")
save(predictions, file=paste0("two_hidden_cv_", nfolds, "_",
                                                           "_", nnodes[[1]] ,
                                                           "_", lr ,
                                                           "_", e,
                                                           "_", b, "_predict.Rdata"))


rm(list=ls())
gc()
# Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
# library(tfruns)
library(keras)
# library(tensorflow)
library(tictoc)
# library(yardstick)
library(dplyr)
library(data.table)
# library(glmnet)

tic.clearlog()

gc()

# One hidden layers, 8 nodes
setwd("C:/")
source("nnet_functions.R")
tic("Load Data")
mh_xtrain_90d_attempt_list = readRDS(file="mh_attempt_90D.RDS")
mh_ytrain_90d_attempt <- mh_xtrain_90d_attempt_list[[1]]
toc(log = TRUE)
splitn_mh <- rep(NA, dim(mh_ytrain_90d_attempt)[[1]])
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(0,1)] <- 0
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(2,3)] <- 1
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(4,5)] <- 2
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(6,7)] <- 3
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(8,9)] <- 4

xtrain_mh <- mh_xtrain_90d_attempt_list[[2]]
ytrain_mh <- as.matrix(mh_xtrain_90d_attempt_list[[1]]$EVENT90)
rm(list=c("mh_xtrain_90d_attempt_list", mh_ytrain_90d_attempt))
gc()

xtrain_mh <- mh_xtrain_90d_attempt_list[[2]]
ytrain_mh <- as.matrix(mh_xtrain_90d_attempt_list[[1]]$EVENT90)
rm(list=c("mh_xtrain_90d_attempt_list"))
gc()

run_log_90d_attempt <- sapply(1:100, function(x) NULL)
run_count <- 1

set.seed(15978)
nfolds <- 5
e <- 100
l <- 4.457927e-07
b <- 2^10
lr <- 0.001
nnodes <- c(4, 8, 16)
shape <- dim(xtrain_mh)[[2]]

predictions <- sapply(1:nfolds, function(x) NULL)

for (i in c(0:(nfolds - 1))) {
  
  model <- NULL
  
  filepath <- file.path(getwd(), paste0("one_hidden_cv_", nfolds, "_", i,
                                        "_", nnodes[[2]] ,
                                        "_", lr ,
                                        "_", e,
                                        "_", b, ".{epoch:04d}.hdf5"))
  
  # Create checkpoint callback to store models after each epoch
  cp_callback <- callback_model_checkpoint(
    filepath = filepath,
    save_freq = 'epoch', save_weights_only = FALSE,
    verbose = 1
  )
  
  model <- model_gen_1hidden_sig(lambda=l, nnodes = nnodes[[2]], learn_rate = lr, shape = shape)
  
  tic(paste0("Model fit fold: ", i, "Nodes: ", nnodes[[2]], "one_hidden"))
  model %>% fit(
    xtrain_mh[splitn_mh != i, ], ytrain_mh[splitn_mh != i, ],
    epochs = e, batch_size = b,
    callbacks = list(cp_callback))
  
  
  predictions[[i + 1]] <- model %>% predict_proba(xtrain_mh, batch_size = b)
  toc(log=TRUE)
  run_log_90d_attempt[[run_count]] <- tic.log(format = TRUE)
  run_count <- run_count + 1
  rm(model)
  gc() 
}
save(run_log_90d_attempt, file="mh_run_log_90d_attempt.Rdata")
save(predictions, file=paste0("one_hidden_cv_", nfolds, "_",
                                                           "_", nnodes[[2]] ,
                                                           "_", lr ,
                                                           "_", e,
                                                           "_", b, "_predict.Rdata"))


rm(list=ls())

# Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
# library(tfruns)
library(keras)
# library(tensorflow)
library(tictoc)
# library(yardstick)
library(dplyr)
library(data.table)
# library(glmnet)

tic.clearlog()

gc()

# Two hidden layers, 8 nodes
setwd("C:/Users/Administrator/Documents/")
source("nnet_example_functions.R")
tic("Load Data")
mh_xtrain_90d_attempt_list = readRDS(file="mh_attempt_90D.RDS")
mh_ytrain_90d_attempt <- mh_xtrain_90d_attempt_list[[1]]
toc(log = TRUE)
splitn_mh <- rep(NA, dim(mh_ytrain_90d_attempt)[[1]])
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(0,1)] <- 0
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(2,3)] <- 1
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(4,5)] <- 2
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(6,7)] <- 3
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(8,9)] <- 4

xtrain_mh <- mh_xtrain_90d_attempt_list[[2]]
ytrain_mh <- as.matrix(mh_xtrain_90d_attempt_list[[1]]$EVENT90)
rm(list=c("mh_xtrain_90d_attempt_list", mh_ytrain_90d_attempt))
gc()

xtrain_mh <- mh_xtrain_90d_attempt_list[[2]]
ytrain_mh <- as.matrix(mh_xtrain_90d_attempt_list[[1]]$EVENT90)
rm(list=c("mh_xtrain_90d_attempt_list"))
gc()

run_log_90d_attempt <- sapply(1:100, function(x) NULL)
run_count <- 1

# Two hidden layers, 8 nodes
set.seed(55674)
nfolds <- 5
e <- 100
l <- 4.457927e-07
b <- 2^10
lr <- 0.001
nnodes <- c(4, 8,16)
shape <- dim(xtrain_mh)[[2]]

predictions <- sapply(1:nfolds, function(x) NULL)

for (i in c(0:(nfolds - 1))) {
  nrows <- sum(table(splitn_mh)[-(i+1)])
  model <- NULL
  
  filepath <- file.path(getwd(), paste0("two_hidden_cv_", nfolds, "_", i,
                                        "_", nnodes[[2]] ,
                                        "_", lr ,
                                        "_", e,
                                        "_", b, ".{epoch:04d}.hdf5"))
  
  # Create checkpoint callback to store models after each epoch
  cp_callback <- callback_model_checkpoint(
    filepath = filepath,
    save_freq = 'epoch', save_weights_only = FALSE,
    verbose = 1
  )
  
  model <- model_gen_2hidden_sig(lambda=l, nnodes = nnodes[[2]], learn_rate = lr, shape = shape)
  
  tic(paste0("Model fit fold: ", i, "Nodes: ", nnodes[[2]], "two_hidden"))
  model %>% fit(
    xtrain_mh[splitn_mh != i, ], ytrain_mh[splitn_mh != i, ],
    epochs = e, batch_size = b,
    callbacks = list(cp_callback))
  
  
  predictions[[i + 1]] <- model %>% predict_proba(xtrain_mh, batch_size = b)
  toc(log=TRUE)
  run_log_90d_attempt[[run_count]] <- tic.log(format = TRUE)
  run_count <- run_count + 1
  rm(model)
  gc() 
}

save(run_log_90d_attempt, file="mh_run_log_90d_attempt.Rdata")
save(predictions, file=paste0("two_hidden_cv_", nfolds, "_",
                                                           "_", nnodes[[2]] ,
                                                           "_", lr ,
                                                           "_", e,
                                                           "_", b, "_predict.Rdata"))


rm(list=ls())

# Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
# library(tfruns)
library(keras)
# library(tensorflow)
library(tictoc)
# library(yardstick)
library(dplyr)
library(data.table)
# library(glmnet)

tic.clearlog()

gc()

# Two hidden layers, 16 nodes

setwd("C:/Users/Administrator/Documents/")
source("nnet_example_functions.R")
tic("Load Data")
mh_xtrain_90d_attempt_list = readRDS(file="mh_attempt_90D.RDS")
mh_ytrain_90d_attempt <- mh_xtrain_90d_attempt_list[[1]]
toc(log = TRUE)
splitn_mh <- rep(NA, dim(mh_ytrain_90d_attempt)[[1]])
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(0,1)] <- 0
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(2,3)] <- 1
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(4,5)] <- 2
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(6,7)] <- 3
splitn_mh[mh_ytrain_90d_attempt$CV_PER %in% c(8,9)] <- 4

xtrain_mh <- mh_xtrain_90d_attempt_list[[2]]
ytrain_mh <- as.matrix(mh_xtrain_90d_attempt_list[[1]]$EVENT90)
rm(list=c("mh_xtrain_90d_attempt_list", mh_ytrain_90d_attempt))
gc()

xtrain_mh <- mh_xtrain_90d_attempt_list[[2]]
ytrain_mh <- as.matrix(mh_xtrain_90d_attempt_list[[1]]$EVENT90)
rm(list=c("mh_xtrain_90d_attempt_list"))
gc()

run_log_90d_attempt <- sapply(1:100, function(x) NULL)
run_count <- 1

set.seed(216854)
nfolds <- 5
e <- 100
l <- 4.457927e-07
b <- 2^10
lr <- 0.001
nnodes <- c(4, 8, 16)
shape <- dim(xtrain_mh)[[2]]

predictions <- sapply(1:nfolds, function(x) NULL)

for (i in c(0:(nfolds - 1))) {
  nrows <- sum(table(splitn_mh)[-(i+1)])
  model <- NULL
  
  filepath <- file.path(getwd(), paste0("two_hidden_cv_", nfolds, "_", i,
                                        "_", nnodes[[3]] ,
                                        "_", lr ,
                                        "_", e,
                                        "_", b, ".{epoch:04d}.hdf5"))
  
  # Create checkpoint callback to store models after each epoch
  cp_callback <- callback_model_checkpoint(
    filepath = filepath,
    save_freq = 'epoch', save_weights_only = FALSE,
    verbose = 1
  )
  
  model <- model_gen_2hidden_sig(lambda=l, nnodes = nnodes[[3]], learn_rate = lr, shape = shape)
  
  tic(paste0("Model fit fold: ", i, "Nodes: ", nnodes[[3]], "two_hidden"))
  model %>% fit(
    xtrain_mh[splitn_mh != i, ], ytrain_mh[splitn_mh != i, ],
    epochs = e, batch_size = b,
    callbacks = list(cp_callback))
  
  
  predictions[[i + 1]] <- model %>% predict_proba(xtrain_mh, batch_size = b)
  toc(log=TRUE)
  save(predictions, file=paste0("two_hidden_cv_", nfolds, "_",
                                "_", nnodes[[3]] ,
                                "_", lr ,
                                "_", e,
                                "_", b, "_predict.Rdata"))
  run_log_90d_attempt[[run_count]] <- tic.log(format = TRUE)
  run_count <- run_count + 1
  rm(model)
  gc() 
}

save(run_log_90d_attempt, file="mh_run_log_90d_attempt.Rdata")
save(predictions, file=paste0("two_hidden_cv_", nfolds, "_",
                              "_", nnodes[[3]] ,
                              "_", lr ,
                              "_", e,
                              "_", b, "_predict.Rdata"))

gc()

model_names <- rep(c("1 Hidden, 4 Nodes", "2 Hidden, 4 Nodes", "1 Hidden, 8 Nodes", "2 Hidden, 8 Nodes"), each=5)
runtime <- data.frame(model_names, seconds=as.numeric(substr(tic.log()[-1], 19, 25)))
save(runtime, file = paste0("runtime_", gsub(":", "_", gsub(" ", "_", date())), ".Rdata"))

# Compile preliminary CV results
fileflag <- grepl("predict", list.files())
fnames <- list.files()[fileflag][4:8]
nfiles <- length(fnames)
predictions <- NULL
res_test <- sapply(1:nfiles, function(X) NULL)
res_train <- sapply(1:nfiles, function(X) NULL)
for (i in 1:nfiles) {
  load(fnames[i])
  res_test[[i]] <- lapply(0:4, function(X) {
    if(i == 1) {
      perf.results(predictions[[i]][splitn_mh==X, 1], 
                   ytrain_mh[splitn_mh==X,1], predictions[[i]][splitn_mh!=X, 1])
    } else{
      perf.results(predictions[[i]][splitn_mh==X, 1], 
                 ytrain_mh[splitn_mh==X,1], predictions[[i]][splitn_mh!=X, 1])
    }
    
  })
  res_train[[i]] <- lapply(0:4, function(X) {
    if(i==1) {
      perf.results(predictions[[i]][splitn_mh!=X, 1], 
                   ytrain_mh[splitn_mh!=X,1], predictions[[i]][splitn_mh!=X, 1])
    } else{
      perf.results(predictions[[i]][splitn_mh!=X, 1], 
                 ytrain_mh[splitn_mh!=X,1], predictions[[i]][splitn_mh!=X, 1])
    }
    
  })
  rm(predictions)
}


# Function to compile performance results
crunch_nnet_results <- function(res) {
  # browser()
  mean_avgpredrisk <- do.call(c, lapply(1:5, function(i) {
    apply(do.call(cbind, lapply(1:5, function(j) {
      res[[i]][[j]]$Calibration[, 2]
    })), 1, mean)}))
  mean_obspredrisk <- do.call(c, lapply(1:5, function(i) {
    apply(do.call(cbind, lapply(1:5, function(j) {
      res[[i]][[j]]$Calibration[, 3]
    })), 1, mean)}))
  
  
  
  calibration<- cbind("Average Predicted"=mean_avgpredrisk, "Observed"=mean_obspredrisk)
  rownames(calibration) <- as.matrix(rbind(res[[1]][[1]]$Calibration[,1], res[[1]][[1]]$Calibration[,1],
                                           res[[1]][[1]]$Calibration[,1], res[[1]][[1]]$Calibration[,1], 
                                           res[[1]][[1]]$Calibration[,1]))
  
  mean_avgperf <- lapply(1:5, function(i) {
    this <- lapply(1:5, function(j) {
      as.matrix(res[[i]][[j]]$Performance[, -1])
    })
    thisaverage <- Reduce('+', this)/length(this)
    rownames(thisaverage) <- as.matrix(res[[1]][[1]]$Performance)[, 1]
    thisaverage
  })
  names(mean_avgperf) <- fnames
  
  mean_AUC <- lapply(1:5, function(i) {
    this <- lapply(1:5, function(j) {
      res[[i]][[j]]$AUC[[1]]
    })
    Reduce('+', this)/length(this)
  })
  
  mean_test_Brier <- lapply(1:5, function(i) {
    this <- lapply(1:5, function(j) {
      res[[i]][[j]]$`Brier Score`[[1]]
    })
    Reduce('+', this)/length(this)
  })
  list(calibration, do.call(rbind, mean_avgperf))
}





# Compute and save CV in and out of sample performance
test_results <- list(fnames, crunch_nnet_results(res_test))
train_results <- list(fnames, crunch_nnet_results(res_train))
save(test_results, file = "test_results_mh_90d_attempt.Rdata")
save(train_results, file = "train_results_mh_90d_attempt.Rdata")



# Fit final model to training data and predict in testing data
nfolds <- 5
e <- 100
l <- 5.27712e-07
b <- 2^10
lr <- 0.001
nnodes <- c(4, 8)
shape <- dim(xtrain_mh)[[2]]

model <- NULL

filepath <- file.path(getwd(), paste0("two_hidden_win_", nfolds, "_", i,
                                      "_", nnodes[[2]] ,
                                      "_", lr ,
                                      "_", e,
                                      "_", b, ".{epoch:04d}.hdf5"))

# Create checkpoint callback to store models after each epoch
cp_callback <- callback_model_checkpoint(
  filepath = filepath,
  save_freq = 'epoch', save_weights_only = FALSE,
  verbose = 1
)

model <- model_gen_2hidden_sig(lambda=l, nnodes = nnodes[[2]], learn_rate = lr, shape = shape)

tic(paste0("Winner: ", i, "Nodes: ", nnodes[[2]], "two_hidden"))
model %>% fit(
  xtrain_mh, ytrain_mh[ , 1],
  epochs = e, batch_size = b,
  callbacks = list(cp_callback))


predictions_win <- model %>% predict_proba(xtrain_mh, batch_size = b)
toc(log=TRUE)
save(predictions_win, file=paste0("two_hidden_win",
                              "_", nnodes[[2]] ,
                              "_", lr ,
                              "_", e,
                              "_", b, "_predict.Rdata"))

mh_train_90d_attempt_2H8N <- perf.results(predictions_win, ytrain_mh, predictions_win)
save(mh_train_90d_attempt_2H8N, file="mh_train_90d_attempt_2H8N.Rdata")

xtest_mh <- mh_xtest_90d_attempt_list[[2]]
ytest_mh <- mh_xtest_90d_attempt_list[[1]]$EVENT90
rm(mh_xtest_90d_attempt_list)
gc()
predictions_win_test <- model %>% predict_proba(xtest_mh, batch_size = b)
save(predictions_win_test, file=paste0("two_hidden_win_test",
                                  "_", nnodes[[2]] ,
                                  "_", lr ,
                                  "_", e,
                                  "_", b, "_predict.Rdata"))


# Final results for winning model
install.packages("bit64")
install.packages("ranger")
library(bit64)
library(ranger)
EventIDs <- fread("~/SuiAttemptIDs.csv", key=c("PERSON_ID","VISIT_SEQ"))

source(file="~/perf.results.id.R")
## merge unique suicide attempt or suicide attempt ids and set ids to missing when OUTCOME==0
res_train <- merge(y_train[, 1:2, with=F], 
                   EventIDs, by=c("PERSON_ID", "VISIT_SEQ"), all.x=TRUE)
res_train[as.matrix(y_train[, "EVENT90", with=F])[,1]==0, ATTEMPT_ID := NA]
res.mod.train <- perf.results.id(pred=predictions_win[,1], outcome=as.vector(y_train$EVENT90), 
                                 outcomeID=res_train$ATTEMPT_ID, 
                                 train.pred=predictions_win[,1])


write.csv(cbind(res.mod.train$Performance, AUC=res.mod.train$AUC, Brier=res.mod.train$`Brier Score`),
          file = "~/final_train_performance_mh_90d_attempt.csv")
write.csv(res.mod.train$Calibration, file = "~/final_train_calibration_mh_90d_attempt.csv")


res_test <- merge(y_test[, 1:2, with=F], 
                  EventIDs, by=c("PERSON_ID", "VISIT_SEQ"), all.x=TRUE)
res_test[as.matrix(y_test[, "EVENT90", with=F])[,1]==0, ATTEMPT_ID := NA]
res.mod.test <- perf.results.id(pred=predictions_win_test[,1], outcome=as.vectory_test$EVENT90, 
                                outcomeID=res_test$ATTEMPT_ID, 
                                train.pred=predictions_win[,1])

write.csv(cbind(res.mod.test$Performance, AUC=res.mod.test$AUC, Brier=res.mod.test$`Brier Score`),
          file = "~/final_test_performance_mh_90d_attempt.csv")
write.csv(res.mod.test$Calibration, file = "~/final_test_calibration_mh_90d_attempt.csv")










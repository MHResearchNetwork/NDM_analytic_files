
rm(list=ls())
gc()

ind <- 3:5

setwd("C:/Users/Administrator/Documents")

# Packages 
library(keras)
library(tictoc)
library(dplyr)
library(data.table)

# Source internally written functions 
source('~/LASSO_functions.R')

lam.vec <- c(0.000000583,
             0.000000924,
             0.00000368,
             0.00000583,
             0.00000924,
             0.0000146,
             0.0000232,
             0.0000368,
             0.0000583,
             0.0000924)


# Loading Data
tic()
load("mh_event90.RData")
toc()

head(y_train)
head(splitn)

summary(y_train)
summary(splitn)
gc()

# Model Building 'Parameters'
e <- 175                  # epochs
b <- 2^10                 # batches
lr <- 0.001               # learning rate
shape <- dim(x_train)[[2]]  # Number of covariates included in design matrix
gc()

Time.CV <- c()

start <- Sys.time()
for(j in ind){
  
  l <- lam.vec[j]
  
  # Setting seed (different from Rod's)
  set.seed(84207456)
  setwd(paste0("C:/Users/Administrator/Documents/MH_Output_", j))
  
  predictions <- sapply(1:5, function(x) NULL)
  
  i <- 0
  while(i < 5){
    model <- NULL
    
    filepath <- file.path(getwd(), paste0("MH_event_", i,
                                          "_", b ,
                                          "_", e,
                                          "_", l ,
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
    
    model <- model_gen_lasso(lambda=l, shape=shape, lr=lr)
    
    # Fit the actual Model
    tic(paste0("screening_MH_and_lambda", l))
    model %>% fit(
      x_train[splitn != i, ], 
      y_train[splitn != i],
      epochs = e, batch_size = b,
      callbacks = list(cp_callback, cp_callback2))
    
    # Make predictions
    predictions[[i + 1]] <- model %>% predict_proba(x_train, batch_size = b)
    
    if(min(predictions[[i + 1]]) == max(predictions[[i + 1]])){
      summary(predictions[[i + 1]])
      paste( "ERROR at fold", i, "Try resetting seed")
      set.seed(12573)
      next
    }
    toc(log=TRUE)
    
    rm(model) # final model has been saved above in cp_callback
    
    gc()
    
    i <- i + 1
  }
  
  save(predictions, file=paste0("MH_event_",
                                "_", b ,
                                "_", e,
                                "_", l , "_predict.Rdata"))
  
  rm(predictions)
  gc()
  
  setwd("C:/Users/Administrator/Documents")
  end <- Sys.time()
  
  Time.CV <- cbind(Time.CV, c(end-start, lam.vec[j]))
}

saveRDS(Time.CV, file ="Total_run_Time_3-5.rds")


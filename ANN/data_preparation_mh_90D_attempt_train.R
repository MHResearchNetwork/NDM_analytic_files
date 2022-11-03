### Prepare training data for neural net model, SRS3 Model Comparisons
### Author: Rob Wellman (robert.d.wellman@kp.org)


rm(list=ls())

# Run to avoid using GPU if necessary
# Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)

library(keras)
library(tictoc)
library(data.table)
library(dplyr)

# Set path to raw training and test data

setwd("C:/")

## Source custom function library
source('nnet_functions.R')

# Load de-identified training data
tic.clearlog()
tic("Total Time")
datdir <-"C:/"

tic("load data")
load(paste0(datdir, "/Train_mh_per_AWS.Rdata"))
toc(log = TRUE)

# Initialized lists to hold variable mins and maxes
mins <- sapply(1:9, function(x) NULL)
maxs <- sapply(1:9, function(x) NULL)

# Load separate datasets for each CV fold
tic("transform data")
factors <- NULL
int_cats <- NULL
for (i in 0:9) {
  fname <- paste0("Train_mh_per_AWS_", i)
  
  # SUbset full data set by CV split and remove missing event data and missing sex
  assign(fname, Train_mh_per_AWS[CV_PER == i & !is.na(EVENT90) & SEX %in% c("M", "F")])

  # Create outcome table per CV split
  assign(paste0("y_train", i), get(fname)[,c("EVENT90")])
  
  # Remove unused variables with data.table
  indx <- which(names(Train_mh_per_AWS)%in%c("PERSON_ID","VISIT_SEQ","CV_PER",
                                                                "EVENT30", "EVENT90", "EVENT180",
                                                                "DEATH30", "DEATH90", "DEATH180",
                                                                "DEF_DEATH30", "DEF_DEATH90", "DEF_DEATH180"))
  get(fname)[, (indx) := NULL]
  
  # Identify factor variables and separate from other covariates
  factor_indx <- grep("factor", sapply(get(fname), class))
  factors <- rbindlist(list(factors, get(fname)[, factor_indx, with=F]))
  get(fname)[, (factor_indx) := NULL]
  
  # Identify integer class variables
  vclasses <- sapply(colnames(get(fname)), function(X) class(get(fname)[[X]]))

  vnumlevels <- sapply(colnames(get(fname)), function(X) n_distinct(get(fname)[[X]]))
  
  indx <- vclasses=="integer" & vnumlevels==3
  
  # Identify variables coded as c(-1, 0, 1)
  integers_categorical <- sapply(colnames(get(fname)[, ..indx]), function(X) {
    ifelse(all.equal(c(-1,0,1), sort(unique(get(fname)[, ..indx][[X]])))==TRUE, TRUE, FALSE) 
  })
  
  integers_categorical <- which(names(get(fname)) %in%  names(integers_categorical[integers_categorical==TRUE]))
  int_cats <- rbindlist(list(int_cats, get(fname)[, integers_categorical, with=F]))
  get(fname)[, (integers_categorical) := NULL]
  
  # rm(list = fname)
  
  mins[[i+1]] <- sapply(1:ncol(get(fname)),
                        function(c) min(get(fname)[, c, with=F]))
  maxs[[i+1]] <- sapply(1:ncol(get(fname)),
                        function(c) max(get(fname)[, c, with=F]))
}
toc(log = TRUE)
gc()
allmin <- apply(do.call(rbind, mins), 2, min)
allmax <- apply(do.call(rbind, maxs), 2, max)
allrange <- allmax - allmin

# Remove variables which have no variability.
var_has_one_val <- which(allrange == 0)
for (i in 0:9) {
  get(paste0("Train_mh_per_AWS_", i))[, (var_has_one_val):=NULL]
}

# Remove variables from maxes
allmin <- allmin[-var_has_one_val]
allmax <- allmax[-var_has_one_val]
allrange <- allrange[-var_has_one_val]

# Make indicators for factor variables
library(fastDummies)
numfactors <- dim(factors)[[2]]
numintcats <- dim(int_cats)[[2]]
factors <- dummy_cols(factors, remove_most_frequent_dummy = TRUE)[, -c(1:numfactors), with = F]
int_cats <- dummy_cols(int_cats, remove_most_frequent_dummy = TRUE, 
                       select_columns = names(int_cats))[, -c(1:numintcats), with = F]

# Compile CV datasets 
tic()
splits_train <- c(1:10)

splitn <- rep(0:9, times = lapply(ls(pattern="Train_mh_per_AWS_"), function(X) dim(get(X))[[1]]))

x_train <- rbindlist(mget(ls(pattern = "Train_mh_per_AWS_")[splits_train]))

ytrain <- data.matrix(rbindlist(mget(ls(pattern = "y_train")[splits_train])))

toc()
gc()

# Normalize continuous variables
tic("normalize variables")
for (j in 1:ncol(x_train)) set(x_train, j = j, value = (x_train[[j]] - allmin[j])/(allmax[j]-allmin[j]))
toc(log = TRUE)

# House keeping.
rm(list=ls(pattern="Train_mh_per_AWS_"))
rm(list=ls(pattern="y_train"))
gc()
rm(list=c('mins', 'maxs', 
          'allmax', 'allmin', 'allrange', 'i', 'j', 
          'integers_categorical', 'numfactors', 'numintcats', 'indx',
          'factor_indx', 'splits_train', 
          'var_has_one_val', 'vclasses', 'vnumlevels'))
gc()

tic("Output final model matrix")
xtrain <- as.matrix(bind_cols(x_train, factors, int_cats))
toc(log = TRUE)

rm(list=c('x_train','factors', 'int_cats', 'Train_mh_per_AWS'))

gc()

toc(log = TRUE)

rm(list=ls(pattern="Train_mh_per_AWS_"))
rm(list=ls(pattern="y_train")[-1])
gc()

# Final Training and Test Sets
x_train <- bind_cols(x_train, factors[splitn %in% c(2:9),])
toc()

rm(factors)
gc()

tic()
x_train <- as.matrix(x_train)
toc()
gc()

#Save final training dataset
saveRDS(list(xtrain=x_train, ytrain=y_train, splitn), "mh_attempt_90D.RDS")
# Create test (validation) dataset for NNET model development
# Author: Robert Wellman (robert.d.wellman@kp.org)

rm(list=ls())

# Run to avoid using GPU if necessary
# Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)

library(keras)
library(data.table)
library(dplyr)
library(tictoc)

# Load test data for processing
datdir <-"C:/"
load("~/Test_mh_per_d90_AWS.Rdata")

# Initialize lists

factors <- NULL
int_cats <- NULL

fname <- paste0("Test_mh_per_AWS_temp")
Test_mh_per_AWS_temp <- Test_mh_per_d90_AWS

# Test_mh_per_AWS_temp[, var_names_test, with=F]

# SUbset full data set by CV split and remove missing event data and missing sex
Test_mh_per_AWS_temp <- subset(get(fname), !is.na(EVENT90))
indx_idvars <- which(colnames(Test_mh_per_AWS_temp) %in% c("PERSON_ID","VISIT_SEQ", "CV_PER"))
mh_ytest_90d_attempt <- Test_mh_per_AWS_temp[, c("PERSON_ID","VISIT_SEQ", "EVENT90")]

save(mh_ytest_90d_death, file = paste0(datdir, "/mh_ytest_90d_attempt.Rdata"))

# Remove unused variables with data.table"PERSON_ID","VISIT_SEQ",
indx <- which(names(get(fname))%in%c("EVENT30", "EVENT90", "EVENT180", "ENR_CALC",
                                     "DEATH30", "DEATH90", "DEATH180",
                                     "DEF_DEATH30", "DEF_DEATH90", "DEF_DEATH180"))

get(fname)[, (indx) := NULL]

# Identify factor variables and separate from other covariates
factor_indx <- grep("factor", sapply(get(fname), class))
factors <- rbindlist(list(factors, get(fname)[, factor_indx, with=F]))
get(fname)[,(factor_indx) := NULL]

# Identify integer class variables
vclasses <- get(fname)[, lapply(.SD, function(x) class(x)), .SDcols = 1:ncol(get(fname))]
vnumlevels <- get(fname)[, lapply(.SD, function(x) n_distinct(x)), .SDcols = 1:ncol(get(fname))]

indx <- vclasses=="integer" & vnumlevels==3

# Identify variables coded as c(-1, 0, 1)
integers_categorical <- sapply(colnames(get(fname)[, ..indx]), function(X) {
  ifelse(all.equal(c(-1,0,1), sort(unique(get(fname)[, ..indx][[X]])))==TRUE, TRUE, FALSE) 
})

integers_categorical <- which(names(get(fname)) %in%  names(integers_categorical[integers_categorical==TRUE]))
int_cats <- rbindlist(list(int_cats, get(fname)[, integers_categorical, with=F]))
get(fname)[, (integers_categorical) := NULL]

indx_idvars <- which(colnames(Test_mh_per_AWS_temp) %in% c("PERSON_ID", "VISIT_SEQ", "CV_PER"))

get(fname)[, (indx_idvars) := NULL]

# Load mins and maxes form training data and single value variables.
load("mh_90d_attempt_min_max.Rdata")
load("mh_90d_attempt_var_one_value.Rdata")

Test_mh_per_AWS_temp[, (vars_with_one_val):=NULL]

allmin <- as.numeric(min_max_train_mh_90d_death[[1]])
allmax <- as.numeric(min_max_train_mh_90d_death[[2]])


# Make indicators for factor variables
# install.packages("fastDummies")
library(fastDummies)
numfactors <- dim(factors)[[2]]
numintcats <- dim(int_cats)[[2]]
factors <- dummy_cols(factors, remove_most_frequent_dummy = TRUE)[, -c(1:numfactors), with = F]
int_cats <- dummy_cols(int_cats, remove_most_frequent_dummy = TRUE, 
                       select_columns = names(int_cats))[, -c(1:numintcats), with = F]

tic()

# x_test <- Test_mh_per_AWS_temp

toc()
gc()

# Normalize continuous variables
tic("normalize variables")
for (j in 1:ncol(Test_mh_per_AWS_temp)) {
  set(Test_mh_per_AWS_temp, j = j, 
      value = (Test_mh_per_AWS_temp[[j]] - allmin[[j]])/(allmax[[j]]-allmin[[j]]))
}

toc(log = TRUE)

tic("Output final model matrix")
mh_xtest_90d_attempt <- as.matrix(bind_cols(Test_mh_per_AWS_temp, factors, int_cats))
toc(log = TRUE)

mh_xtest_90d_death_list <- list(mh_ytest_90d_attempt, mh_xtest_90d_attempt)
save(mh_xtest_90d_attempt_list, file="~/mh_xtest_90d_attempt.Rdata")



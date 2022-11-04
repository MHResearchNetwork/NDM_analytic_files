##
## This file:
##   1. pulls data that has already been 'scrubbed' and standardized 
##   2. fits a 'LASSO' via glmnet
##   3. provides information on the parameter space of lambdas 
## 
## The main goal: figure out the space of lambdas for dataset with 10000, 100000 and 1000000 rows/visits
## 


# Packages
library(glmnet)
library(data.table)
library(doParallel)
library(tictoc)


setwd("C:/Users/Administrator/Documents")

# Reading in the needed data
load(file= "Train_mh.RData")


# Choosing fold
set.seed(12343098)
i <- sample(0:9, 1)
x.mat <- xtrain[splitn == i, ]
y.vec <- ytrain[splitn == i, ]

# Removing data from all folds
rm(xtrain)
rm(ytrain)
rm(splitn)
gc()

# Splitting data into 10 folds
n.visits <- dim(x.mat)[1]
split <- sample(1:10, n.visits, replace = TRUE, prob=rep(.1, 10))

#################################################
# Fitting glmnet model with parallel code
################################################

# A MILLION VISITS

tic("glmfit.amili")
amilli.fit <- glmnet(x.mat, y.vec, family = 'binomial', nfolds = 5,
                     standardize = FALSE, trace.it = 1, nlambda=125)
toc(log=TRUE)

return <- cbind(amilli.fit$lambda, amilli.fit$df)
colnames(return) <- c("Lambda", "Nonzero_Coeffs")
saveRDS(return, file="G:\\Amilli.rds")

rm(return)
gc()

# REDUCING SIZE TO 100000 AND 10000
set.seed(127687)
ind.100k <- sample(1:n.visits, 100000, replace=F)
ind.10k <- sample(1:n.visits, 10000, replace=F)

x.mat100 <- x.mat[ind.100k, ]
y.vec100 <- y.vec[ind.100k]

x.mat10 <- x.mat[ind.10k, ]
y.vec10 <- y.vec[ind.10k]

rm(x.mat)
rm(y.vec)
gc()


# A HUNDRED THOUNSAND VISITS
tic("glmfit.100K")
hundedk.fit <- glmnet(x.mat100, y.vec100, family = 'binomial', nfolds = 5,
                      standardize = FALSE, trace.it = 1, nlambda=125)
toc(log=TRUE)

return <- cbind(hundedk.fit$lambda, hundedk.fit$df)
colnames(return) <- c("Lambda", "Nonzero_Coeffs")
saveRDS(return, file="G:\\Hundredk.rds")

rm(return)
gc()


# TEN THOUSAND VISITS
tic("glmfit.10K")
tenk.fit <- glmnet(x.mat10, y.vec10, family = 'binomial', nfolds = 5,
                   standardize = FALSE, trace.it = 1, nlambda=125)
toc(log=TRUE)

return <- cbind(tenk.fit$lambda, tenk.fit$df)
colnames(return) <- c("Lambda", "Nonzero_Coeffs")
saveRDS(return, file="G:\\Tenk.rds")

rm(return)
gc()


# All Data

rm(list = ls())
gc()

# Loading Data
tic()
setwd("C:/Users/Administrator/Documents")
load("MH_event.RData")
toc()

splitn$CV_PER[which(splitn$CV_PER %in% c(0,1))] <- 0
splitn$CV_PER[which(splitn$CV_PER %in% c(2,3))] <- 1
splitn$CV_PER[which(splitn$CV_PER %in% c(4,5))] <- 2
splitn$CV_PER[which(splitn$CV_PER %in% c(6,7))] <- 3
splitn$CV_PER[which(splitn$CV_PER %in% c(8,9))] <- 4

dim(y_train)
dim(splitn)
summary(splitn)

gc()


#################################################
# Fitting glmnet
################################################

# A MILLION VISITS

tic("fit")
all.fit <- glmnet(x_train, y_train$EVENT_90, family = 'binomial',
                                 standardize = FALSE, trace.it = 1, nlambda=25)
toc(log=TRUE)

return <- cbind(all.fit$lambda, all.fit$df)
colnames(return) <- c("Lambda", "Nonzero_Coeffs")
saveRDS(return, file="G:\\All.rds")

rm(return)
gc()


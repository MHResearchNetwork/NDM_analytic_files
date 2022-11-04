

rm(list=ls())
gc()

# Packages 
library(keras)
library(tictoc)

dir <- "C:/Users/Administrator/Documents/"
b <- 2^10  # 2^12 or 2^10

id <- "Train_mh.0175.hdf5"
fnames <- list.files(dir)
findx  <-  grep(id, fnames)


# New Place holder for Interactions
Design.I <- c()


i <- 1
tic()
load(paste0(dir, "Train_mh_Age_Diag2.RData"))
toc()

model <- load_model_hdf5(file.path(paste0(dir, fnames[findx[i]])))
weights <- get_weights(model)[[1]]

ind <- order(abs(weights[ , 1]), decreasing=TRUE)[1:300]
length(which(ind > 223))

tic()
Design.I <- cbind(Design.I, Interactions[ , ind])
toc()

rm(Interactions)
rm(y_train, splitn)
gc()


i <- 2
tic()
load("Train_mh_Age_Diagnosis.RData")
toc()

model <- load_model_hdf5(file.path(paste0(dir, fnames[findx[i]])))
weights <- get_weights(model)[[1]]

ind <- order(abs(weights[ , 1]), decreasing=TRUE)[1:200]
length(which(ind > 169))

tic()
Design.I <- cbind(Design.I, Interactions[ , ind ])
toc()

rm(Interactions)
rm(y_train, splitn)
gc()


i <- 3
tic()
load("Train_mh_gender.RData")
toc()

model <- load_model_hdf5(file.path(paste0(dir, fnames[findx[i]])))
weights <- get_weights(model)[[1]]

ind <- order(abs(weights[ , 1]), decreasing=TRUE)[1:170]
length(which(ind > 195))

tic()
Design.I <- cbind(Design.I, Interactions[ , ind ])
toc()

rm(Interactions)
rm(y_train, splitn)
gc()


i <- 4
tic()
load("Train_mh_AWS_enr1.RData")
toc()

model <- load_model_hdf5(file.path(paste0(dir, fnames[findx[i]])))
weights <- get_weights(model)[[1]]

ind <- order(abs(weights[ , 1]), decreasing=TRUE)[1:450]

tic()
Design.I <- cbind(Design.I, x_train[ , ind ])
toc()

rm(x_train)
rm(y_train, splitn)
gc()


i <- 5
tic()
load("Train_mh_PHQ9_PriorSA.RData")
toc()

model <- load_model_hdf5(file.path(paste0(dir, fnames[findx[i]])))
weights <- get_weights(model)[[1]]

ind <- order(abs(weights[ , 1]), decreasing=TRUE)[1:185]
length(which(ind > 393))

Interactions <- cbind(Front, Interactions)

tic()
Design.I <- cbind(Design.I, Interactions[ , ind ])
toc()

rm(Interactions, Front)
rm(y_train, splitn)
gc()


i <- 6
tic()
load("Train_mh_Race_Diag2.RData")
toc()

model <- load_model_hdf5(file.path(paste0(dir, fnames[findx[i]])))
weights <- get_weights(model)[[1]]

ind <- order(abs(weights[ , 1]), decreasing=TRUE)[1:170]
length(which(ind > 226))

Interactions <- cbind(Front, Interactions)

tic()
Design.I <- cbind(Design.I, Interactions[ , ind ])
toc()

rm(Interactions, Front)
rm(y_train, splitn)
gc()


i <- 7
tic()
load("Train_mh_Race_Diagnosis.RData")
toc()

model <- load_model_hdf5(file.path(paste0(dir, fnames[findx[i]])))
weights <- get_weights(model)[[1]]

ind <- order(abs(weights[ , 1]), decreasing=TRUE)[1:185]
length(which(ind > 171))

tic()
Design.I <- cbind(Design.I, Interactions[ , ind ])
toc()

rm(Interactions)
rm(y_train, splitn)
gc()


i <- 8
tic()
load("Train_mh_Race_Medication.RData")
toc()

model <- load_model_hdf5(file.path(paste0(dir, fnames[findx[i]])))
weights <- get_weights(model)[[1]]

ind <- order(abs(weights[ , 1]), decreasing=TRUE)[1:155]
length(which(ind > 186))

x_train <- Design.I
rm(Design.I)
gc()

tic()
x_train <- cbind(x_train, Interactions[ , ind ])
toc()

rm(Interactions)
gc()

x_train <- x_train[, !duplicated(colnames(x_train))]


tic()
save(x_train, y_train, splitn, file ="C:/Users/Administrator/Documents/mh_event90.RData")
toc()


# Checking how many predictors in final models:
model <- load_model_hdf5(file.path(paste0(dir,"mh_event_1024_175_9.24e-06.0175.hdf5")))
weights <- get_weights(model)[[1]]


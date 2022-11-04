

# Clearing environment
rm(list=ls())
gc()

# Packages 
library(keras)
library(tictoc)

# Prelimanry values
lam <- 1.46e-05
outdir <- paste0("C:/Users/Administrator/Documents", "/MH_Output_Final/")

# Data 
tic()
load("mh_event90.RData")
toc()

# Changing CV and y_train
splitn[which(splitn %in% c(0,1))] <- 0
splitn[which(splitn %in% c(2,3))] <- 1
splitn[which(splitn %in% c(4,5))] <- 2
splitn[which(splitn %in% c(6,7))] <- 3
splitn[which(splitn %in% c(8,9))] <- 4

y0 <- y_train[which(splitn == 0)]
y1 <- y_train[which(splitn == 1)]
y2 <- y_train[which(splitn == 2)]
y3 <- y_train[which(splitn == 3)]
y4 <- y_train[which(splitn == 4)]

y <- c(y0, y1, y2, y3, y4)


# Getting subset of covariates not equal to zero
id <- paste0("MH_event_1024_175_", lam, ".0")
fnames <- list.files(outdir)
findx  <-  grep(id, fnames)

model <- load_model_hdf5(paste0(outdir, fnames[176]))
coeffs.list <- get_weights(model)
coeffs.noInt <- coeffs.list[[1]]

cut.off <- 1e-08
ind.w <- which(abs(coeffs.noInt) > cut.off)

tic()
x_train <- x_train[ , ind.w]
toc()

gc()

tic()
save(x_train, y_train, splitn, file = paste0(outdir, "\\", "Logit_Preds_MH_event90", ".RData"))
toc()

tic()
var.names <- colnames(x_train)
saveRDS(var.names, file=paste0(getwd(), "/Final_Pred_Names_MH_Event.rds"))
toc()

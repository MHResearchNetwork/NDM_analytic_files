##
## Using Rod's function to obtain in-sample and out of sample 
## perfomance measures.
##


# Clearing environment
rm(list=ls())
gc()

# Packages 
library(keras)
library(tictoc)

# Functions 
source('~/perf.results.id.R')

# Working Directory
setwd("C:/Users/Administrator/Documents")
outdir <- getwd()

# Data 
tic()
load("mh_event90.RData")
toc()

# Preliminary values
lam.vec <- c(9.24e-09, 3.68e-08, 5.83e-08, 
             9.24e-08, 2.55e-07, 
             3.68e-07, 5.83e-07, 9.24e-07,
             3.68e-06, 5.83e-06, 9.24e-06,
             1.46e-05, 2.32e-05, 3.68e-05,
             5.83e-05, 9.24e-05, 1.46e-04,
             2.32e-04, 3.68e-04, 9.24e-04)

# Waiting on the last one!
lam.vec <- lam.vec[ -20]

# Output of Document
Tab.out <- c()
Tab.in <- c()

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

#####################################
## Getting Results
#####################################

for(i in 1:length(lam.vec)){
  
  lam <- lam.vec[i]
  
  # Get predictions
  load(paste0(outdir, "/MH_event__1024_175_", lam, "_predict.Rdata"))
  
  out.sample <- c(predictions[[1]][which(splitn == 0), 1], 
                  predictions[[2]][which(splitn == 1), 1],
                  predictions[[3]][which(splitn == 2), 1],
                  predictions[[4]][which(splitn == 3), 1],
                  predictions[[5]][which(splitn == 4), 1])
  
  in.sample <- list(predictions[[1]][-which(splitn == 0), 1],
                    predictions[[2]][-which(splitn == 1), 1],
                    predictions[[3]][-which(splitn == 2), 1],
                    predictions[[4]][-which(splitn == 3), 1],
                    predictions[[5]][-which(splitn == 4), 1])
  
  rm(predictions)
  gc()
  
  # Out of sample performance 
  out.perf <- perf.results.id(pred = out.sample, outcome = y, train.pred = out.sample)
  
  Tab.out <- rbind(Tab.out, cbind(out.perf$Performance[5, -c(1:2)], out.perf$AUC, out.perf$`Brier Score`))
  
  # In-Sample performance
  in.perf.0 <- perf.results.id(pred = in.sample[[1]], outcome = y_train[-which(splitn == 0)], train.pred = in.sample[[1]])
  in.perf.1 <- perf.results.id(pred = in.sample[[2]], outcome = y_train[-which(splitn == 1)], train.pred = in.sample[[2]])
  in.perf.2 <- perf.results.id(pred = in.sample[[3]], outcome = y_train[-which(splitn == 2)], train.pred = in.sample[[3]])
  in.perf.3 <- perf.results.id(pred = in.sample[[4]], outcome = y_train[-which(splitn == 3)], train.pred = in.sample[[4]])
  in.perf.4 <- perf.results.id(pred = in.sample[[5]], outcome = y_train[-which(splitn == 4)], train.pred = in.sample[[5]])
  
  Perf <- rbind(cbind(in.perf.0$Performance[5, -c(1:2)], in.perf.0$AUC, in.perf.0$`Brier Score`), 
                cbind(in.perf.1$Performance[5, -c(1:2)], in.perf.1$AUC, in.perf.1$`Brier Score`),
                cbind(in.perf.2$Performance[5, -c(1:2)], in.perf.2$AUC, in.perf.2$`Brier Score`), 
                cbind(in.perf.3$Performance[5, -c(1:2)], in.perf.3$AUC, in.perf.3$`Brier Score`),
                cbind(in.perf.4$Performance[5, -c(1:2)], in.perf.4$AUC, in.perf.4$`Brier Score`))
  
  Tab.in <- rbind(Tab.in, colMeans(Perf))
  
  print(paste("Done", i))
}

junk1 <- Tab.in
junk2 <- Tab.out

Tab.in <- cbind(lam.vec, Tab.in)
colnames(Tab.in)[2:12] <- paste0(colnames(Tab.in)[2:12], " 99th Perc.")
Tab.in <- cbind(Tab.in[ , 1], round(Tab.in[ ,2:14], 4))
colnames(Tab.in)[c(1, 13, 14)] <- c("Lambda", "AUC", "Brier Score")
#Tab.in <- rbind(Tab.in[2, ], Tab.in[1, ], Tab.in[3:(dim(Tab.in)[1]), ])

Tab.out <- cbind(lam.vec, Tab.out)
colnames(Tab.out)[2:12] <- paste0(colnames(Tab.out)[2:12], " 99th Perc.")
Tab.out <- cbind(Tab.out[ , 1], round(Tab.out[ ,2:14], 4))
colnames(Tab.out)[c(1, 13, 14)] <- c("Lambda", "AUC", "Brier Score")
#Tab.out <- rbind(Tab.out[2, ], Tab.out[1, ], Tab.out[3:(dim(Tab.out)[1]), ])

Tab.out[ , 13] <-  round(junk2[ , 12], 5)

## Save Tables 
saveRDS(Tab.in, file="C:/Users/Administrator/Documents/In-sample-MH.rds")
saveRDS(Tab.out, file="C:/Users/Administrator/Documents/Out-of-sample-MH.rds")


install.packages("writexl")
library("writexl")

Tab.in  <- as.data.frame(Tab.in)
Tab.out <- as.data.frame(Tab.out)

write_xlsx(Tab.in, "C:/Users/Administrator/Documents/In-sample-MH.xlsx")
write_xlsx(Tab.out, "C:/Users/Administrator/Documents/Out-of-sample-MH.xlsx")



####################################
## How many params are set to 0
####################################

Tab <- matrix(rep(NA, 5*5), ncol=5)
cut.off <- 1e-08
cut.off0 <- 1e-06
cut.off2 <- 1e-07
cut.off3 <- 1e-05
Tab.0 <- Tab
Tab.2 <- Tab
Tab.3 <- Tab

for(i in 10:14){
  
  lam <- lam.vec[i]
  
  for(j in 0:4){
    # Get model
    model <- load_model_hdf5(paste0(outdir, "/MH_event_", j, "_1024_175_", lam, ".0175.hdf5"))
    # Get weights
    weights.mod <- get_weights(model)[[1]]
    n.w <- length(weights.mod)
    # Save
    Tab[i-9 , j+1] <- n.w - sum(abs(weights.mod) > cut.off)
    Tab.0[i-9 , j+1] <- n.w -sum(abs(weights.mod) > cut.off0)
    Tab.2[i-9 , j+1] <- n.w - sum(abs(weights.mod) > cut.off2)
    Tab.3[i-9 , j+1] <- n.w - sum(abs(weights.mod) > cut.off3)
  }
  
  print(paste("Done", i))
}

Tab <- cbind(lam.vec[10:14], Tab)
Tab.2 <- cbind(lam.vec[10:14], Tab.2)
Tab.0 <- cbind(lam.vec[10:14], Tab.0)
Tab.3 <- cbind(lam.vec[10:14], Tab.3)

Tab.o <- rbind(Tab, Tab.2, Tab.0, Tab.3)
Tab.o <- cbind(rep(c(cut.off, cut.off2, cut.off0, cut.off3), each=5), Tab.o)
colnames(Tab.o) <- c("Threshold", "Lambda", paste('Fold', 0:4))

Tab.o <- as.data.frame(Tab.o)
Tab.Kept <- Tab.o
Tab.Kept[ , 3:7] <- length(weights.mod) - Tab.Kept[ , 3:7]

write_xlsx(Tab.o, "C:/Users/Administrator/Documents/Params-Set-to-0-MH.xlsx")
write_xlsx(Tab.Kept, "C:/Users/Administrator/Documents/Params-Kept-MH.xlsx")


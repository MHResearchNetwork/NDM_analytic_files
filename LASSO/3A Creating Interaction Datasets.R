
##
## This file:
##   Creates datasets with the interactions of interest.
##


## Libraries
library(data.table)
library(tictoc)


## Data
setwd("C:/Users/Administrator/Documents")
load(file="Train_mh.RData")

x_train[which(is.na(x_train) == T)] <- 0

# save .RData file
tic()
save(x_train, y_train, splitn, file = paste0(outdir, "\\", "Train_mh", ".RData"))
toc()

## Updated data storage output directory
outdir <-"C:/Users/Administrator/Documents"


########################################################
# Race and Ethnicity w/ Basic Diagnosis Information 
########################################################

## Getting indeces of variables we want to interact
var.name <- c(paste0("RACE_", c("AS", "BA", "HP", "IN", "MU", "OT", "UN")), 
              "HISPANIC_U", "HISPANIC_Y")
indx <- which(colnames(x_train) %in% var.name)

var.name2 <- c(paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                      "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                      "CON", "DIA", "AST", "PAI"), "_TOT_DAYS_LAST01M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_TOT_DAYS_LAST03M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_TOT_DAYS_LAST12M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_TOT_DAYS_LAST24M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_TOT_DAYS_LAST60M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_DAYS_PER_LAST03M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_DAYS_PER_LAST60M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_LAST_MTH_WITH"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_1ST_MTH_WITH"))
int <- which(colnames(x_train) %in% var.name2)

## Creating design matrix
Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_train[ , int]*x_train[ , indx[i]])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

tic()
colnames(Interactions) <- new.col.names
Interactions <- cbind(x_train[ , c(int, indx)], Interactions)
toc()


tic()
save(Interactions, y_train, splitn, file = paste0(outdir, "\\", "Train_mh_Race_Diagnosis", ".RData"))
toc()

rm(Interactions)
rm(new.col.names)
rm(var.name, indx, var.name2, int)
gc()


########################################################
# Race and Ethnicity w/ Medication Information 
########################################################

## Getting indeces of variables we want to interact
var.name <- c(paste0("RACE_", c("AS", "BA", "HP", "IN", "MU", "OT", "UN")), 
              "HISPANIC_U", "HISPANIC_Y")
indx <- which(colnames(x_train) %in% var.name)

var.name2 <- c(paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST01M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST03M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST12M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST24M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NEVER_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_MTHS_PER_LAST03M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_MTHS_PER_LAST12M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_MTHS_PER_LAST24M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_MTHS_PER_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST01M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST03M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST12M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST24M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_DAYS_PER_LAST03M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_DAYS_PER_LAST12M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_DAYS_PER_LAST24M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_DAYS_PER_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_LAST_MTH_WITH"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_1ST_MTH_WITH"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_ON_HAND")
               )
int <- which(colnames(x_train) %in% var.name2)

## Creating design matrix
Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_train[ , int]*x_train[ , indx[i]])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

tic()
colnames(Interactions) <- new.col.names
Interactions <- cbind(x_train[ , c(int, indx)], Interactions)
toc()

tic()
save(Interactions, y_train, splitn, file = paste0(outdir, "\\", "Train_mh_Race_Medication", ".RData"))
toc()

print(dim(Interactions))

rm(Interactions)
rm(new.col.names)
rm(var.name, indx, var.name2, int)
gc()


########################################################
# Race and Ethnicity w/ Prior Ideation Attempt and Diagnosis
########################################################

## Getting indeces of variables we want to interact
var.name <- c(paste0("RACE_", c("AS", "BA", "HP", "IN", "MU", "OT", "UN")), 
              "HISPANIC_U", "HISPANIC_Y")
indx <- which(colnames(x_train) %in% var.name)

var.name2 <- c(paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST01M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST03M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST12M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST24M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST60M"),
               
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_NEVER_LAST60M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST03M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST12M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST24M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST60M"),
               
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_LAST_MTH_WITH"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_LAST_MTH_WOUT"),
               
               c("PHQ8_MISSING", "ITEM9_MISSING", "PHQ8_VISIT", "ITEM9_VISIT"),
               
               c("ITEM9_MISS_PRIOR", "ITEM9_MAX_PRIOR", "ITEM9_MTHS_SINCE_MAX",
                 "ITEM9_NUM_PRIOR", "ITEM9_MTHS_SINCE_LAST",
                 
                 "ITEM9_SCORE0_NUM_PRIOR", "ITEM9_SCORE1_NUM_PRIOR", 
                 "ITEM9_SCORE2_NUM_PRIOR", "ITEM9_SCORE3_NUM_PRIOR",
                 
                 "ITEM9_SCORE0_NUM_LAST01M", "ITEM9_SCORE1_NUM_LAST01M",
                 "ITEM9_SCORE2_NUM_LAST01M", "ITEM9_SCORE3_NUM_LAST01M",
                 
                 "ITEM9_SCORE0_NUM_LAST03M", "ITEM9_SCORE1_NUM_LAST03M",
                 "ITEM9_SCORE2_NUM_LAST03M", "ITEM9_SCORE3_NUM_LAST03M",
                 
                 "ITEM9_SCORE0_NUM_LAST12M", "ITEM9_SCORE1_NUM_LAST12M",
                 "ITEM9_SCORE2_NUM_LAST12M", "ITEM9_SCORE3_NUM_LAST12M",
                 
                 "ITEM9_SCORE0_NUM_LAST24M", "ITEM9_SCORE1_NUM_LAST24M",
                 "ITEM9_SCORE2_NUM_LAST24M", "ITEM9_SCORE3_NUM_LAST24M",
                 
                 "ITEM9_SCORE0_NUM_LAST60M", "ITEM9_SCORE1_NUM_LAST60M",
                 "ITEM9_SCORE2_NUM_LAST60M", "ITEM9_SCORE3_NUM_LAST60M",
                 
                 "ITEM9_SCORE0_PCT", "ITEM9_SCORE1_PCT", "ITEM9_SCORE2_PCT", "ITEM9_SCORE3_PCT",

                 "ITEM9_SCORE0_MTHS_SINCE", "ITEM9_SCORE1_MTHS_SINCE", 
                 "ITEM9_SCORE2_MTHS_SINCE", "ITEM9_SCORE3_MTHS_SINCE"),
               
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NEVER_LAST60M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST03M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST06M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST12M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST24M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST60M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_LAST_MTH_WITH_MAX"))

int <- which(colnames(x_train) %in% var.name2)

## Creating design matrix
Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_train[ , int]*x_train[ , indx[i]])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

tic()
colnames(Interactions) <- new.col.names
Interactions <- cbind(x_train[ , c(int, indx)], Interactions)
toc()

tic()
save(Interactions, y_train, splitn, file = paste0(outdir, "\\", "Train_mh_Race_Diag2", ".RData"))
toc()

print(dim(Interactions))

rm(Interactions)
rm(new.col.names)
rm(var.name, indx, var.name2, int)
gc()


########################################################
# Age w/ Basic Diagnosis Information 
########################################################

## Getting indeces of Age
var.name <- c("AGE")
indx <- which(colnames(x_train) %in% var.name)

var.name <- c("AGE_11_17", "AGE_18_25", "AGE_26_35", "AGE_36_45",
              "AGE_46_55", "AGE_56_65", "AGE_65_Plus") 
Age.Mat <- cbind(ifelse(x_train[ ,indx] <= 17, 1, 0), 
                 ifelse(x_train[ ,indx] > 17 & x_train[ ,indx] <= 25, 1, 0),
                 ifelse(x_train[ ,indx] > 26 & x_train[ ,indx] <= 35, 1, 0),
                 ifelse(x_train[ ,indx] > 36 & x_train[ ,indx] <= 45, 1, 0),
                 ifelse(x_train[ ,indx] > 46 & x_train[ ,indx] <= 55, 1, 0),
                 ifelse(x_train[ ,indx] > 56 & x_train[ ,indx] <= 65, 1, 0),
                 ifelse(x_train[ ,indx] > 65, 1, 0))

var.name2 <- c(paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_TOT_DAYS_LAST01M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_TOT_DAYS_LAST03M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_TOT_DAYS_LAST12M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_TOT_DAYS_LAST24M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_TOT_DAYS_LAST60M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_DAYS_PER_LAST03M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_DAYS_PER_LAST60M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_LAST_MTH_WITH"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_1ST_MTH_WITH"))
int <- which(colnames(x_train) %in% var.name2)


## Creating design matrix
Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_train[ , int]*Age.Mat[ , i])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

tic()
colnames(Age.Mat) <- var.name
colnames(Interactions) <- new.col.names
Interactions <- cbind(Age.Mat, x_train[ , c(int)], Interactions)
toc()

tic()
save(Interactions, y_train, splitn, file = paste0(outdir, "\\", "Train_mh_Age_Diagnosis", ".RData"))
toc()

print(dim(Interactions))

rm(Interactions)
rm(new.col.names)
rm(var.name2, int)
gc()


########################################################
# Age w/ Prior Ideation Attempt and Diagnosis
########################################################

## Getting indices of variables to interact with
var.name2 <- c(paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST01M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST03M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST12M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST24M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST60M"),
               
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_NEVER_LAST60M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST03M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST12M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST24M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST60M"),
               
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_LAST_MTH_WITH"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_LAST_MTH_WOUT"),
               
               c("PHQ8_MISSING", "ITEM9_MISSING", "PHQ8_VISIT", "ITEM9_VISIT"),
               
               c("ITEM9_MISS_PRIOR", "ITEM9_MAX_PRIOR", "ITEM9_MTHS_SINCE_MAX",
                 "ITEM9_NUM_PRIOR", "ITEM9_MTHS_SINCE_LAST",
                 
                 "ITEM9_SCORE0_NUM_PRIOR", "ITEM9_SCORE1_NUM_PRIOR", 
                 "ITEM9_SCORE2_NUM_PRIOR", "ITEM9_SCORE3_NUM_PRIOR",
                 
                 "ITEM9_SCORE0_NUM_LAST01M", "ITEM9_SCORE1_NUM_LAST01M", 
                 "ITEM9_SCORE2_NUM_LAST01M", "ITEM9_SCORE3_NUM_LAST01M",
                 
                 "ITEM9_SCORE0_NUM_LAST03M", "ITEM9_SCORE1_NUM_LAST03M", 
                 "ITEM9_SCORE2_NUM_LAST03M", "ITEM9_SCORE3_NUM_LAST03M",
                 
                 "ITEM9_SCORE0_NUM_LAST12M", "ITEM9_SCORE1_NUM_LAST12M", 
                 "ITEM9_SCORE2_NUM_LAST12M", "ITEM9_SCORE3_NUM_LAST12M",
                 
                 "ITEM9_SCORE0_NUM_LAST24M", "ITEM9_SCORE1_NUM_LAST24M", 
                 "ITEM9_SCORE2_NUM_LAST24M", "ITEM9_SCORE3_NUM_LAST24M",
                 
                 "ITEM9_SCORE0_NUM_LAST60M", "ITEM9_SCORE1_NUM_LAST60M", 
                 "ITEM9_SCORE2_NUM_LAST60M", "ITEM9_SCORE3_NUM_LAST60M",
                 
                 "ITEM9_SCORE0_PCT", "ITEM9_SCORE1_PCT", 
                 "ITEM9_SCORE2_PCT", "ITEM9_SCORE3_PCT",
                 
                 "ITEM9_SCORE0_MTHS_SINCE", "ITEM9_SCORE1_MTHS_SINCE", 
                 "ITEM9_SCORE2_MTHS_SINCE", "ITEM9_SCORE3_MTHS_SINCE"),
               
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NEVER_LAST60M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST03M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST06M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST12M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST24M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST60M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_LAST_MTH_WITH_MAX"))

int <- which(colnames(x_train) %in% var.name2)

## Creating design matrix
Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions,  x_train[ , int]*Age.Mat[ , i])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

tic()
colnames(Interactions) <- new.col.names
Interactions <- cbind(Age.Mat, x_train[ , c(int)], Interactions)
toc()

tic()
save(Interactions, y_train, splitn, file = paste0(outdir, "\\", "Train_mh_Age_Diag2", ".RData"))
toc()

print(dim(Interactions))

rm(Interactions)
rm(new.col.names)
rm(var.name, indx, var.name2, int)
rm(Age.Mat)
gc()


########################################################
# Gender and Everything 
########################################################

## Getting indeces of variables we want to interact
var.name <- c(paste0("SEX_", c("M", "O", "U")))
indx <- which(colnames(x_train) %in% var.name)

var.name2 <- c(paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST01M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST03M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST12M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST24M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NEVER_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_MTHS_PER_LAST03M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_MTHS_PER_LAST12M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_MTHS_PER_LAST24M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_MTHS_PER_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST01M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST03M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST12M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST24M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_DAYS_PER_LAST03M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_DAYS_PER_LAST12M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_DAYS_PER_LAST24M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_DAYS_PER_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_LAST_MTH_WITH"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_1ST_MTH_WITH"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_ON_HAND" ),
                     
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST01M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST03M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST12M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST24M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST60M"),
               
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_NEVER_LAST60M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST03M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST12M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST24M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST60M"),
               
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_LAST_MTH_WITH"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_LAST_MTH_WOUT"),
               
               c("PHQ8_MISSING", "ITEM9_MISSING", "PHQ8_VISIT", "ITEM9_VISIT"),
               
               c("ITEM9_MISS_PRIOR", "ITEM9_MAX_PRIOR", "ITEM9_MTHS_SINCE_MAX",
                 "ITEM9_NUM_PRIOR", "ITEM9_MTHS_SINCE_LAST",
                 
                 "ITEM9_SCORE0_NUM_PRIOR", "ITEM9_SCORE1_NUM_PRIOR", 
                 "ITEM9_SCORE2_NUM_PRIOR", "ITEM9_SCORE3_NUM_PRIOR",
                 
                 "ITEM9_SCORE0_NUM_LAST01M", "ITEM9_SCORE1_NUM_LAST01M",
                 "ITEM9_SCORE2_NUM_LAST01M", "ITEM9_SCORE3_NUM_LAST01M",
                 
                 "ITEM9_SCORE0_NUM_LAST03M", "ITEM9_SCORE1_NUM_LAST03M",
                 "ITEM9_SCORE2_NUM_LAST03M", "ITEM9_SCORE3_NUM_LAST03M",
                 
                 "ITEM9_SCORE0_NUM_LAST12M", "ITEM9_SCORE1_NUM_LAST12M",
                 "ITEM9_SCORE2_NUM_LAST12M", "ITEM9_SCORE3_NUM_LAST12M",
                 
                 "ITEM9_SCORE0_NUM_LAST24M", "ITEM9_SCORE1_NUM_LAST24M",
                 "ITEM9_SCORE2_NUM_LAST24M", "ITEM9_SCORE3_NUM_LAST24M",
                 
                 "ITEM9_SCORE0_NUM_LAST60M", "ITEM9_SCORE1_NUM_LAST60M",
                 "ITEM9_SCORE2_NUM_LAST60M", "ITEM9_SCORE3_NUM_LAST60M",
                 
                 "ITEM9_SCORE0_PCT", "ITEM9_SCORE1_PCT", 
                 "ITEM9_SCORE2_PCT", "ITEM9_SCORE3_PCT",
                 
                 "ITEM9_SCORE0_MTHS_SINCE", "ITEM9_SCORE1_MTHS_SINCE", 
                 "ITEM9_SCORE2_MTHS_SINCE", "ITEM9_SCORE3_MTHS_SINCE"),
               
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NEVER_LAST60M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST03M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST06M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST12M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST24M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST60M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_LAST_MTH_WITH_MAX")
)

int <- which(colnames(x_train) %in% var.name2)


## Creating design matrix
Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_train[ , int]*x_train[ , indx[i]])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

tic()
colnames(Interactions) <- new.col.names
Interactions <- cbind(x_train[ , c(int, indx)], Interactions)
toc()

tic()
save(Interactions, y_train, splitn, file = paste0(outdir, "\\", "Train_mh_gender", ".RData"))
toc()

print(dim(Interactions))

rm(Interactions)
rm(new.col.names)
rm(var.name, indx, var.name2, int)
gc()


########################################################
# PHQ9 and Prior SUcide Attempt and Everything 
########################################################

indx <- which(colnames(x_train) %in% c("ITEM9_VISIT"))
indx2 <- which(colnames(x_train) %in% c("ITEM9_MISSING"))

PHQ9.Mat <- cbind( ifelse(x_train[ ,indx] == 0, 1, 0),
                   ifelse(x_train[ ,indx] == 1/3, 1, 0), 
                   ifelse(x_train[ ,indx] == 2/3, 1, 0),
                   ifelse(x_train[ ,indx] == 1, 1, 0),
                   x_train[ , indx2])

# Getting indeces of variables we want to interact
var.name <- c(paste0("ITEM9_VISIT_", c("0", "1over3", "2over3", '1')), c("ITEM9_MISSING"))

var.name2 <- c(paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST01M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST03M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST12M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST24M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NUM_MTHS_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_NEVER_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_MTHS_PER_LAST03M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_MTHS_PER_LAST12M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_MTHS_PER_LAST24M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_MTHS_PER_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST01M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST03M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST12M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST24M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_TOT_DAYS_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_DAYS_PER_LAST03M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_DAYS_PER_LAST12M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_DAYS_PER_LAST24M"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_DAYS_PER_LAST60M"),
               
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_LAST_MTH_WITH"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_1ST_MTH_WITH"),
               paste0(c("ACV", "ADR", "ADP", "BEN", "FGA", "HYP", "LIT", "SGA"), "_ON_HAND" ),
               
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST01M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST03M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST12M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST24M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_TOT_DAYS_LAST60M"),
               
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_NEVER_LAST60M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST03M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST12M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST24M"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_DAYS_PER_LAST60M"),
               
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_LAST_MTH_WITH"),
               paste0(c("ASA", "LSA", "OSA", "AIP"), "_LAST_MTH_WOUT"),
               
               c("ITEM9_MISS_PRIOR", "ITEM9_MAX_PRIOR", "ITEM9_MTHS_SINCE_MAX",
                 "ITEM9_NUM_PRIOR", "ITEM9_MTHS_SINCE_LAST",
                 
                 "ITEM9_SCORE0_NUM_PRIOR", "ITEM9_SCORE1_NUM_PRIOR", 
                 "ITEM9_SCORE2_NUM_PRIOR", "ITEM9_SCORE3_NUM_PRIOR",
                 
                 "ITEM9_SCORE0_NUM_LAST01M", "ITEM9_SCORE1_NUM_LAST01M",
                 "ITEM9_SCORE2_NUM_LAST01M", "ITEM9_SCORE3_NUM_LAST01M",
                 
                 "ITEM9_SCORE0_NUM_LAST03M", "ITEM9_SCORE1_NUM_LAST03M",
                 "ITEM9_SCORE2_NUM_LAST03M", "ITEM9_SCORE3_NUM_LAST03M",
                 
                 "ITEM9_SCORE0_NUM_LAST12M", "ITEM9_SCORE1_NUM_LAST12M",
                 "ITEM9_SCORE2_NUM_LAST12M", "ITEM9_SCORE3_NUM_LAST12M",
                 
                 "ITEM9_SCORE0_NUM_LAST24M", "ITEM9_SCORE1_NUM_LAST24M",
                 "ITEM9_SCORE2_NUM_LAST24M", "ITEM9_SCORE3_NUM_LAST24M",
                 
                 "ITEM9_SCORE0_NUM_LAST60M", "ITEM9_SCORE1_NUM_LAST60M",
                 "ITEM9_SCORE2_NUM_LAST60M", "ITEM9_SCORE3_NUM_LAST60M",
                 
                 "ITEM9_SCORE0_PCT", "ITEM9_SCORE1_PCT", 
                 "ITEM9_SCORE2_PCT", "ITEM9_SCORE3_PCT",
                 
                 "ITEM9_SCORE0_MTHS_SINCE", "ITEM9_SCORE1_MTHS_SINCE", 
                 "ITEM9_SCORE2_MTHS_SINCE", "ITEM9_SCORE3_MTHS_SINCE"),
               
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NEVER_LAST60M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST03M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST06M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST12M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST24M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_NUM_MTHS_LAST60M"),
               paste0(c("DEP", "ANX", "BIP", "SCH", "OPD", "DEM", "ADD", 
                        "ASD", "PER", "ALC", "DRU", "PTS", "EAT", "TBI",
                        "CON", "DIA", "AST", "PAI"), "_LAST_MTH_WITH_MAX")
)

int <- which(colnames(x_train) %in% var.name2)

## Creating design matrix
Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_train[ , int]*PHQ9.Mat[ , i])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

colnames(PHQ9.Mat) <- var.name

PHQ9.Mat <- cbind(PHQ9.Mat, x_train[ , int])

var.name <- c("ASA_TOT_DAYS_LAST60M")
indx <- which(colnames(x_train) %in% var.name)

PHQ9.Mat2 <- matrix(ifelse(x_train[ , indx] > 0, 1, 0), ncol=1)

var.name2 <- c("ITEM9_MISS_PRIOR", "ITEM9_MAX_PRIOR", "ITEM9_MTHS_SINCE_MAX",
               "ITEM9_NUM_PRIOR", "ITEM9_MTHS_SINCE_LAST",
               
               "ITEM9_SCORE0_NUM_PRIOR", "ITEM9_SCORE1_NUM_PRIOR", 
               "ITEM9_SCORE2_NUM_PRIOR", "ITEM9_SCORE3_NUM_PRIOR",
               
               "ITEM9_SCORE0_NUM_LAST01M", "ITEM9_SCORE1_NUM_LAST01M",
               "ITEM9_SCORE2_NUM_LAST01M", "ITEM9_SCORE3_NUM_LAST01M",
               
               "ITEM9_SCORE0_NUM_LAST03M", "ITEM9_SCORE1_NUM_LAST03M",
               "ITEM9_SCORE2_NUM_LAST03M", "ITEM9_SCORE3_NUM_LAST03M",
               
               "ITEM9_SCORE0_NUM_LAST12M", "ITEM9_SCORE1_NUM_LAST12M",
               "ITEM9_SCORE2_NUM_LAST12M", "ITEM9_SCORE3_NUM_LAST12M",
               
               "ITEM9_SCORE0_NUM_LAST24M", "ITEM9_SCORE1_NUM_LAST24M",
               "ITEM9_SCORE2_NUM_LAST24M", "ITEM9_SCORE3_NUM_LAST24M",
               
               "ITEM9_SCORE0_NUM_LAST60M", "ITEM9_SCORE1_NUM_LAST60M",
               "ITEM9_SCORE2_NUM_LAST60M", "ITEM9_SCORE3_NUM_LAST60M",
               
               "ITEM9_SCORE0_PCT", "ITEM9_SCORE1_PCT", "ITEM9_SCORE2_PCT", "ITEM9_SCORE3_PCT",
               
               "ITEM9_SCORE0_MTHS_SINCE", "ITEM9_SCORE1_MTHS_SINCE", 
               "ITEM9_SCORE2_MTHS_SINCE", "ITEM9_SCORE3_MTHS_SINCE")
int <- which(colnames(x_train) %in% var.name2)

tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_train[ , int]*PHQ9.Mat2[ , i])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

colnames(Interactions) <- new.col.names

tic()
Interactions <- cbind(PHQ9.Mat2, PHQ9.Mat, Interactions)
toc()


tic()
save(Interactions, y_train, splitn, file = paste0(outdir, "\\", "Train_mh_PHQ9_PriorSA", ".RData"))
toc()

print(dim(Interactions))

rm(Interactions)
rm(new.col.names)
rm(var.name, indx, var.name2, int)
rm(PHQ9.Mat, PHQ9.Mat2)
gc()

rm(list = ls())
gc()


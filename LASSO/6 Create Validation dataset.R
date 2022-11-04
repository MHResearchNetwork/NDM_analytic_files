
rm(list = ls())
gc()

## Libraries
library(data.table)
library(tictoc)
library(fastDummies)
library(dplyr)
library(bit64)

################################
## Test_mh_per_AWS_enr1_PARA_AWS_enr1
################################

## Reading in data
tic()
load(file="G:\\Test_mh.Rdata")
toc()
dim(Test_mh_per_AWS_enr1)

# Read in final data used for logistic regression model (about); i.e., design matrix for final set of predictors 
load(file = "G:\\Logit_Preds_MH_event90.RData")
dim(x_train)
outdir <- "G:\\data"


## Preliminary params/storage objects
fname <- "Test_mh_per_AWS_enr1"

## Working through data

# Remove missing events
tic()
assign(fname, subset(get(fname), !is.na(EVENT90)))
toc()
dim(get(fname))

# create outcome 
tic()
assign("y_test", get(fname)[,c("EVENT90")])
toc()

y_test <- y_test$EVENT90


# create merge stuff
tic()
match <- get(fname)[, c("PERSON_ID","VISIT_SEQ", "FIRST_VISIT",
                        "EVENT30", "EVENT90", "EVENT180")]
save(match,  file = paste0(outdir, "\\", "For_Merging_With_All_Outcomes_Event_test_MH_Event90.Rdata"))
rm(match)
gc()
toc()



# Remove unused variables with data.table
tic()
indx <- which(names(get(fname))%in%c("PERSON_ID","VISIT_SEQ",
                                     "EVENT30", "EVENT90", "EVENT180",
                                     "DEATH30", "DEATH90", "DEATH180",
                                     "DEF_DEATH30", "DEF_DEATH90", "DEF_DEATH180"))
get(fname)[, (indx) := NULL]
toc()

# Printing variables with NAs
tic()
na.colname <- colSums(is.na(get(fname)))
na.colname <- which(na.colname != 0)
print(na.colname)
na.colname <- as.numeric(na.colname)
toc()

# replace NA's with zero's
tic()
Test_mh_per_AWS_enr1$ITEM9_MTHS_SINCE_LAST[which(is.na(get(fname)$ITEM9_MTHS_SINCE_LAST) == TRUE)] <- 0
toc()

# Identify character variables and separate from other covariates
tic()
charac_indx <- grep("character", sapply(get(fname), class))
characters <- get(fname)[, charac_indx, with=F]
get(fname)[, (charac_indx) := NULL]
toc()

# Getting class type and number of levels for each remaining variable
tic()
vclasses <- sapply(colnames(get(fname)), function(X) class(get(fname)[[X]]))
vnumlevels <- sapply(colnames(get(fname)), function(X) length(unique(get(fname)[[X]])))
toc()

# Getting matrix of variables that are integers with 3 levels 
indx <- vclasses=="integer" & vnumlevels==3

integers_categorical <- sapply(colnames(get(fname)[, ..indx]), function(X) {
  ifelse(all.equal(c(-1,0,1), sort(unique(get(fname)[, ..indx][[X]])))==TRUE, TRUE, FALSE) })

integers_categorical <- which(names(get(fname)) %in%  names(integers_categorical[integers_categorical==TRUE]))
int_cats <- get(fname)[, integers_categorical, with=F]
get(fname)[, (integers_categorical) := NULL]

# Make indicators for factor variables
numcharacters <- dim(characters)[[2]]
numintcats <- dim(int_cats)[[2]]
characters <- dummy_cols(characters, remove_most_frequent_dummy = TRUE)[, -c(1:numcharacters), with = F]
int_cats <- dummy_cols(int_cats, remove_most_frequent_dummy = TRUE, 
                       select_columns = names(int_cats))[, -c(1:numintcats), with = F]

# Determining variable ranges 
tic()
mins <- sapply(1:ncol(get(fname)), function(c) min(get(fname)[, c, with=F], na.rm=T))
maxs <- sapply(1:ncol(get(fname)), function(c) max(get(fname)[, c, with=F], na.rm=T))
toc()

gc()

range <- maxs - mins

# Several variables have no variability, remove them
var_has_one_val <- which(range == 0)
get(fname)[, (var_has_one_val) := NULL]

mins <- mins[-var_has_one_val]
maxs <- maxs[-var_has_one_val]
range <- range[-var_has_one_val]

# Create xtest
tic()
x_test <- get(fname)
toc()

# Normalize continuous variables
tic()
for (j in 1:ncol(x_test)) set(x_test, j = j, value = (x_test[[j]] - mins[j])/(maxs[j]-mins[j]))
toc()

# Put everything back together
tic()
x_test <- bind_cols(x_test, characters)
x_test <- bind_cols(x_test, int_cats)
toc()

# Remove things
rm(characters)
rm(int_cats)
rm(Test_mh_per_AWS_enr1)
rm(mins)
rm(maxs)
gc()

# transform xtrain to a matrix
tic()
x_test <- as.matrix(x_test)
merge_mh_test <- as.matrix(merge_mh_test)
toc()
gc()

sum(is.na(x_test))

dim(x_train)
dim(x_test)

Final <- c()
ind.full <- which(colnames(x_test) %in%colnames(x_train))
Final <- cbind(Final, x_test[ , ind.full])

rm(splitn, y_train, range, na.colname, numintcats, integers_categorical, ind.full, 
   fname, numcharacters, var_has_one_val, vnumlevels)
rm(indx, j, vclasses, charac_indx)
gc()


# "ASA_TOT_DAYS_LAST60M_Indicator"
########################################################
# Race and Ethnicity w/ Basic Diagnosis Information 
########################################################

## Getting indeces of variables we want to interact
var.name <- c(paste0("RACE_", c("AS", "BA", "HP", "IN", "MU", "OT", "UN")), 
              "HISPANIC_U", "HISPANIC_Y")
indx <- which(colnames(x_test) %in% var.name)

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
int <- which(colnames(x_test) %in% var.name2)

Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_test[ , int]*x_test[ , indx[i]])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

tic()
colnames(Interactions) <- new.col.names
toc()

int.F <- which(colnames(Interactions) %in% colnames(x_train))
Final <- cbind(Final, Interactions[ , int.F])

rm(Interactions)
gc()

########################################################
# Race and Ethnicity w/ Medication Information 
########################################################

## Getting indeces of variables we want to interact
var.name <- c(paste0("RACE_", c("AS", "BA", "HP", "IN", "MU", "OT", "UN")), 
              "HISPANIC_U", "HISPANIC_Y")
indx <- which(colnames(x_test) %in% var.name)

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

int <- which(colnames(x_test) %in% var.name2)

Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_test[ , int]*x_test[ , indx[i]])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

tic()
colnames(Interactions) <- new.col.names
toc()

int.F <- which(colnames(Interactions) %in% colnames(x_train))
dim(Final)
Final <- cbind(Final, Interactions[ , int.F])
dim(Final)

rm(Interactions)
gc()


########################################################
# Race and Ethnicity w/ Prior Ideation Attempt and Diagnosis
########################################################

## Getting indeces of variables we want to interact
var.name <- c(paste0("RACE_", c("AS", "BA", "HP", "IN", "MU", "OT", "UN")), 
              "HISPANIC_U", "HISPANIC_Y")
indx <- which(colnames(x_test) %in% var.name)

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

int <- which(colnames(x_test) %in% var.name2)

Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_test[ , int]*x_test[ , indx[i]])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

tic()
colnames(Interactions) <- new.col.names
toc()

int.F <- which(colnames(Interactions) %in% colnames(x_train))
dim(Final)

Final <- cbind(Final, Interactions[ , int.F])
dim(Final)
sum(is.na(Final))

rm(Interactions)
gc()


########################################################
# Age w/ Basic Diagnosis Information 
########################################################

## Getting indeces of Age
var.name <- c("AGE")
indx <- which(colnames(x_test) %in% var.name)

var.name <- c("AGE_11_17", "AGE_18_25", "AGE_26_35", "AGE_36_45",
              "AGE_46_55", "AGE_56_65", "AGE_65_Plus") 
Age.Mat <- cbind(ifelse(x_test[ ,indx] <= 17, 1, 0), 
                 ifelse(x_test[ ,indx] > 17 & x_test[ ,indx] <= 25, 1, 0),
                 ifelse(x_test[ ,indx] > 26 & x_test[ ,indx] <= 35, 1, 0),
                 ifelse(x_test[ ,indx] > 36 & x_test[ ,indx] <= 45, 1, 0),
                 ifelse(x_test[ ,indx] > 46 & x_test[ ,indx] <= 55, 1, 0),
                 ifelse(x_test[ ,indx] > 56 & x_test[ ,indx] <= 65, 1, 0),
                 ifelse(x_test[ ,indx] > 65, 1, 0))

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
int <- which(colnames(x_test) %in% var.name2)


## Creating design matrix
Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_test[ , int]*Age.Mat[ , i])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

tic()
colnames(Age.Mat) <- var.name
colnames(Interactions) <- new.col.names
Interactions <- cbind(Age.Mat, x_test[ , c(int)], Interactions)
toc()

int.F <- which(colnames(Interactions) %in% colnames(x_train))
dim(Final)

Final <- cbind(Final, Interactions[ , int.F])
dim(Final)

rm(Interactions)
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

int <- which(colnames(x_test) %in% var.name2)

## Creating design matrix
Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions,  x_test[ , int]*Age.Mat[ , i])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

tic()
colnames(Interactions) <- new.col.names
Interactions <- cbind(Age.Mat, x_test[ , c(int)], Interactions)
toc()

int.F <- which(colnames(Interactions) %in% colnames(x_train))
int.F
dim(Final)

Final <- cbind(Final, Interactions[ , int.F])
dim(Final)

rm(Interactions)
gc()

rm(Age.Mat)
gc()


########################################################
# Gender and Everything 
########################################################

## Getting indeces of variables we want to interact
var.name <- c(paste0("SEX_", c("M", "O", "U")))
indx <- which(colnames(x_test) %in% var.name)

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

int <- which(colnames(x_test) %in% var.name2)


## Creating design matrix
Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_test[ , int]*x_test[ , indx[i]])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

tic()
colnames(Interactions) <- new.col.names
toc()

int.F <- which(colnames(Interactions) %in% colnames(x_train))
int.F
dim(Final)

Final <- cbind(Final, Interactions[ , int.F])
dim(Final)

rm(Interactions)
gc()


########################################################
# PHQ9 and Prior SUcide Attempt and Everything 
########################################################

indx <- which(colnames(x_test) %in% c("ITEM9_VISIT"))
indx2 <- which(colnames(x_test) %in% c("ITEM9_MISSING"))

PHQ9.Mat <- cbind( ifelse(x_test[ ,indx] == 0, 1, 0),
                   ifelse(x_test[ ,indx] == 1/3, 1, 0), 
                   ifelse(x_test[ ,indx] == 2/3, 1, 0),
                   ifelse(x_test[ ,indx] == 1, 1, 0),
                   x_test[ , indx2])

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

int <- which(colnames(x_test) %in% var.name2)

## Creating design matrix
Interactions <- c()
new.col.names <- c()
tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_test[ , int]*PHQ9.Mat[ , i])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

colnames(PHQ9.Mat) <- var.name

PHQ9.Mat <- cbind(PHQ9.Mat, x_test[ , int])

var.name <- c("ASA_TOT_DAYS_LAST60M")
indx <- which(colnames(x_test) %in% var.name)

PHQ9.Mat2 <- matrix(ifelse(x_test[ , indx] > 0, 1, 0), ncol=1)
colnames(PHQ9.Mat2) <- "ASA_TOT_DAYS_LAST60M_Indicator"

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
int <- which(colnames(x_test) %in% var.name2)

tic()
for(i in 1:length(var.name)){
  Interactions <- cbind(Interactions, x_test[ , int]*PHQ9.Mat2[ , i])
  new.col.names <- c(new.col.names, paste0(var.name2, "_",var.name[i]))
  print(i)
}
toc()

colnames(Interactions) <- new.col.names

tic()
Interactions <- cbind(PHQ9.Mat2, PHQ9.Mat, Interactions)
toc()


int.F <- which(colnames(Interactions) %in% colnames(x_train))
int.F
dim(Final)

Final <- cbind(Final, Interactions[ , int.F])
dim(Final)

rm(Interactions,PHQ9.Mat2, PHQ9.Mat)
gc()


### Making Final design matrix

length(unique(colnames(Final)))
dim(x_train)

sum(is.na(x_test))
sum(is.na(Final))

# junk <- colSums((Final))
# which(is.na(junk))
# apply(Final[ , which(is.na(junk))], 2, function(x) sum(is.na(x)))

Final <-  Final[, !duplicated(colnames(Final))]
dim(Final)
dim(x_train)
sum(is.na(Final))

col.order <- sapply(colnames(x_train), function(x, D) which(colnames(D) == x), D=Final)
col.order <- unlist(col.order)
Final <- Final[ , col.order]

x_test <- Final
rm(Final)
gc()


#### No adding correct variables 

# save .RData file
tic()
save(x_test, y_test, file = "G:\\Test_mh_event90.RData")
toc()

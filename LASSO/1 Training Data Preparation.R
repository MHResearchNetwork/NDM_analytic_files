##
## This file:
##   Reads in the raw data and transforms it to the 
##   appropriate format. 
##   We transfrom the following variables:
##    (SEX, RACE, HISPANIC, HHLD_INC, COLL_DEG, INS_ACA, INS_MEDICAID, 
##    INS_COMMERCIAL, INS_PRIVATEPAY, INS_STATESUBSIDIZED, INS_SELFFUNDED, 
##    INS_MEDICARE, INS_HIGHDEDUCTIBLE, INS_OTHER)
##


## Libraries
library(data.table)
library(tictoc)
library(fastDummies)
library(dplyr)


################################
## Train_mh_per_PARA_AWS_enr1
################################

## Data
tic()
load(file="G:\\Train_mh.Rdata")
toc()


## Updated data storage output directory
outdir <-"G:\\data"


## Preliminary params/storage objects
fname <- "Train_mh_per_AWS_enr1"
# data <- get(fname)


## Working through data

# Remove missing events and unknown sex
tic()
assign(fname, subset(get(fname), !is.na(EVENT90)))
toc()

# SEX categories
summary(as.factor(get(fname)$SEX))


# Create outcome matrix
tic()
assign("y_train", get(fname)[,c("EVENT90")])
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
get(fname)[which(is.na(get(fname)[ , na.colname, with=F]) == T) , na.colname] <- 0
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

# Create xtrain
tic()
x_train <- get(fname)
toc()

# Make cros-validation number vector
tic()
assign("splitn", x_train[,c("CV_PER")])
toc()

# Remove cross-val numbering and Event90 from get(fname)
tic()
indx <- which(names(x_train)%in%c("CV_PER"))
x_train[, (indx) := NULL]
toc()

# Normalize continuous variables
tic()
for (j in 1:ncol(x_train)) set(x_train, j = j, value = (x_train[[j]] - mins[j])/(maxs[j]-mins[j]))
toc()

# Put everything back together
tic()
x_train <- bind_cols(x_train, characters)
x_train <- bind_cols(x_train, int_cats)
toc()

# Remove things
rm(characters)
rm(int_cats)
rm(Train_mh_per_AWS_enr1)
rm(mins)
rm(maxs)
gc()

# transform xtrain to a matrix
tic()
x_train <- as.matrix(x_train)
y_train <- as.matrix(y_train)
splitn <- as.matrix(splitn)
toc()
gc()

x_train[which(is.na(x_train) == T)] <- 0

# save .RData file
tic()
save(x_train, y_train, splitn, file = paste0(outdir, "\\", "Train_mh", ".RData"))
toc()



rm(list = ls())
gc()

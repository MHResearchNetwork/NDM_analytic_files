
## clear memory
rm(list=ls())
gc()

## load packages
library(data.table)
library(bit64)
library(ranger)
library(ROCR)

## get session information
sessionInfo()

## set working directory
data_dir <- "C:/Users/Administrator/Downloads"
setwd(data_dir)

### load summary objects of OUT-OF-SAMPLE performance and organize for easy comparisons
### i indicates model
for(i in 1:13){
  print(i)
  load(file=paste("tune_mh_per_resmod", i, "_e90.Rdata", sep=""))
  objectName <- as.name(paste("res.mod", i, sep=""))
  rescur <- data.table(
    
    model = i,
    mtry = eval(objectName)$mtry,
    num.trees = eval(objectName)$num.trees,
    min.node.size = eval(objectName)$min.node.size,
    AUC = eval(objectName)$AUC,
    brier.sc = eval(objectName)$`Brier Score`,
    
    sens_95 = eval(objectName)$Performance$Sens[[4]],
    spec_95 = eval(objectName)$Performance$Spec[[4]],
    ppv_95 = eval(objectName)$Performance$PPV[[4]],
    npv_95 = eval(objectName)$Performance$NPV[[4]],
    fpr_95 = eval(objectName)$Performance$FPR[[4]],
    fnr_95 = eval(objectName)$Performance$FNR[[4]],
    fscore_95 = eval(objectName)$Performance$Fscore[[4]],
    
    sens_99 = eval(objectName)$Performance$Sens[[5]],
    spec_99 = eval(objectName)$Performance$Spec[[5]],
    ppv_99 = eval(objectName)$Performance$PPV[[5]],
    npv_99 = eval(objectName)$Performance$NPV[[5]],
    fpr_99 = eval(objectName)$Performance$FPR[[5]],
    fnr_99 = eval(objectName)$Performance$FNR[[5]],
    fscore_99 = eval(objectName)$Performance$Fscore[[5]],
    
    sens_99.5 = eval(objectName)$Performance$Sens[[6]],
    spec_99.5 = eval(objectName)$Performance$Spec[[6]],
    ppv_99.5 = eval(objectName)$Performance$PPV[[6]],
    npv_99.5 = eval(objectName)$Performance$NPV[[6]],
    fpr_99.5 = eval(objectName)$Performance$FPR[[6]],
    fnr_99.5 = eval(objectName)$Performance$FNR[[6]],
    fscore_99.5 = eval(objectName)$Performance$Fscore[[6]],
    
    mod.hrs = eval(objectName)$mod.hrs,
    ptrain.hrs = eval(objectName)$ptrain.hrs,
    ptest.hrs = eval(objectName)$ptest.hrs
    
  )
  rescur[,all.5cv.hrs := mod.hrs+ptrain.hrs+ptest.hrs]
  if(i==1){resmods <- rescur} else{resmods <- rbind(resmods,rescur)}
  rm(objectName,rescur)
}


### display selected results
print(
  round(resmods[order(num.trees,min.node.size,mtry),
                c("num.trees","min.node.size","mtry",
                  "AUC","brier.sc",
                  "sens_95","ppv_95","fscore_95",
                  "sens_99","ppv_99","fscore_99",
                  "sens_99.5","ppv_99.5","fscore_99.5",
                  "all.5cv.hrs"
                ), with=FALSE],3)
  , row.names=FALSE)


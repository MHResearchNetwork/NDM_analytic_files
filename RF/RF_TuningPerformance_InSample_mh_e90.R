
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

### load summary objects of IN-SAMPLE performance and organize for easy comparisons
### i indicates model, j indicates fold
for(i in 1:13){
  for(j in 1:5){
    print(c(i,j))
    load(file=paste("tune_mh_per_trainmod", i, "_", j, "_e90.Rdata", sep=""))
    objectName <- as.name(paste("res.mod", i, "_", j, sep=""))
    rescur <- data.table(
      
      model = i,
      fold = j,
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
      fscore_99.5 = eval(objectName)$Performance$Fscore[[6]]
      
    )
    if(i==1 & j==1){resmods <- rescur} else{resmods <- rbind(resmods,rescur)}
    rm(objectName,rescur)
  }
}
cvmods <- resmods[,.(mtry = min(mtry),
                     num.trees = min(num.trees),
                     min.node.size = min(min.node.size),
                     
                     AUC = mean(AUC),
                     min_AUC = min(AUC),
                     max_AUC = max(AUC),
                     
                     brier.sc = mean(brier.sc),
                     min_brier.sc = min(brier.sc),
                     max_brier.sc = max(brier.sc),
                     
                     sens_95 = mean(sens_95),
                     min_sens_95 = min(sens_95),
                     max_sens_95 = max(sens_95),
                     spec_95 = mean(spec_95),
                     min_spec_95 = min(spec_95),
                     max_spec_95 = max(spec_95),
                     ppv_95 = mean(ppv_95),
                     min_ppv_95 = min(ppv_95),
                     max_ppv_95 = max(ppv_95),
                     npv_95 = mean(npv_95),
                     min_npv_95 = min(npv_95),
                     max_npv_95 = max(npv_95),
                     fpr_95 = mean(fpr_95),
                     min_fpr_95 = min(fpr_95),
                     max_fpr_95 = max(fpr_95),
                     fnr_95 = mean(fnr_95),
                     min_fnr_95 = min(fnr_95),
                     max_fnr_95 = max(fnr_95),
                     fscore_95 = mean(fscore_95),
                     min_fscore_95 = min(fscore_95),
                     max_fscore_95 = max(fscore_95),
                     
                     sens_99 = mean(sens_99),
                     min_sens_99 = min(sens_99),
                     max_sens_99 = max(sens_99),
                     spec_99 = mean(spec_99),
                     min_spec_99 = min(spec_99),
                     max_spec_99 = max(spec_99),
                     ppv_99 = mean(ppv_99),
                     min_ppv_99 = min(ppv_99),
                     max_ppv_99 = max(ppv_99),
                     npv_99 = mean(npv_99),
                     min_npv_99 = min(npv_99),
                     max_npv_99 = max(npv_99),
                     fpr_99 = mean(fpr_99),
                     min_fpr_99 = min(fpr_99),
                     max_fpr_99 = max(fpr_99),
                     fnr_99 = mean(fnr_99),
                     min_fnr_99 = min(fnr_99),
                     max_fnr_99 = max(fnr_99),
                     fscore_99 = mean(fscore_99),
                     min_fscore_99 = min(fscore_99),
                     max_fscore_99 = max(fscore_99),
                     
                     sens_99.5 = mean(sens_99.5),
                     min_sens_99.5 = min(sens_99.5),
                     max_sens_99.5 = max(sens_99.5),
                     spec_99.5 = mean(spec_99.5),
                     min_spec_99.5 = min(spec_99.5),
                     max_spec_99.5 = max(spec_99.5),
                     ppv_99.5 = mean(ppv_99.5),
                     min_ppv_99.5 = min(ppv_99.5),
                     max_ppv_99.5 = max(ppv_99.5),
                     npv_99.5 = mean(npv_99.5),
                     min_npv_99.5 = min(npv_99.5),
                     max_npv_99.5 = max(npv_99.5),
                     fpr_99.5 = mean(fpr_99.5),
                     min_fpr_99.5 = min(fpr_99.5),
                     max_fpr_99.5 = max(fpr_99.5),
                     fnr_99.5 = mean(fnr_99.5),
                     min_fnr_99.5 = min(fnr_99.5),
                     max_fnr_99.5 = max(fnr_99.5),
                     fscore_99.5 = mean(fscore_99.5),
                     min_fscore_99.5 = min(fscore_99.5),
                     max_fscore_99.5 = max(fscore_99.5)
                     
),
by=list(model)]


### display selected results (note these are means)
print(
  round(cvmods[order(num.trees,min.node.size,mtry),
               c("num.trees","min.node.size","mtry",
                 "AUC",
                 "brier.sc",
                 "sens_95","ppv_95","fscore_95",
                 "sens_99","ppv_99","fscore_99",
                 "sens_99.5","ppv_99.5","fscore_99.5"
               ), with=FALSE],3)
  , row.names=FALSE)


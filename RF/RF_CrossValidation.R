#### Program to perform cross-validation and save results ####

## start k-fold cross-validation
for(k in 1:5){
  
  ## track process and set seed
  print(k)
  curseed <- (cur.mtry + cur.num.trees + cur.min.node.size + k)
  
  ## fit model
  mod.time <- system.time({
    rf_mod <- ranger(dependent.variable.name="OUTCOME", data=tunedata[CV_PER!=k, c(feat,"OUTCOME"), with=FALSE],
                     num.trees=cur.num.trees, mtry=cur.mtry,
                     importance="none", write.forest=TRUE, probability=TRUE,
                     min.node.size=cur.min.node.size,
                     respect.unordered.factors="partition",
                     oob.error=FALSE, save.memory=FALSE, seed=curseed)
  })
  gc()
  
  ## generate predictions on training data
  ptrain.time <- system.time({
    pred_train <- predict(rf_mod, tunedata[CV_PER!=k, c(feat), with=FALSE])
  })
  gc()
  
  ## generate predictions on test data
  ptest.time <- system.time({
    pred_test <- predict(rf_mod, tunedata[CV_PER==k, c(feat), with=FALSE])
  })
  gc()
  
  ## store key fields, predictions, and outcomes
  res_train <- cbind(tunedata[CV_PER!=k, c("PERSON_ID","VISIT_SEQ","OUTCOME","CV_PER"), with=FALSE], PRED=pred_train$predictions[,2])
  res_test <- cbind(tunedata[CV_PER==k, c("PERSON_ID","VISIT_SEQ","OUTCOME","CV_PER"), with=FALSE], PRED=pred_test$predictions[,2])
  gc()
  
  ## save model and results
  save(rf_mod, res_train, res_test, mod.time, ptrain.time, ptest.time,
       file = paste(cur.mod.name, "_", k, "_", cur.mod.outcome, ".Rdata", sep=""))
  gc()
  
  ## clean workspace
  rm(rf_mod,res_train,res_test,mod.time,ptrain.time,ptest.time,pred_train,pred_test,curseed)
  gc()
  
}

## clean workspace
rm(cur.mod.name,cur.mod.outcome,cur.mtry,cur.num.trees,cur.min.node.size,k)
gc()

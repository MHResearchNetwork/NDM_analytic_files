##
## Functions for running LASSO models
##


#### perf.results - function to provide calibration and performance tables, metrics, and plots
####

perf.results <- function(pred, # predicted probability of outcome=1 for each observation in the test set
                         outcome, # 0/1 numeric outcome for each observation in the test set
                         train.pred, # predicted probability of outcome=1 for each observation in the training set
                         strata.pctile=c(0.50, 0.75, 0.90, 0.95, 0.99, 0.995), # vector of percentiles to use for calibration/performance strata
                         # (provide values from 0-1, not including 0 or 1)
                         inc.plots=FALSE # produce plots? 
                         # (plots can be slow to create)
) {    
  ## data.table version 1.12.2
  library(data.table)
  ## ROCR version 1.0.7
  library(ROCR)
  
  # identify cutpoints in training data that correspond to provided strata percentiles
  train.pctile <- quantile(train.pred, strata.pctile)   
  
  # create calibration table
  calib.tab <- data.table(t(mapply(function(x,y){rbind(mean(pred[pred>=x & pred<y]),
                                                       mean(outcome[pred>=x & pred<y]))},
                                   c(0,train.pctile), c(train.pctile,1.01))))
  calib.tab <- data.table(paste(c("0%",names(train.pctile)), c(names(train.pctile),"100%"), sep="-"), calib.tab)
  names(calib.tab) <- c("Pctile","AvgPredRisk","ObsRisk")
  
  # create ROCR prediction object and table of values (needed to construct performance table and plots)
  # prediction is a function in the ROCR package
  obj.rocr <- prediction(pred, outcome)
  tab.rocr <- data.table(RiskCutpoint=obj.rocr@cutoffs[[1]],
                         Sens=performance(obj.rocr,"sens")@y.values[[1]],
                         Spec=performance(obj.rocr,"spec")@y.values[[1]],
                         PPV=performance(obj.rocr,"ppv")@y.values[[1]],
                         NPV=performance(obj.rocr,"npv")@y.values[[1]],
                         FPR=performance(obj.rocr,"fpr")@y.values[[1]],
                         FNR=performance(obj.rocr,"fnr")@y.values[[1]],
                         Fscore=performance(obj.rocr,"f")@y.values[[1]],
                         TP=obj.rocr@tp[[1]],
                         FP=obj.rocr@fp[[1]],
                         FN=obj.rocr@fn[[1]],
                         TN=obj.rocr@tn[[1]])
  
  # create performance table
  perf.tab <- rbindlist(lapply(train.pctile, function(x){last(tab.rocr[RiskCutpoint>=x])}))
  perf.tab$RiskCutpoint <- train.pctile
  perf.tab <- data.table(Pctile=names(train.pctile), perf.tab)
  
  # compute auc
  auc.rocr <- performance(obj.rocr,"auc")
  
  # construct plots, if requested (change range of x and y-axes, as needed)
  if(inc.plots==TRUE){
    
    # plot of ROC curve
    roc.rocr <- performance(obj.rocr,"tpr","fpr")
    plot(roc.rocr, main=paste("AUC =", round(auc.rocr@y.values[[1]],4)), cex.main=1)
    abline(a=0,b=1)
    
    # plot of Precision-Recall curve
    prec.rocr <- performance(obj.rocr,"prec","rec")
    plot(prec.rocr, main="Precision-Recall curve", cex.main=1, xlim=c(0,0.15), ylim=c(0,1))
    
    # plot of PPV, NPV, FPR, FNR curves using an x-axis defined by the percentile cutpoints in training data
    pctile.plot <- seq(0, 1, 0.005)
    train.pctile.plot <- quantile(train.pred, pctile.plot)
    tab.rocr.plot <- rbindlist(lapply(train.pctile.plot, function(x){last(tab.rocr[RiskCutpoint>=x])}))
    plot(x=pctile.plot*100, y=tab.rocr.plot$PPV, xlab="Risk Cutpoint Percentile", ylab="PPV", main="PPV-curve", type='l', lwd=1, cex.main=1, xlim=c(0,100), ylim=c(0,0.3))
    plot(x=pctile.plot*100, y=tab.rocr.plot$NPV, xlab="Risk Cutpoint Percentile", ylab="NPV", main="NPV-curve", type='l', lwd=1, cex.main=1, xlim=c(0,100), ylim=c(0.99,1))
    plot(x=pctile.plot*100, y=tab.rocr.plot$FPR, xlab="Risk Cutpoint Percentile", ylab="FPR", main="FPR-curve", type='l', lwd=1, cex.main=1, xlim=c(0,100), ylim=c(0,1))
    plot(x=pctile.plot*100, y=tab.rocr.plot$FNR, xlab="Risk Cutpoint Percentile", ylab="FNR", main="FNR-curve", type='l', lwd=1, cex.main=1, xlim=c(0,100), ylim=c(0,1))
  }
  
  # return calibration table, preformance table, and AUC 
  res.list <- list(calib.tab, perf.tab, auc.rocr@y.values[[1]])
  names(res.list) <- c("Calibration","Performance","AUC")
  return(res.list)
  
}
####

auc <- function(n, x, y) {
  x_1 <- sample(x[y==1], n, replace=T)
  x_0 <- sample(x[y==0], n, replace=T)
  (sum(x_1 > x_0) + .5*sum(x_1==x_0))/n
}

bin_cross_entropy <- function(y_pred, y_true) {
  eps <- 1e-15
  y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
  LogLoss <- -mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
  return(LogLoss)
}

perf.glm <- function(model, yval, xval) {
  preds_train <- as.matrix(predict(model, type = "response"))
  preds_val <- as.matrix(predict(model, newdata=as.data.frame(xval), type = "response"))
  
  c(perf.results(pred=preds_val, outcome=yval, train.pred=preds_train), 
    TrainLoss = bin_cross_entropy(preds_val, yval))
}

perf.nnet.logistic.cat <- function(results, yval) {
  c(perf.results(pred=results$pred_validation[,2], outcome=yval, 
                 train.pred=results$pred_training[,2]), TrainLoss = bin_cross_entropy(results$pred_validation[,2], yval))
}

perf.nnet.logistic.cat2 <- function(predsval, yval, predstest) {
  c(perf.results(pred=predsval, outcome=yval, 
                 train.pred=predstest), TrainLoss = bin_cross_entropy(predsval, yval))
}



norm.data <- function(X) {
  (X - min(X, na.rm=TRUE))/(max(X)-min(X))
}

get_coefs <- function(model) {
  coefs <- get_weights(model)
  c(coefs[[2]][2]-coefs[[2]][1], coefs[[1]][, 2] - coefs[[1]][, 1]) 
}



extractCalibration <- function(results) {
  results[["Calibration"]]
}
extractPerformance <- function(results) {
  results[["Performance"]]
}
extractAUC <- function(results) {
  results[["AUC"]]
}
extractTrainLoss<- function(results) {
  results[["TrainLoss"]]
}
# 
auc_roc <- R6::R6Class("ROC", 
                       inherit = KerasCallback, 
                       
                       public = list(
                         newmetrics = NULL,
                         
                         initialize = function(training_data, validation_data){
                           self$x = training_data[[1]]
                           self$y = training_data[[2]]
                           self$x_val = validation_data[[1]]
                           self$y_val = validation_data[[2]]
                         },
                         
                         on_train_begin = function(logs=NULL){
                           
                           return()
                         },
                         
                         on_train_end = function(logs=NULL){
                           assign("auc", unlist(self$newmetrics), envir=globalenv())
                           return()
                         },
                         
                         on_epoch_begin = function(epoch, logs=NULL) {
                           return()
                         },
                         
                         on_epoch_end = function(epoch, logs=NULL){
                           y_pred_val <- self$model$predict(self$x_val)
                           score_val <- Metrics::auc(actual=self$y_val, predicted=y_pred_val)
                           self$newmetrics <- c(self$newmetrics, score_val)
                           
                         },
                         
                         on_batch_begin = function(batch, logs=NULL) {
                           return()
                         },
                         
                         on_batch_end = function(batch, logs=NULL) {
                           return()
                         }
                       ), lock_objects = FALSE
)

save_pred <- R6::R6Class("PRED", 
                         inherit = KerasCallback, 
                         
                         public = list(
                           
                           initialize = function(training_data, run_id){
                             self$x = training_data[[1]]
                             self$y = training_data[[2]]
                             # self$x_val = validation_data[[1]]
                             # self$y_val = validation_data[[2]]
                             self$fname = run_id
                           },
                           
                           on_train_begin = function(logs=NULL){
                             
                             return()
                           },
                           
                           on_train_end = function(logs=NULL){
                             return()
                           },
                           
                           on_epoch_begin = function(epoch, logs=NULL) {
                             return()
                           },
                           
                           on_epoch_end = function(epoch, logs=NULL){
                             if (epoch != 0 & epoch %% 19 == 0) {
                               # y_pred_val <- self$model$predict(self$x_val)
                               y_pred <- self$model$predict(self$x)
                               if(epoch + 1 < 10) {
                                 # saveRDS(cbind(y_pred_val[, 2], self$y_val), paste0("RDS/", self$fname, "_val_000", epoch + 1, ".RDS"))
                                 saveRDS(cbind(y_pred[, 2], self$y), paste0("RDS/", self$fname, "_train_000", epoch + 1, ".RDS"))
                               } else if(epoch + 1 >= 10 & epoch + 1 < 100) {
                                 # saveRDS(cbind(y_pred_val[, 2], self$y_val), paste0("RDS/", self$fname, "_val_00", epoch + 1, ".RDS"))
                                 saveRDS(cbind(y_pred[, 2], self$y), paste0("RDS/", self$fname, "_train_00", epoch + 1, ".RDS"))
                               } else if(epoch + 1 == 100) {
                                 # saveRDS(cbind(y_pred_val[, 2], self$y_val), paste0("RDS/", self$fname, "_val_0", epoch + 1, ".RDS"))
                                 saveRDS(cbind(y_pred[, 2], self$y), paste0("RDS/", self$fname, "_train_0", epoch + 1, ".RDS"))
                               } else if(epoch + 1 < 1000) {
                                 # saveRDS(cbind(y_pred_val[, 2], self$y_val), paste0("RDS/", self$fname, "_val_0", epoch + 1, ".RDS"))
                                 saveRDS(cbind(y_pred[, 2], self$y), paste0("RDS/", self$fname, "_train_0", epoch + 1, ".RDS"))
                               } else {
                                 # saveRDS(cbind(y_pred_val[, 2], self$y_val), paste0("RDS/", self$fname, "_val_", epoch + 1, ".RDS"))
                                 saveRDS(cbind(y_pred[, 2], self$y), paste0("RDS/", self$fname, "_train_", epoch + 1, ".RDS"))
                               }
                             }
                           },
                           
                           on_batch_begin = function(batch, logs=NULL) {
                             return()
                           },
                           
                           on_batch_end = function(batch, logs=NULL) {
                             return()
                           }
                         ), lock_objects = FALSE
)

save_pred_fwrite <- R6::R6Class("PREDFAST", 
                                inherit = KerasCallback, 
                                
                                public = list(
                                  
                                  initialize = function(training_data, validation_data, run_id){
                                    self$x = training_data[[1]]
                                    self$y = training_data[[2]]
                                    self$x_val = validation_data[[1]]
                                    self$y_val = validation_data[[2]]
                                    self$fname = run_id
                                  },
                                  
                                  on_train_begin = function(logs=NULL){
                                    
                                    return()
                                  },
                                  
                                  on_train_end = function(logs=NULL){
                                    return()
                                  },
                                  
                                  on_epoch_begin = function(epoch, logs=NULL) {
                                    return()
                                  },
                                  
                                  on_epoch_end = function(epoch, logs=NULL){
                                    if (epoch == 0 | epoch %% 10 == 0) {
                                      y_pred_val <- self$model$predict(self$x_val)
                                      y_pred <- self$model$predict(self$x)
                                      if(epoch + 1 < 10) {
                                        fwrite(cbind(y_pred_val[, 2], self$y_val), paste0("RDS/", self$fname, "_val_000", epoch + 1, ".csv"), sep = ",")
                                        fwrite(cbind(y_pred[, 2], self$y), paste0("RDS/", self$fname, "_train_000", epoch + 1, ".csv"), sep = ",")
                                      } else if(epoch + 1 >= 10 & epoch + 1 < 100) {
                                        fwrite(cbind(y_pred_val[, 2], self$y_val), paste0("RDS/", self$fname, "_val_00", epoch + 1, ".csv"), sep = ",")
                                        fwrite(cbind(y_pred[, 2], self$y), paste0("RDS/", self$fname, "_train_00", epoch + 1, ".csv"), sep = ",")
                                      } else if(epoch + 1 == 100) {
                                        fwrite(cbind(y_pred_val[, 2], self$y_val), paste0("RDS/", self$fname, "_val_0", epoch + 1, ".csv"), sep = ",")
                                        fwrite(cbind(y_pred[, 2], self$y), paste0("RDS/", self$fname, "_train_0", epoch + 1, ".csv"), sep = ",")
                                      } else if(epoch + 1 < 1000) {
                                        fwrite(cbind(y_pred_val[, 2], self$y_val), paste0("RDS/", self$fname, "_val_0", epoch + 1, ".csv"), sep = ",")
                                        fwrite(cbind(y_pred[, 2], self$y), paste0("RDS/", self$fname, "_train_0", epoch + 1, ".csv"), sep = ",")
                                      } else {
                                        fwrite(cbind(y_pred_val[, 2], self$y_val), paste0("RDS/", self$fname, "_val_", epoch + 1, ".csv"), sep = ",")
                                        fwrite(cbind(y_pred[, 2], self$y), paste0("RDS/", self$fname, "_train_", epoch + 1, ".csv"), sep = ",")
                                      }
                                    }
                                  },
                                  
                                  on_batch_begin = function(batch, logs=NULL) {
                                    return()
                                  },
                                  
                                  on_batch_end = function(batch, logs=NULL) {
                                    return()
                                  }
                                ), lock_objects = FALSE
)



model_gen_lasso <- function(lambda, shape, lr) {
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = 1, activation = 'sigmoid', 
                input_shape = shape, 
                kernel_regularizer = regularizer_l1(l=lambda))
  
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(lr = lr)
  )
  
}



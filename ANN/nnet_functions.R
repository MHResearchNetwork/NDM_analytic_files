run_base_model <- function(xtrain, ytrain, xval, yval, n_epochs=10, batch_size=128, learn_rate=0.001, callback_list=NULL) {
  gc()
  
  get_coefs <- function(model) {
    coefs <- get_weights(model)
    c(coefs[[2]][2]-coefs[[2]][1], coefs[[1]][, 2] - coefs[[1]][, 1]) 
  }
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = 2, activation = 'softmax', 
                input_shape = dim(xtrain)[[2]])
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learn_rate)
  )
  
  history <- model %>% fit(
    xtrain, to_categorical(ytrain), 
    epochs = n_epochs, batch_size = batch_size,
    callbacks = callback_list,
    validation_data = list(xval, to_categorical(yval))
  )
  
  preds_val   <- model %>% predict_proba(xval, batch_size = batch_size)
  preds_train   <- model %>% predict_proba(xtrain, batch_size = batch_size)
  
  
  list(model=model, history=history, coefficients=get_coefs(model), pred_validation=preds_val, pred_training=preds_train)
}

run_nnet_1hidden <- function(xtrain, ytrain, xval, yval, 
                             nnodes, n_epochs=10, batch_size=128, 
                             learn_rate=0.001, callback_list=NULL) {
  gc()
  
  get_coefs <- function(model) {
    coefs <- get_weights(model)
    c(coefs[[2]][2]-coefs[[2]][1], coefs[[1]][, 2] - coefs[[1]][, 1]) 
  }
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = nnodes, activation = 'relu', 
                input_shape = dim(xtrain)[[2]]) %>%
    layer_dense(units = 2, activation = 'softmax')
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learn_rate)
  )
  
  history <- model %>% fit(
    xtrain, to_categorical(ytrain), 
    epochs = n_epochs, batch_size = batch_size,
    callbacks = callback_list,
    validation_data = list(xval, to_categorical(yval))
  )
  
  preds_val   <- model %>% predict_proba(xval, batch_size = batch_size)
  preds_train   <- model %>% predict_proba(xtrain, batch_size = batch_size)
  
  
  list(model=model, history=history, coefficients=get_coefs(model), pred_validation=preds_val, pred_training=preds_train)
}


run_nnet_base <- function(xtrain, ytrain, xval, yval, nnodes, n_epochs=10, batch_size=128, learn_rate=0.001, callback_list=NULL) {
  gc()
  
  # get_coefs <- function(model) {
  #   coefs <- get_weights(model)
  #   c(coefs[[2]][2]-coefs[[2]][1], coefs[[1]][, 2] - coefs[[1]][, 1]) 
  # }
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = nnodes, activation = 'relu', 
                input_shape = dim(xtrain)[[2]]) %>%
    layer_dense(units = nnodes, activation = 'relu') %>%
    layer_dense(units = 2, activation = 'softmax')
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learn_rate)
  )
  
  history <- model %>% fit(
    xtrain, to_categorical(ytrain), 
    epochs = n_epochs, batch_size = batch_size,
    callbacks = callback_list,
    validation_data = list(xval, to_categorical(yval))
  )
  
  preds_val   <- model %>% predict_proba(xval, batch_size = batch_size)
  preds_train   <- model %>% predict_proba(xtrain, batch_size = batch_size)
  
  
  list(model=model, history=history, pred_validation=preds_val, pred_training=preds_train)
}

run_nnet_base_reg <- function(xtrain, ytrain, xval, yval, nnodes, n_epochs=10, batch_size=128, learn_rate=0.001, lambda, callback_list=NULL) {
  gc()
  
  # get_coefs <- function(model) {
  #   coefs <- get_weights(model)
  #   c(coefs[[2]][2]-coefs[[2]][1], coefs[[1]][, 2] - coefs[[1]][, 1]) 
  # }
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = nnodes, activation = 'relu', 
                input_shape = dim(xtrain)[[2]], 
                kernel_regularizer =  regularizer_l1(l=lambda)) %>%
    layer_dense(units = nnodes, activation = 'relu', 
                kernel_regularizer =  regularizer_l2(l=lambda)) %>%
    layer_dense(units = 2, activation = 'softmax', 
                kernel_regularizer =  regularizer_l2(l=lambda))
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learn_rate)
  )
  
  history <- model %>% fit(
    xtrain, to_categorical(ytrain), 
    epochs = n_epochs, batch_size = batch_size,
    callbacks = callback_list,
    validation_data = list(xval, to_categorical(yval))
  )
  
  preds_val   <- model %>% predict_proba(xval, batch_size = batch_size)
  preds_train   <- model %>% predict_proba(xtrain, batch_size = batch_size)
  
  
  list(model=model, history=history, pred_validation=preds_val, pred_training=preds_train)
}



run_nnet_1hidden_reg <- function(xtrain, ytrain, xval, yval, lambda=0.01, nnodes, n_epochs=10, batch_size=128, learn_rate=0.001, callback_list=NULL) {
  gc()
  
  get_coefs <- function(model) {
    coefs <- get_weights(model)
    c(coefs[[4]][2]-coefs[[4]][1], coefs[[3]][, 2] - coefs[[3]][, 1], 
      unlist(coefs[1:2])) 
  }
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = nnodes, activation = 'relu', 
                input_shape = dim(xtrain)[[2]], 
                kernel_regularizer =  regularizer_l1(l=lambda)) %>%
    layer_dense(units = 2, activation = 'softmax', 
                kernel_regularizer =  regularizer_l2(l=lambda))
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learn_rate)
  )
  
  history <- model %>% fit(
    xtrain, to_categorical(ytrain), 
    epochs = n_epochs, batch_size = batch_size,
    callbacks = callback_list,
    validation_data = list(xval, to_categorical(yval))
  )
  
  preds_val   <- model %>% predict_proba(xval, batch_size = batch_size)
  preds_train   <- model %>% predict_proba(xtrain, batch_size = batch_size)
  
  
  list(model=model, history=history, coefficients=get_coefs(model), pred_validation=preds_val, pred_training=preds_train)
}

run_base_model_lasso <- function(xtrain, ytrain, xval, yval, lambda=0.01, n_epochs=10, batch_size=128, learn_rate=0.001, callback_list=NULL) {
  gc()
  
  get_coefs <- function(model) {
    coefs <- get_weights(model)
    c(coefs[[2]][2]-coefs[[2]][1], coefs[[1]][, 2] - coefs[[1]][, 1]) 
  }
  
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = 2, activation = 'softmax', 
                input_shape = dim(xtrain)[[2]], 
                kernel_regularizer = regularizer_l1(l=lambda))
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learn_rate)
  )
  
  history <- model %>% fit(
    xtrain, to_categorical(ytrain), 
    epochs = n_epochs, batch_size = batch_size,
    callbacks = callback_list,
    validation_data = list(xval, to_categorical(yval))
  )
  
  preds_val   <- model %>% predict_proba(xval, batch_size = batch_size)
  preds_train   <- model %>% predict_proba(xtrain, batch_size = batch_size)
  
  
  list(model=model, history=history, coefficients=get_coefs(model), pred_validation=preds_val, pred_training=preds_train)
}



run_base_model_lasso2 <- function(xtrain, ytrain, xval, yval, lambda=0.01, n_epochs=10, batch_size=128, learn_rate=0.001, callback_list=NULL) {
  gc()
  
  get_coefs <- function(model) {
    coefs <- get_weights(model)
    c(coefs[[2]], coefs[[1]])
  }
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = 1, activation = 'sigmoid', 
                input_shape = dim(xtrain)[[2]], kernel_regularizer =  regularizer_l1(l=lambda))
  
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(lr = learn_rate)
  )
  
  history <- model %>% fit(
    xtrain, ytrain, 
    epochs = n_epochs, batch_size = batch_size,
    callbacks = callback_list,
    validation_data = list(xval, yval)
  )
  
  preds_val   <- model %>% predict_proba(xval, batch_size = batch_size)
  preds_train   <- model %>% predict_proba(xtrain, batch_size = batch_size)
  
  
  list(model=model, history=history, coefficients=get_coefs(model), pred_validation=preds_val, pred_training=preds_train)
}

run_base_model_continue <- function(model,initial_epoch, xtrain, ytrain, xval, yval, n_epochs=10, batch_size=128, learn_rate=0.001, callback_list=NULL) {
  
  get_coefs <- function(mod) {
    coefs <- get_weights(mod)
    c(coefs[[2]][2]-coefs[[2]][1], coefs[[1]][, 2] - coefs[[1]][, 1]) 
  }
  
  history <- model %>% fit(
    xtrain, to_categorical(ytrain), 
    epochs = n_epochs, batch_size = batch_size,
    callbacks = callback_list,
    validation_data = list(xval, to_categorical(yval))
  )
  
  preds_val   <- model %>% predict_proba(xval, batch_size = batch_size)
  preds_train   <- model %>% predict_proba(xtrain, batch_size = batch_size)
  
  
  list(model=model, history=history, coefficients=get_coefs(model), pred_validation=preds_val, pred_training=preds_train)
}

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
  res.list <- list(calib.tab, cbind(perf.tab, "AUC"=auc.rocr@y.values[[1]], "Brier (MSE)"=mean((outcome-pred)^2)))
  names(res.list) <- c("Calibration","Performance")
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



plot.nnet.logistic.cat <- function(dir, id, model) {
  get_coefs <- function(model) {
    coefs <- get_weights(model)
    c(coefs[[4]][2]-coefs[[4]][1], coefs[[3]][, 2] - coefs[[3]][, 1], 
      unlist(coefs[1:2])) 
  }
  
  fnames <- list.files(dir)
  findx  <-  grep(id, fnames)
  nepochs <- length(findx)
  model_weights <- NULL
  
  for (i in findx) {
    model %>% load_model_weights_hdf5(
      file.path(paste0(dir, fnames[i]))
    )
    model_weights <- rbind(model_weights, get_coefs(model))
  }
  
  yll <- min(model_weights)
  ylr <- max(model_weights)
  plot(1:nepochs, model_weights[, 1], type='l', ylim=c(yll,ylr), lwd=2, xlab="Epochs", ylab="Coefficient Betas")
  apply(model_weights[,-1], 2, function(X) lines(1:nepochs, X, type='l', col = sample(colors(), 1), lwd=2))
}

plot.nnet.logistic.cat_2layer <- function(dir, id, model) {
  get_coefs <- function(model) {
    coefs <- get_weights(model)
    c(coefs[[6]][2]-coefs[[6]][1], coefs[[5]][, 2] - coefs[[5]][, 1]) 
  }
  
  fnames <- list.files(dir)
  findx  <-  grep(id, fnames)
  nepochs <- length(findx)
  model_weights <- NULL
  
  for (i in findx) {
    model %>% load_model_weights_hdf5(
      file.path(paste0(dir, fnames[i]))
    )
    model_weights <- rbind(model_weights, get_coefs(model))
  }
  
  yll <- min(model_weights)
  ylr <- max(model_weights)
  plot(1:nepochs, model_weights[, 1], type='l', ylim=c(yll,ylr), lwd=2, xlab="Epochs", ylab="Coefficient Betas")
  apply(model_weights[,-1], 2, function(X) lines(1:nepochs, X, type='l', col = sample(colors(), 1), lwd=2))
}

plot.nnet.logistic.cat_nohidden <- function(dir, id, model) {
  get_coefs <- function(model) {
    coefs <- get_weights(model)
    c(coefs[[2]][2]-coefs[[2]][1], coefs[[1]][, 2] - coefs[[1]][, 1]) 
  }
  
  fnames <- list.files(dir)
  findx  <-  grep(id, fnames)
  nepochs <- length(findx)
  model_weights <- NULL
  
  for (i in findx) {
    model %>% load_model_weights_hdf5(
      file.path(paste0(dir, fnames[i]))
    )
    model_weights <- rbind(model_weights, get_coefs(model))
  }
  
  yll <- min(model_weights)
  ylr <- max(model_weights)
  par(mar = c(5,5,2,5))
  plot(1:nepochs, model_weights[, 1], type='l', ylim=c(yll,ylr), lwd=2, xlab="Epochs", ylab="Coefficient Betas")
  apply(model_weights[,-1], 2, function(X) lines(1:nepochs, X, type='l', col = sample(colors(), 1), lwd=2))
  return(model_weights)
}

norm.data <- function(X) {
  (X - min(X, na.rm=TRUE))/(max(X)-min(X))
}

get_coefs <- function(model) {
  coefs <- get_weights(model)
  c(coefs[[2]][2]-coefs[[2]][1], coefs[[1]][, 2] - coefs[[1]][, 1]) 
}


res.nnet.logistic.cat <- function(dir, id, model, yval, xval, xtrain, batch_size) {
  
  fnames <- list.files(dir)
  findx  <-  grep(id, fnames)
  nepochs <- length(findx)
  model_weights <- NULL
  calibration <- NULL
  performance <- NULL
  auc <- NULL
  trainloss <- NULL
  Epoch <- 1
  for (i in findx) {
    model %>% load_model_weights_hdf5(
      file.path(paste0(checkpoint_dir, "/", fnames[i]))
    )
    current_res <- NULL
    preds_test <- NULL
    preds_train <- NULL
    preds_test    <- model %>% predict_proba(xval, batch_size = batch_size)
    preds_train   <- model %>% predict_proba(xtrain, batch_size = batch_size)
    current_res <- perf.nnet.logistic.cat2(preds_test[,2], yval, preds_train[,2])
    calibration <- rbind(calibration, cbind(current_res$Calibration, Epoch=Epoch))
    performance <- rbind(performance, cbind(current_res$Performance, Epoch=Epoch))
    auc <- cbind(auc,c(current_res$AUC, Epoch=Epoch))
    trainloss <- cbind(trainloss, c(current_res$TrainLoss, Epoch=Epoch))
    
    Epoch <- Epoch + 1

    gc()
  }
  list(calibration, performance, auc, trainloss)
}



res.nnet.logistic.cat <- function(dir, id, model, yval, xval, xtrain, batch_size) {
  
  fnames <- list.files(dir)
  findx  <-  grep(id, fnames)
  nepochs <- length(findx)
  
  lapply(findx, function(X) {
    model %>% load_model_weights_hdf5(
      file.path(paste0(checkpoint_dir, "/", fnames[X]))
    ) 
    Epoch <- which(findx == X)
    preds_test    <- model %>% predict_proba(xval, batch_size = batch_size)
    preds_train   <- model %>% predict_proba(xtrain, batch_size = batch_size)
    current_res <- perf.nnet.logistic.cat2(preds_test[,2], yval, preds_train[,2])
    output <- list(Calibration=cbind(current_res$Calibration, Epoch=Epoch),
         cbind(current_res$Performance, Epoch=Epoch),
         c(current_res$AUC, Epoch=Epoch),
         c(current_res$TrainLoss, Epoch=Epoch))
    names(output) <- c("Calibration", "Performance", "AUC", "TrainLoss")
    output
  })
  
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


model_gen_logistic <- function(shape, lr) {
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = 2, activation = 'softmax', 
                input_shape = shape)
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = lr)
  )
  
}


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

model_gen_1hidden <- function(lambda, nnodes, 
                              learn_rate=0.001, shape) {
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = nnodes, activation = 'relu', 
                input_shape = shape, 
                kernel_regularizer = regularizer_l1(l=lambda)) %>%
    layer_dense(units = 2, activation = 'softmax')
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learn_rate)
  )
}

model_gen_1hidden_sig <- function(lambda, nnodes, 
                              learn_rate=0.001, shape) {
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = nnodes, activation = 'relu', 
                input_shape = shape, 
                kernel_regularizer = regularizer_l1(l=lambda)) %>%
    layer_dense(units = 1, activation = 'sigmoid')
  
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(lr = learn_rate)
  )
}


model_gen_1hidden_noreg <- function(nnodes, 
                              learn_rate=0.001) {
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = nnodes, activation = 'relu', 
                input_shape = dim(xtrain)[[2]]) %>%
    layer_dense(units = 2, activation = 'softmax')
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learn_rate)
  )
}

model_gen_2hidden <- function(lambda, nnodes, 
                              learn_rate=0.001, shape) {
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = nnodes, activation = 'relu', 
                input_shape = shape, 
                kernel_regularizer = regularizer_l1(l=lambda)) %>%
    layer_dense(units = nnodes, activation = 'relu') %>%
    layer_dense(units = 2, activation = 'softmax')
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learn_rate)
  )
}

model_gen_2hidden <- function(lambda, nnodes, 
                              learn_rate=0.001, shape) {
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = nnodes, activation = 'relu', 
                input_shape = shape, 
                kernel_regularizer = regularizer_l1(l=lambda)) %>%
    layer_dense(units = nnodes, activation = 'relu') %>%
    layer_dense(units = 2, activation = 'softmax')
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learn_rate)
  )
}

model_gen_2hidden_sig <- function(lambda, nnodes, 
                              learn_rate=0.001, shape) {
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = nnodes, activation = 'relu', 
                input_shape = shape, 
                kernel_regularizer = regularizer_l1(l=lambda)) %>%
    layer_dense(units = nnodes, activation = 'relu') %>%
    layer_dense(units = 1, activation = 'sigmoid')
  
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(lr = learn_rate)
  )
}

crunch_nnet_results <- function(res) {
  
  mean_avgpredrisk <- do.call(c, lapply(1:4, function(i) {
    apply(do.call(cbind, lapply(1:5, function(j) {
      res[[i]][[j]]$Calibration[, 2]
    })), 1, mean)}))
  mean_obspredrisk <- do.call(c, lapply(1:4, function(i) {
    apply(do.call(cbind, lapply(1:5, function(j) {
      res[[i]][[j]]$Calibration[, 3]
    })), 1, mean)}))
  
  
  
  calibration<- cbind("Average Predicted"=mean_avgpredrisk, "Observed"=mean_obspredrisk)
  rownames(calibration) <- as.matrix(rbind(res[[1]][[1]]$Calibration[,1], res[[1]][[1]]$Calibration[,1],
                                           res[[1]][[1]]$Calibration[,1], res[[1]][[1]]$Calibration[,1]))
  
  mean_avgperf <- lapply(1:4, function(i) {
    this <- lapply(1:5, function(j) {
      as.matrix(res[[i]][[j]]$Performance[, -1])
    })
    thisaverage <- Reduce('+', this)/length(this)
    rownames(thisaverage) <- as.matrix(res[[1]][[1]]$Performance)[, 1]
    thisaverage
  })
  names(mean_avgperf) <- fnames
  
  mean_AUC <- lapply(1:4, function(i) {
    this <- lapply(1:5, function(j) {
      res[[i]][[j]]$AUC[[1]]
    })
    Reduce('+', this)/length(this)
  })
  
  mean_test_Brier <- lapply(1:4, function(i) {
    this <- lapply(1:5, function(j) {
      res[[i]][[j]]$`Brier Score`[[1]]
    })
    Reduce('+', this)/length(this)
  })
  list(calibration, do.call(rbind, mean_avgperf))
}




##
## Functions for running LASSO models
##

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



model_gen_lasso <- function(shape, lr) {
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = 1, activation = 'sigmoid', 
                input_shape = shape)
  
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(lr = lr)
  )
  
}



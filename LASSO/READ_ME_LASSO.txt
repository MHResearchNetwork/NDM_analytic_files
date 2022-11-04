
0 LASSO Functions: has all the functions needed to fit all LASSO models via Keras 
0 Logit Function: has all functions needed to fit all logit models via Keras
0 Performance Results Functions: Functions that extract performance measures 
1 Training Data Preparation: Creating and preparaing the training data to appropriately fir LASSO functions
2 Getting Lambda for Screening Models: Running a glmnet models on all and subset of the training models to get an 'optimal' lambda to use in the screenign models
3A Creating Interaction Datasets: Creates all datasets needed for screaning models
3B Fit Lasso Screening models: Fits all screenign lasso models on datasets created in 3A
3C Craeting Dataset After Screening: Creates final lasso dataset by selecting the top predictors in each of the screening models and combining those with the main predictors of interest
4A Fit Lasso Models for all Possible Lambda Values - CV: Fits lasso models for 20 lambda values on each fold and stores the predictions and models
4B Performance for CV - To find Optimal Lambda: Gets performance criteria for all models fit in 4A 
4C Fit Final lasso Model an all Trainign Data: Using optimal lambda from performance measures in 4B, this file fits a lasso model on all the trainign data
4D Creating Dataset of predictors selected by Final lasso model: creates a dataset of the predictors that were not shrunken to 0 in the model fit in 4C
4F Fit Final Logistic regression model: Using the dataset in 4D fita the final model (a logistic regression model) and stores the model and predictors
5 Performance for Final Logit model: calculates performance criteria for final logit model
6 Create Validation dataset: creates the validation dataset needed to get predictiosn from the final logit model 
7 Predictions for Validation dataset: gets predictions and performance criteria of the validation dataset using the final logit model
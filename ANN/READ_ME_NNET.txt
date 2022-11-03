File Descriptions:

data_preparation_mh_90D_attempt_test.R
Creates test (validation) dataset for use in NNET model development.

data_preparation_mh_90D_attempt_train.R
Create training datset for use in NNET model development.

nnet_functions.R
Author-development library of functions for working with NNET models with kera.

nnet_mh_90D_attempt_crossvalidation.R
Code to perform cross-validation of all tuning parameters, selection of the final model,
CV results, fitting of final model to training data and final predictions in the test dataset.

perf.results.R
Function for computing performance measure and calibration for developed models.

perf.results.id.R
Similar to perf.results.R with the addition of including unique event counts.
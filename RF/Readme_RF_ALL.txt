Explanation of following programs:

RF_TuningSearch_mh_e90.R - Performs tuning parameter search for random forest 90-day attempt model in MH sample
RF_CrossValidation.R - Called by the RF_TuningSearch_mh_e90.R program to faciliate cross-validation
RF_TuningSummarize_mh_e90.R - Creates summary objects of performance from tuning parameter search for random forest 90-day attempt model in MH sample
RF_ComputePerformance.R - Called by the RF_TuningSummarize_mh_e90.R program (and RF_FinalPerformance_mh_e90.R program) to faciliate performance calculations
RF_TuningPerformance_InSample_mh_e90.R - Compares in-sample performance across tuning parameters for random forest 90-day attempt model in MH sample
RF_TuningPerformance_OutSample_mh_e90.R - Compares out-of-sample performance across tuning parameters for random forest 90-day attempt model in MH sample
RF_FinalModel_mh_e90.R - Estimates final random forest 90-day attempt model in MH sample using selected tuning parameters
RF_FinalPerformance_mh_e90.R - Provides performance point estimates for final random forest 90-day attempt model in MH sample
ALL_FinalPerformanceCI_mh_e90.R - Performs boostrap and generates final performance estimates with CIs for all (RF, ANN, LAS, ensemble) 90-day attempt models in MH sample
ALL_BootPerformance.R - Called by the ALL_FinalPerformanceCI_mh_e90.R program to faciliate performance calculations for bootstrap 
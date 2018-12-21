# Principal Components Variable Importance Reconstruction (PC-VIR)
Data and R code for Principal Components Variable Importance Reconstruction (PC-VIR) method (Carignan & Egurtzegi)


This repository provides the data and R functions necessary to replicate the results from Carignan & Egurtzegi (in preparation).


Save the following data set into your R working directory: acoustic_data.Rda


Import the following functions into R: feature.PCA (from acoustic_PCA.R), logistic_training (from oral-nasal_training.R), PC.VIR (from PC-VIR.R), plot.imp.vars (from plot_imp_vars.R).


Use the code provided in PC-VIR_analysis.R to perform PCA-based variable importance reconstruction, and to plot the results.

Use the code provided in test_PC-VIR_results.R to validate models and compare prediction accuracy against a Partial Least Squares Discriminant Analysis (PLS-DA) model.

# Filename: PC-VIR_README.md
# Date: 2019-02-21
# Author: Christopher Carignan
# Associate investigator: Ander Egurtzegi
# Email: c.carignan@phonetik.uni-muenchen.de
# Institution: Institute of Phonetics and Speech Processing (IPS), Ludwig-Maximilians-Universität München, Munich, Germany

This repository provides data and R code for replicating the principal components variable importance reconstruction (PC-VIR) method from: Carignan & Egurtzegi (under revision), "Principal components variable importance reconstruction (PC-VIR): Exploring factor importance in multicollinear acoustic data for speech", Journal of the Acoustical Society of America.

Save the following data set into your R working directory: acoustic_data.Rda

The primary script used is 00_PC-VIR_analysis.R. NB: annotation is provided in this script to help guide the user through the analysis. 

The following scripts provide functions that are called within 00_PC-VIR_analysis.R:

01_feature_PCA.R
02_logistic_training.R
03_PC_VIR.R
04_plot_imp_vars.R

These scripts are numbered following the order that they appear (and are used) in the 00_PC-VIR_analysis.R script. Before performing the analysis, each of the four functions in these scripts must be saved into your environment by running each of the four scripts. 

There is an optional script, called 05_test_PC-VIR_results.R, which can be used to validate the PC-VIR models and compare prediction accuracy against a results obtained from partial least squares logistic regression models. NB: this model validation script should only be run *after* the following three functions are successfully run in sequential order: feature_PCA(), logistic_training(), and PC_VIR().

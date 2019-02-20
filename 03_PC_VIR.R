# Filename: 03_PC_VIR.R
# Date: 2019-02-20
# Author: Christopher Carignan
# Associate investigator: Ander Egurtzegi
# Email: c.carignan@phonetik.uni-muenchen.de
# Institution: Institute of Phonetics and Speech Processing (IPS), Ludwig-Maximilians-Universität München, Munich, Germany
# Description:
#   Function called from within the script 00_PC-VIR_analysis.R
#   Reconstructs the important variables (from acoustic_data.Rda) from the PCA-based regression models output by PC_VIR() from 03_PC_VIR.R


PC_VIR <- function(PCdata, traindata, mylogit, features, adjust=F){
  coeffs <- c()
  speakers <- unique(traindata$speaker)
  
  for (speaker in speakers){
    this.mod  <- mylogit[[speaker]] # Get the fitted logistic model for the speaker
    z.values  <- coef(summary(this.mod))[,"z value"] # Get the z-statistics for the PCs in the model
    p.values  <- coef(summary(this.mod))[,"Pr(>|z|)"] # Get the p-values for the PCs in the model
    pc.num    <- length(z.values) - 1 # How many PCs were there?
    
    # Preallocate arrays for the PC-VIR coefficients
    coeffs[[speaker]] <- data.frame(matrix(ncol = length(features), nrow = 0))
    colnames(coeffs[[speaker]]) <- features
    
    for (pc in 1:pc.num){
      # Get the statistic from the logistic model for this PC
      this.z.val  <- z.values[pc+1]
      this.p.val  <- p.values[pc+1]
      
      # Optional adjustment of z-statistic to control for Type I error 
      # (Bonferroni correction based on number of PCs retained)
      if (adjust){
        p.adj <- pc.num*this.p.val # adjust the p-value
        if (p.adj > 1){
          p.adj <- 1
        }
        this.z.val <- sign(this.z.val)*qnorm(1-p.adj/2) # adjust the z-statistic
      }
      
      # Get the PC coefficients
      these.coeffs  <- t(as.data.frame(PCdata[[speaker]]$model$rotation[,pc]))
      
      # Multiply the PC coefficients by the z-statistic for the PC to reconstruct the variable importance
      coeffs[[speaker]] <- rbind(coeffs[[speaker]],(these.coeffs * this.z.val)) 
    }
    coeffs[[speaker]] <- colSums(coeffs[[speaker]]) # Sum the weighted coefficients for the speaker
    coeffs[[speaker]] <- as.data.frame(coeffs[[speaker]]) # Add to data array
  }
  coeffs <- as.data.frame(coeffs) # Convert array
  colnames(coeffs) <- speakers # Add speaker names to columns
  
  return(as.data.frame(coeffs))
}
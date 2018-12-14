# Reconstruct the important variables from the PCA-based regression models

PC.VIR <- function(PCdata, traindata, mylogit, features){
  coeffs <- c()
  speakers <- unique(traindata$speaker)
  
  for (speaker in speakers){
    this.mod  <- mylogit[[speaker]] # Get the fitted logistic model for the speaker
    z.values  <- coef(summary(this.mod))[,"z value"] # Get the z-statistics for the PCs in the model
    pc.num    <- length(z.values) - 1 # How many PCs were there?
    
    # Preallocate an array for the PC-VIR coefficients
    coeffs[[speaker]] <- data.frame(matrix(ncol = length(features), nrow = 0))
    colnames(coeffs[[speaker]]) <- features
    
    for (pc in 1:pc.num){
      this.z.val    <- z.values[pc+1] # Get the z-statistic from the logistic model for this PC
      these.coeffs  <- t(as.data.frame(PCdata[[speaker]]$model$rotation[,pc])) # Get the PC coefficients
      
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
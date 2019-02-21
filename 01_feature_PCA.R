# Filename: 01_feature_PCA.R
# Date: 2019-02-20
# Author: Christopher Carignan
# Associate investigator: Ander Egurtzegi
# Email: c.carignan@phonetik.uni-muenchen.de
# Institution: Institute of Phonetics and Speech Processing (IPS), Ludwig-Maximilians-Universität München, Munich, Germany
# Description:
#   Function called from within the script 00_PC-VIR_analysis.R
#   Performs a principal components analysis (PCA) on the acoustic data from acoustic_data.Rda


feature_PCA <- function(my.data, features){
  PCdata <- c()
  
  # Separate PCA models are performed for each speaker
  for (speaker in unique(my.data$speaker)){
    thisdata  <- my.data[my.data$speaker==speaker,] # Get the speaker data
    
    # Perform a principal components analysis (PCA) on the speaker's acoustic features
    pca       <- prcomp(thisdata[ ,which(colnames(thisdata) %in% features)], scale=T, center=T)
    
    # Determine the variance explained by the PCs
    vars      <- apply(pca$x, 2, var)
    to.keep   <- which(vars >= 1) # Keep PCs with eigenvalues >= 1 (i.e., Kaiser criterion)
      
    # Save the PC scores and original PCA models
    PCdata[[speaker]]$scores  <- pca$x[,1:length(to.keep)]
    PCdata[[speaker]]$model   <- pca
  }
  return(PCdata)
}
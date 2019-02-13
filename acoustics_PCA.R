### PCA of acoustic features
feature_PCA <- function(my.data,features){
  PCdata <- c()
  
  # Separate PCA models are performed for each speaker
  for (speaker in unique(my.data$speaker)){
    thisdata  <- my.data[my.data$speaker==speaker,] # Get the speaker data
    
    # Perform a principal components analysis (PCA) on the speaker's acoustic features
    pca       <- prcomp(thisdata[ ,which(colnames(thisdata) %in% features)], scale=T, center=T)
    
    # Determine the variance explained by the PCs
    vars      <- apply(pca$x, 2, var)
    props     <- vars / sum(vars)
    var.exp   <- cumsum(props)
    to.keep <- which(vars > 1) # Keep PCs with eigenvalues > 1 (i.e., Kaiser criterion)
      
    # Save the PC scores and original PCA models
    PCdata[[speaker]]$scores  <- pca$x[,1:length(to.keep)]
    PCdata[[speaker]]$model   <- pca
  }
  return(PCdata)
}
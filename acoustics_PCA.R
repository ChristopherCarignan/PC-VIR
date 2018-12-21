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
    to.keep   <- var.exp[1:min(which(var.exp >= 0.8))] # Keep as many PCs as explain at least 80% of variance
    
    # Save the PC scores and original PCA models
    PCdata[[speaker]]$scores  <- pca$x[,1:length(to.keep)]
    PCdata[[speaker]]$model   <- pca
  }
  return(PCdata)
}
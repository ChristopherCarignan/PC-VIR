# Create a plot of the variables determined to be at least moderately important to the binary distinction

plot_imp_vars <- function(coeffs,features,adj.n=NULL){
  # Create blank plotting array
  plot.dat <- c()
  speakers <- names(PC.VIR.coeffs)
  
  # Bind all of the PC-VIR coefficients into a single vector
  plot.dat$dat <- as.vector(
    rbind(
      as.numeric(
        unlist(
          as.data.frame(coeffs)
        )
      )
    )
  )
  # Add names of features to plotting array
  plot.dat$var <- rep(rownames(coeffs)[1:length(features)],
                      times=nrow(coeffs)/length(features))
  # Add names of speakers to plotting array
  plot.dat$spk <- rep(speakers, each=length(features))
  plot.dat <- as.data.frame(plot.dat)
  colnames(plot.dat) <- c('dat','vars','spk')
  
  # Get the average values for the variables
  means   <- tapply(plot.dat$dat, plot.dat$vars, mean)
  # Labels for variables to go on x-axis of plot
  labels  <- c('F1\n amplitude','A1-P0','A1-P1','A1-P2',
               'F2\n amplitude', 'F3\n amplitude', 'A3-P0','Spectral\n COG',
               'F1\n frequency','F1\n bandwidth','F2\n frequency','F2\n bandwidth','F3\n frequency','F3\n bandwidth',
               'H1-H2','P0\n amplitude','P0\n prominence','P1\n amplitude','P1\n prominence','P2\n amplitude')
  order   <- sort(abs(means), decreasing=T) # Sort the variables by decreasing importance
  ord.seq <- match(names(order),names(means)) # Get the linear sequence of the sorting
  labels  <- labels[ord.seq] # Re-order the labels to match the sequence
  
  plot.dat$vars <- factor(plot.dat$vars, levels=names(order))
  
  
  # NB: the following code includes automatic thresholding of moderate and strong importance of the variables
  
  if (!is.null(adj.n)){
    z.thresh <- qnorm(.025/adj.n, lower.tail=F) # alpha adjustment
  }else{
    z.thresh <- 1.96 # no alpha adjustment
  }
  
  # Threshold for moderate importance = z-score * 0.5 (i.e., moderate linear relationship of PC scores)
  mod.thresh  <- z.thresh*0.5
  moderate    <- min(which(order < mod.thresh)) # Cutoff for moderate importance
  moderate    <- mean(c(moderate,moderate-1))
  
  # Threshold for strong importance = z-score * 0.7 (i.e., strong linear relationship of PC scores)
  str.thresh  <- z.thresh*0.7
  strong      <- min(which(order < str.thresh)) # Cutoff for strong importance
  strong      <- mean(c(strong,strong-1))

  
  # Create the plot
  p <- ggplot(plot.dat, aes(x=vars, y=dat, group=vars)) + 
    geom_hline(yintercept=0) + 
    # Lines for levels of moderate importance
    geom_hline(yintercept=mod.thresh, linetype=3, lwd=0.7) + 
    geom_hline(yintercept=-mod.thresh, linetype=3, lwd=0.7) + 
    # Lines for levels of strong importance
    geom_hline(yintercept=str.thresh, linetype=2, lwd=0.7) + 
    geom_hline(yintercept=-str.thresh, linetype=2, lwd=0.7) + 
    # This geom has to be added twice for some reason: once before and once after the colored fields
    geom_boxplot(notch=F,fill='white') + stat_summary(fun.y=mean, geom="point", size=4, pch=21, fill='lightgray') +
    # Distinguish variables of no importance
    geom_rect(data=NULL,aes(xmin=moderate,xmax=length(features)+1,ymin=-Inf,ymax=Inf),fill="red",alpha=0.002) + 
    # Distinguish variables of moderate importance
    geom_vline(xintercept=moderate, linetype=1, col='gray') +
    geom_rect(data=NULL,aes(xmin=strong,xmax=moderate,ymin=-Inf,ymax=Inf),fill="yellow",alpha=0.002) + 
    # Distinguish variables of strong importance
    geom_vline(xintercept=strong, linetype=1, col='gray') +
    geom_rect(data=NULL,aes(xmin=0,xmax=strong,ymin=-Inf,ymax=Inf),fill="green",alpha=0.002) + 
    # Add the boxplot geom again
    geom_boxplot(notch=F,fill='white') + stat_summary(fun.y=mean, geom="point", size=4, pch=21, fill='lightgray') + 
    # Set text for levels of importance
    annotate(geom="text", x=mean(c(0,strong)), y=max(plot.dat$dat), label="strong", size=6) +
    annotate(geom="text", x=mean(c(strong,moderate)), y=max(plot.dat$dat), label="moderate", size=6) +
    annotate(geom="text", x=mean(c(moderate,length(features)+1)), y=max(plot.dat$dat), label="no importance", size=6) +
    scale_x_discrete(name='', labels=labels) + theme_classic() + 
    theme(axis.text.x=element_text(size=12, angle=45, vjust=0.75),axis.title=element_text(size=16),axis.text=element_text(size=12)) + 
    ylab('Coefficient of contribution to nasality') + xlab('')
  
  return(p)
}
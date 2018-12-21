# Create a plot of the variables determined to be at least moderately important to the binary distinction

plot_imp_vars <- function(coeffs,features){
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
  
  # Threshold for moderate importance = 0.98 (1.96 * 0.5)
  # 1.96 = level of z-statistic significance
  # 0.5 = moderate linear relationship of PC scores
  
  # Threshold for moderate importance = 1.372 (1.96 * 0.7)
  # 1.96 = level of z-statistic significance
  # 0.7 = strong linear relationship of PC scores
  
  cutoff <- min(which(order < 0.98)) # Cutoff for moderate importance
  cutoff <- mean(c(cutoff,cutoff-1))
  
  # Create the plot
  p <- ggplot(plot.dat, aes(x=vars, y=dat, group=vars)) + 
    geom_hline(yintercept=0) + 
    # Lines for levels of moderate importance
    geom_hline(yintercept=0.98, linetype=3, lwd=0.7) + 
    geom_hline(yintercept=-0.98, linetype=3, lwd=0.7) + 
    # Lines for levels of strong importance
    geom_hline(yintercept=1.372, linetype=2, lwd=0.7) + 
    geom_hline(yintercept=-1.372, linetype=2, lwd=0.7) + 
    # This geom has to be added twice for some reason: once before and once after the colored fields
    geom_boxplot(notch=F,fill='white') + stat_summary(fun.y=mean, geom="point", size=4, pch=21, fill='lightgray') +
    # Distinguish variables of importance
    geom_vline(xintercept=cutoff, linetype=1, col='gray') +
    geom_rect(data=NULL,aes(xmin=0,xmax=cutoff,ymin=-Inf,ymax=Inf),fill="green",alpha=0.002) + 
    geom_rect(data=NULL,aes(xmin=cutoff,xmax=length(features)+1,ymin=-Inf,ymax=Inf),fill="red",alpha=0.002) + 
    # Add the boxplot geom again
    geom_boxplot(notch=F,fill='white') + stat_summary(fun.y=mean, geom="point", size=4, pch=21, fill='lightgray') + 
    scale_x_discrete(name='', labels=labels) + theme_classic() + 
    theme(axis.text.x=element_text(size=12, angle=45, vjust=0.75),axis.title=element_text(size=16),axis.text=element_text(size=12)) + 
    ylab('Coefficient of contribution to nasality') + xlab('')
  
  return(p)
}
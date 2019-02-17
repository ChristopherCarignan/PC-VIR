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
  labels  <- c('F1\namplitude','A1-P0','A1-P1','A1-P2',
               'F2\namplitude', 'F3\namplitude', 'A3-P0','Spectral\nCOG',
               'F1\nfrequency','F1\nbandwidth','F2\nfrequency','F2\nbandwidth','F3\nfrequency','F3\nbandwidth',
               'H1-H2','P0\namplitude','P0\nprominence','P1\namplitude','P1\nprominence','P2\namplitude')
  order   <- sort(abs(means), decreasing=T) # Sort the variables by decreasing importance
  ord.seq <- match(names(order),names(means)) # Get the linear sequence of the sorting
  labels  <- labels[ord.seq] # Re-order the labels to match the sequence
  
  plot.dat$vars <- factor(plot.dat$vars, levels=names(order))
  
  # NB: the following code includes automatic thresholding of moderate and strong importance of the variables
  
  # Threshold for moderate importance = 0.98 (1.96 * 0.5)
  # 1.96 = level of z-statistic significance
  # 0.5 = moderate linear relationship of PC scores
  
  moderate <- min(which(order < 0.98)) # Cutoff for moderate importance
  moderate <- mean(c(moderate,moderate-1))
  
  # Threshold for strong importance = 1.372 (1.96 * 0.7)
  # 1.96 = level of z-statistic significance
  # 0.7 = strong linear relationship of PC scores
  
  strong <- min(which(order < 1.372)) # Cutoff for strong importance
  strong <- mean(c(strong,strong-1))
  
  
  # Create the plot
  p <- ggplot(plot.dat, aes(x=vars, y=dat, group=vars)) + 
    geom_hline(yintercept=0) + 
    # Lines for levels of moderate importance
    geom_hline(yintercept=0.98, linetype=3, lwd=0.7) + 
    geom_hline(yintercept=-0.98, linetype=3, lwd=0.7) + 
    # Lines for levels of strong importance
    geom_hline(yintercept=1.372, linetype=2, lwd=0.7) + 
    geom_hline(yintercept=-1.372, linetype=2, lwd=0.7) + 
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
    annotate(geom="text", x=mean(c(0,strong)), y=max(plot.dat$dat), label="strong") +
    annotate(geom="text", x=mean(c(strong,moderate)), y=max(plot.dat$dat), label="moderate") +
    annotate(geom="text", x=mean(c(moderate,length(features)+1)), y=max(plot.dat$dat), label="no import.") +
    scale_x_discrete(name='', labels=labels) + theme_classic() + 
    theme(axis.text.x=element_text(size=12, angle=60, vjust=0.7),
          axis.title=element_text(size=16),axis.text=element_text(size=12),
          plot.margin = margin(0.5, 0.5, -0.75, 0.5, "cm"),) + 
    ylab('Coefficient of contribution to nasality') + xlab('')
  
  return(p)
}
# Create a plot of the variables determined to be at least moderately important to the binary distinction

plot_imp_vars <- function(coeffs,features,adj.n){
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
  
  z.thresh <- 1.96 # no z-statistic adjustment
  
  # Adjustment of z-statistic for number of PCs retained
  # Maximally conservative method equivalent to Bonferroni adjustment for p-values
  z.thresh.adj <- qnorm(.025/adj.n, lower.tail=F)
  
  # Thresholds for moderate importance = z-score * 0.5 (i.e., moderate linear relationship of PC scores)
  mod.thresh  <- z.thresh*0.5
  moderate    <- min(which(order < mod.thresh))
  moderate    <- mean(c(moderate,moderate-1))
  
  mod.thresh.adj  <- z.thresh.adj*0.5
  moderate.adj    <- min(which(order < mod.thresh.adj))
  moderate.adj    <- mean(c(moderate.adj,moderate.adj-1))
  
  # Thresholds for strong importance = z-score * 0.7 (i.e., strong linear relationship of PC scores)
  str.thresh  <- z.thresh*0.7
  strong      <- min(which(order < str.thresh))
  strong      <- mean(c(strong,strong-1))
  
  str.thresh.adj  <- z.thresh.adj*0.7
  strong.adj      <- min(which(order < str.thresh.adj))
  strong.adj      <- mean(c(strong.adj,strong.adj-1))

  
  # Create the plot
  p <- ggplot(plot.dat, aes(x=vars, y=dat, group=vars)) + 
    # Distinguish variables of no importance
    geom_rect(data=NULL,aes(xmin=moderate,xmax=length(features)+1,ymin=0,ymax=Inf),fill="red",alpha=0.002) + 
    geom_rect(data=NULL,aes(xmin=moderate.adj,xmax=length(features)+1,ymin=-Inf,ymax=0),fill="red",alpha=0.002) + 
    # Distinguish variables of moderate importance
    geom_rect(data=NULL,aes(xmin=strong,xmax=moderate,ymin=0,ymax=Inf),fill="yellow",alpha=0.002) + 
    geom_rect(data=NULL,aes(xmin=strong.adj,xmax=moderate.adj,ymin=-Inf,ymax=0),fill="yellow",alpha=0.002) + 
    geom_segment(aes(x=moderate, y=0, xend=moderate, yend=Inf), linetype=1, col='gray') +
    geom_segment(aes(x=moderate.adj, y=0, xend=moderate.adj, yend=-Inf), linetype=1, col='gray') +
    # Distinguish variables of strong importance
    geom_rect(data=NULL,aes(xmin=0,xmax=strong,ymin=0,ymax=Inf),fill="green",alpha=0.002) + 
    geom_rect(data=NULL,aes(xmin=0,xmax=strong.adj,ymin=-Inf,ymax=0),fill="green",alpha=0.002) + 
    geom_segment(aes(x=strong, y=0, xend=strong, yend=Inf), linetype=1, col='gray') +
    geom_segment(aes(x=strong.adj, y=0, xend=strong.adj, yend=-Inf), linetype=1, col='gray') +
    # Zero line
    geom_hline(yintercept=0) + 
    # Lines for levels of moderate importance
    geom_hline(yintercept=mod.thresh, linetype=3, lwd=0.7) + 
    geom_hline(yintercept=-mod.thresh, linetype=3, lwd=0.7) + 
    geom_hline(yintercept=mod.thresh.adj, linetype=3, lwd=0.7) + 
    geom_hline(yintercept=-mod.thresh.adj, linetype=3, lwd=0.7) + 
    # Lines for levels of strong importance
    geom_hline(yintercept=str.thresh, linetype=2, lwd=0.7) + 
    geom_hline(yintercept=-str.thresh, linetype=2, lwd=0.7) + 
    geom_hline(yintercept=str.thresh.adj, linetype=2, lwd=0.7) + 
    geom_hline(yintercept=-str.thresh.adj, linetype=2, lwd=0.7) + 
    # Add the boxplots
    geom_boxplot(notch=F,fill='white') + stat_summary(fun.y=mean, geom="point", size=4, pch=21, fill='lightgray') + 
    scale_x_discrete(name='', labels=labels) + theme_classic() + 
    scale_y_continuous(sec.axis = sec_axis(~., name = "   Unadjusted          Adjusted", labels=NULL)) +
    theme(axis.text.x=element_text(size=11, angle=60, vjust=0.7),
          axis.title=element_text(size=16),axis.text=element_text(size=12),
          plot.margin=unit(c(0.5,0.5,-0.75,0.5),"cm")) + 
    ylab('Coefficient of contribution to nasality') + xlab('')
  
  return(p)
}
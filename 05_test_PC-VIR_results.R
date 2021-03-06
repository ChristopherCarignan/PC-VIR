# Filename: 04_plot_imp_vars.R
# Date: 2019-02-20
# Author: Christopher Carignan
# Associate investigator: Ander Egurtzegi
# Email: c.carignan@phonetik.uni-muenchen.de
# Institution: Institute of Phonetics and Speech Processing (IPS), Ludwig-Maximilians-Universität München, Munich, Germany
# Description:
#   Optional function called from within the script 00_PC-VIR_analysis.R
#   Tests results from principal components varible importance reconstruction (PC-VIR)
#   NB: the analyses performed in 00_PC-VIR_analysis.R must be completed before the testing performed in this script


library(plsRglm)
library(dplyr)
library(ResourceSelection)
library(lme4)
library(lmerTest)
library(ggpubr)

set.seed(123) # Select random seed for replicability 

all.dat <- train.data # Binary training data from oral-nasal_training.R

all.dat$ID <- as.numeric(as.factor(paste0(all.dat$tokennum, all.dat$phonenum, all.dat$speaker, all.dat$point))) # Unique ID by item
all.dat$nasality2  <- as.numeric(all.dat$nasality=='nasal') # Nasality as a non-factor number: 0, 1


# Compare PC-VIR results against multiple logistic mixed effects models
glmm.dat <- all.dat
glmm.dat$nasality <- relevel(glmm.dat$nasality, ref="oral")

# normalize acoustic variables for each speaker (z-score transformation)
for (speaker in unique(glmm.dat$speaker)){
  for (feature in features){
    glmm.dat[[feature]][glmm.dat$speaker==speaker] <- scale(glmm.dat[[feature]][glmm.dat$speaker==speaker], center=T, scale=T)
  }
}

glmm.results <- c()
for (feature in 1:length(features)){
  # formula for logistic mixed effects model
  # random slopes/intercepts by phone, random intercepts by speaker
  fmla <- as.formula(paste("nasality ~ ", features[feature], " + (1 + ", features[feature], "|phone) + (1|speaker)"))
  
  glmm <- glmer(fmla, data=glmm.dat, family='binomial') # create the model
  
  # log the model results
  glmm.results$var[feature]    <- features[feature]
  glmm.results$est.[feature]   <- coef(summary(glmm))[2]
  glmm.results$z.val[feature]  <- coef(summary(glmm))[6]
  glmm.results$p.val[feature]  <- coef(summary(glmm))[8]
}
glmm.results <- as.data.frame(glmm.results)
glmm.results[order(abs(glmm.results$p.val)),] # display the results, ordered by p-value

# Compare the results for PC-VIR and the multiple logistic mixed effects models
var.order <- glmm.results$var[order(abs(glmm.results$z.val))]

# Get the statistically relevant variables according to the GLMMs (Bonferroni corrected)
data1     <- glmm.results$z.val[match(var.order, glmm.results$var)]
thresh1   <- glmm.results$p.val[match(var.order, glmm.results$var)] <= 0.05/length(features)

# Get the statistically relevant variables according to the PC-VIR model (at least moderate importance)
data2     <- rowMeans(PC.VIR.coeffs)[match(var.order, rownames(PC.VIR.coeffs))]
thresh2   <- abs(data2[match(var.order, names(data2))]) >= 0.98

threshes  <- c()
threshes[thresh1 & !thresh2]  <- "GLMMs"    # variables identified as significant in the GLMMs (but not with PC-VIR)
threshes[!thresh1 & thresh2]  <- "PC-VIR"   # variables identified as significant with PC-VIR (but not in the GLMMs)
threshes[thresh1 & thresh2]   <- "both"     # variables identified as significant with both methods
threshes[!thresh1 & !thresh2] <- "neither"  # variables not identified as significant with either method

# combine data
comb.dat  <- as.data.frame(cbind(data1,data2))
comb.dat$Significance <- threshes

# create a scatter plot of results (with regression line and 95% confidence intervals)
sp <- ggscatter(comb.dat, x="data1", y="data2", shape="Significance", color="Significance", size=8,
                add="reg.line",  # Add regression line
                add.params=list(color="blue", fill="lightgray"),
                conf.int=T, # Add confidence interval
                
                xlab="z-statistic from logistic mixed effects models",
                ylab="PC-VIR coefficient (speaker average)"
)
# Add correlation coefficient
sp + stat_cor(method="pearson", size=6) + font("xlab", size=16) + font("ylab", size=16) + font("xy.text", size=14) + 
  font("legend.title", size=16) + font("legend.text", size=16) + 
  scale_shape_discrete(solid=F, breaks=c("PC-VIR","GLMMs","both","neither")) + 
  scale_color_discrete(breaks=c("PC-VIR","GLMMs","both","neither"))


## Data preparation for model validation
HL.tests <- c() # Will be used to store the results of the Hosmer-Lemeshow tests for model validation

# Training data for validation: 80%
# Testing data for validatin: 20%
test.dat   <- c()
for (speaker in unique(all.dat$speaker)){
  subdata     <- all.dat[all.dat$speaker==speaker,] # Get the speaker data
  this.samp   <- unique(as.data.frame(subdata$ID)) %>% sample_frac(0.2) # Get 20% of the speaker's data 
  test.dat    <- rbind(test.dat,subdata[subdata$ID %in% this.samp[,1],]) # Add to testing data
}
train.dat  <- all.dat[!(all.dat$ID %in% test.dat$ID),] # Allocate the remaining 80% to training data


# Build speaker-specific logistic models on 80% training data
mylogit <- c()
mod.dat <- c()
for (speaker in unique(train.dat$speaker)){
  speakerdat  <- train.dat[train.dat$speaker==speaker,] # Get the training data for the speaker
  PCs         <- PC.data[[speaker]]$scores # Get the PC scores for the speaker
  PCs         <- PCs[speakerdat$rownum,] # Only include the PC scores that correspond to the training data items
  
  mod.dat[[speaker]] <- cbind.data.frame(speakerdat[,'nasality'], PCs) # Combine PC scores and nasality factor
  colnames(mod.dat[[speaker]])[1] <- 'nasality'
  
  # Relevel the factor so that oral = 0, nasal = 1
  mod.dat[[speaker]]$nasality <- relevel(mod.dat[[speaker]]$nasality, ref="oral")
  
  # Run the logistic regression model
  mylogit[[speaker]] <- glm(nasality ~ ., data=mod.dat[[speaker]], family="binomial")
}


# Perform the PC-VIR analysis and predict nasal/oral probabilities for testing data set
HL.tests$PCVIR <- c()
for (speaker in unique(train.dat$speaker)){
  speakerdat  <- train.dat[train.dat$speaker==speaker,] # Get the training data for the speaker
  PCs         <- PC.data[[speaker]]$scores # Get the PC scores for the speaker
  PCs         <- PCs[speakerdat$rownum,] # Only include the PC scores that correspond to the training data items
  
  this.mod  <- mylogit[[speaker]] # Get the fitted logistic model for the speaker
  z.values  <- coef(summary(this.mod))[,"z value"] # Get the z-statistics for the PCs in the model
  p.values  <- coef(summary(this.mod))[,"Pr(>|z|)"] # Get the p-values for the PCs in the model
  pc.num    <- length(z.values) - 1 # How many PCs were there?
  
  # Preallocate an array for the PC-VIR coefficients
  coeffs <- data.frame(matrix(ncol=length(features), nrow=0))
  colnames(coeffs) <- features
  
  # Do the variable importance reconstruction
  for (pc in 1:pc.num){
    # Get the statistic from the logistic model for this PC
    this.z.val  <- z.values[pc+1]
    this.p.val  <- p.values[pc+1]
    
    # Type I error adjustment for number of PCs (Bonferroni correction)
    p.adj <- pc.num*this.p.val # adjust the p-value
    if (p.adj > 1){
      p.adj <- 1
    }
    this.z.val <- sign(this.z.val)*qnorm(1-p.adj/2)
    
    # Get the PC coefficients
    these.coeffs  <- t(as.data.frame(PC.data[[speaker]]$model$rotation[,pc]))
    
    # Multiply the PC coefficients by the z-statistic for the PC to reconstruct the variable importance
    coeffs <- rbind(coeffs, (these.coeffs * this.z.val)) 
  }
  coeffs <- as.data.frame(colSums(coeffs)) # Sum the weighted coefficients for the speaker
  
  # Select the important variables to keep (at least moderate importance)
  to.keep       <- abs(coeffs)>=0.98
  sig.coeffs    <- coeffs[to.keep]
  coeff.names   <- rownames(coeffs)[to.keep]
  
  # Perform a new PC analysis on all speaker data, for only the variables selected by the PC-VIR model
  pca <- prcomp(all.dat[all.dat$speaker==speaker, coeff.names])
  
  # Combine nasality factor and the PC scores for the binomial logistic regression
  pca.dat <- cbind(all.dat$nasality[all.dat$speaker==speaker], as.data.frame(pca$x))
  names(pca.dat)[1] <- 'nasality'
  pca.dat$nasality  <- relevel(pca.dat$nasality, ref="oral")
  
  # New PCA-based logistic regression on only the selected variables
  bin.mod <- glm(nasality ~ ., data=pca.dat, family="binomial")
  
  # Perform Hosmer-Lemeshow test for goodness of fit (model validation)
  HL.test <- hoslem.test(bin.mod$y, fitted(bin.mod))
  HL.tests$PCVIR <- rbind(HL.tests$PCVIR, HL.test$statistic)
  
  # Get the speaker data from the 20% test data set
  speakerdat  <- all.dat[all.dat$rownum %in% test.dat$rownum[test.dat$speaker==speaker],]
  # Get the PC scores corresponding with the test data set
  PC.speaker  <- pca$x[all.dat$rownum[all.dat$speaker==speaker] %in% test.dat$rownum[test.dat$speaker==speaker],]
  
  # Predict nasality probabilities and add them to the test data matrix
  test.dat$PCVIR.pred[test.dat$speaker==speaker]  <- predict(bin.mod, type="response", newdata=as.data.frame(PC.speaker))
}
# Convert the predicted response values to categorical labels
test.dat$PCVIR.pred2[test.dat$PCVIR.pred < 0.5]   <- "oral"
test.dat$PCVIR.pred2[test.dat$PCVIR.pred >= 0.5]  <- "nasal"
test.dat <- test.dat %>% mutate(PCVIR.acc = 1*(nasality == PCVIR.pred2))


## Model comparison/validation
# The results will now be compared against the same training and testing data sets, 
# using partial least squares logistic regression (PLS-LR) to train and predict values
HL.tests$PLSLR <- c()
for (speaker in unique(train.dat$speaker)){
  speak.dat <- train.dat[train.dat$speaker==speaker,] # Get the training data for the speaker
  
  plsa <- as.formula(paste("nasality2 ~ ", paste(features, collapse="+"))) # PLS-LR formula as string
  
  # Build the PLS-LR model: predicting nasality from acoustic features in training set
  pls.mod <- plsRglm(plsa, data=speak.dat, modele='pls-glm-logistic', verbose=F, pvals.expli=T)
  
  # The plsRglm() function provides the ability to output whether the invididual variables are significant
  # for each component retained in the model (0 = not significant, 1 = significant)
  # We will select only those variables that are significant for all of the retained components
  sig.facs <- rowSums(pls.mod$pvalstep)/ncol(pls.mod$pvalstep)
  pls.to.keep <- features[sig.facs>0]
  
  # Perform a new PC analysis on all speaker data, for only the variables selected by the PLS-LR model
  pca <- prcomp(all.dat[all.dat$speaker==speaker, pls.to.keep])
  
  # Combine nasality factor and the PC scores for the binomial logistic regression
  pca.dat <- cbind(all.dat$nasality[all.dat$speaker==speaker], as.data.frame(pca$x))
  names(pca.dat)[1] <- 'nasality'
  pca.dat$nasality  <- relevel(pca.dat$nasality, ref="oral")
  
  # New PCA-based logistic regression on only the selected variables
  bin.mod <- glm(nasality ~ ., data=pca.dat, family="binomial")
  
  # Perform Hosmer-Lemeshow test for goodness of fit (model validation)
  HL.test <- hoslem.test(bin.mod$y, fitted(bin.mod))
  HL.tests$PLSLR <- rbind(HL.tests$PLSLR, HL.test$statistic)
  
  # Get the speaker data from the 20% test data set
  speakerdat  <- all.dat[all.dat$rownum %in% test.dat$rownum[test.dat$speaker==speaker],]
  # Get the PC scores corresponding with the test data set
  PC.speaker  <- pca$x[all.dat$rownum[all.dat$speaker==speaker] %in% test.dat$rownum[test.dat$speaker==speaker],]
  
  # Predict nasality probabilities and add them to the test data matrix
  test.dat$PLSLR.pred[test.dat$speaker==speaker] <- predict(bin.mod, type="response", newdata=as.data.frame(PC.speaker))
}
# Convert the predicted response values to categorical labels
test.dat$PLSLR.pred2[test.dat$PLSLR.pred < 0.5]   <- "oral"
test.dat$PLSLR.pred2[test.dat$PLSLR.pred >= 0.5]  <- "nasal"
test.dat <- test.dat %>% mutate(PLSLR.acc = 1*(nasality == PLSLR.pred2))


## Model validation (goodness of fit)
# Here, we get the average Chi-squared values of the Hosmer-Lemeshow tests for each model.
# Then, we calculate p-values from the average Chi-squared value and the number of degrees of freedom.
# For the Hosmer-Lemeshow test, the null hypothesis holds that the model fits the data, 
# i.e., a p-value *greater than* 0.05 indicates that the logistic regression model fits the data well
dof <- HL.test$parameter

# First, the model based on the PC-VIR method:
pchisq(mean(HL.tests$PCVIR), df=dof)

# Second, the model based on the PLS-LR method:
pchisq(mean(HL.tests$PLSLR), df=dof)


## Model validation (accuracy of prediction)
mean(test.dat$PCVIR.pred2 == test.dat$nasality)
mean(test.dat$PLSLR.pred2 == test.dat$nasality)
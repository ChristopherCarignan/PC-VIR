# Test results from principal components varible importance reconstruction (PC-VIR)

library(plsRglm)
library(dplyr)
library(ResourceSelection)

set.seed(123) # Select random seed for replicability 

all.dat <- train.data # Binary training data from oral-nasal_training.R

all.dat$ID <- as.numeric(as.factor(paste0(all.dat$tokennum,all.dat$phonenum,all.dat$speaker))) # Unique ID by token
all.dat$nasality2  <- as.numeric(all.dat$nasality=='nasal') # Nasality as a non-factor number: 0, 1

HL.tests <- c() # Will be used to store the results of the Hosmer-Lemeshow test for model validation

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
  
  mod.dat[[speaker]] <- cbind.data.frame(speakerdat[,'nasality'],PCs) # Combine PC scores and nasality factor
  colnames(mod.dat[[speaker]])[1] <- 'nasality'
  
  # Relevel the factor so that oral = 0, nasal = 1
  mod.dat[[speaker]]$nasality <- relevel(mod.dat[[speaker]]$nasality, ref="oral")
  
  # Run the logistic regression model
  mylogit[[speaker]] <- glm(nasality ~ ., data = mod.dat[[speaker]], family = "binomial")
}


# Perform the PC-VIR analysis and predict nasal/oral probabilities for testing data set
HL.tests$PCVIR <- c()
for (speaker in unique(train.dat$speaker)){
  speakerdat  <- train.dat[train.dat$speaker==speaker,] # Get the training data for the speaker
  PCs         <- PC.data[[speaker]]$scores # Get the PC scores for the speaker
  PCs         <- PCs[speakerdat$rownum,] # Only include the PC scores that correspond to the training data items
  
  this.mod  <- mylogit[[speaker]] # Get the fitted logistic model for the speaker
  z.values  <- coef(summary(this.mod))[,"z value"] # Get the z-statistics for the PCs in the model
  pc.num    <- length(z.values) - 1 # How many PCs were there?
  
  # Preallocate an array for the PC-VIR coefficients
  coeffs <- data.frame(matrix(ncol = length(features), nrow = 0))
  colnames(coeffs) <- features
  
  # Do the variable importance reconstruction
  for (pc in 1:pc.num){
    this.z.val    <- z.values[pc+1] # Get the z-statistic from the logistic model for this PC
    these.coeffs  <- t(as.data.frame(PC.data[[speaker]]$model$rotation[,pc])) # Get the PC coefficients
    
    # Multiply the PC coefficients by the z-statistic for the PC to reconstruct the variable importance
    coeffs <- rbind(coeffs, (these.coeffs * this.z.val)) 
  }
  coeffs <- as.data.frame(colSums(coeffs)) # Sum the weighted coefficients for the speaker
  
  # Select the important variables to keep (at least moderate importance)
  to.keep       <- abs(coeffs)>=0.98
  sig.coeffs    <- coeffs[to.keep]
  coeff.names   <- rownames(coeffs)[to.keep]
  
  # Perform a new PC analysis on all speaker data, for only the variables selected by the PC-VIR model
  pca <- prcomp(all.dat[all.dat$speaker==speaker,coeff.names])

  # Combine nasality factor and the PC scores for the binomial logistic regression
  pca.dat <- cbind(all.dat$nasality[all.dat$speaker==speaker],as.data.frame(pca$x))
  names(pca.dat)[1] <- 'nasality'
  pca.dat$nasality  <- relevel(pca.dat$nasality, ref="oral")
  
  # New PCA-based logistic regression on only the selected variables
  bin.mod <- glm(nasality ~ ., data = pca.dat, family = "binomial")
  
  # Perform Hosmer-Lemeshow test for goodness of fit (model validation)
  HL.test <- hoslem.test(bin.mod$y, fitted(bin.mod))
  HL.tests$PCVIR <- rbind(HL.tests$PCVIR, HL.test$statistic)
  
  # Get the speaker data from the 20% test data set
  speakerdat  <- all.dat[all.dat$rownum %in% test.dat$rownum[test.dat$speaker==speaker],]
  # Get the PC scores corresponding with the test data set
  PC.speaker  <- pca$x[all.dat$rownum[all.dat$speaker==speaker] %in% test.dat$rownum[test.dat$speaker==speaker],]
  
  # Predict nasality probabilities and add them to the test data matrix
  test.dat$PCVIR.pred[test.dat$speaker==speaker] <- predict(bin.mod, type="response", newdata=as.data.frame(PC.speaker))
}
# Find the difference (i.e., error) between the predicted values and a perfect response (0,1)
test.dat$PCVIR.diff <- test.dat$PCVIR.pred - test.dat$nasality2


## Model comparison/validation
# The results will now be compared against the same training and testing data sets, 
# using partial least squares discriminant analysis (PLS-DA) to train and predict values
HL.tests$PLSDA <- c()
for (speaker in unique(train.dat$speaker)){
  speak.dat <- train.dat[train.dat$speaker==speaker,] # Get the training data for the speaker
  
  plsa <- as.formula(paste("nasality2 ~ ", paste(features, collapse= "+"))) # PLS-DA formula as string
  
  # Build the PLS-DA model: predicting nasality from acoustic features in training set
  pls.mod <- plsRglm(plsa, data=speak.dat, modele='pls-glm-logistic', verbose=F, pvals.expli=T)
  
  # The plsRglm() function provides the ability to output whether the invididual variables are significant
  # for each component retained in the model (0 = not significant, 1 = significant)
  # We will select only those variables that are significant for all of the retained components
  sig.facs <- rowSums(pls.mod$pvalstep)/ncol(pls.mod$pvalstep)
  pls.to.keep <- features[sig.facs==1]
  
  # Perform a new PC analysis on all speaker data, for only the variables selected by the PLS-DA model
  pca <- prcomp(all.dat[all.dat$speaker==speaker,pls.to.keep])
  
  # Combine nasality factor and the PC scores for the binomial logistic regression
  pca.dat <- cbind(all.dat$nasality[all.dat$speaker==speaker],as.data.frame(pca$x))
  names(pca.dat)[1] <- 'nasality'
  pca.dat$nasality  <- relevel(pca.dat$nasality, ref="oral")
  
  # New PCA-based logistic regression on only the selected variables
  bin.mod <- glm(nasality ~ ., data = pca.dat, family = "binomial")
  
  # Perform Hosmer-Lemeshow test for goodness of fit (model validation)
  HL.test <- hoslem.test(bin.mod$y, fitted(bin.mod))
  HL.tests$PLSDA <- rbind(HL.tests$PLSDA, HL.test$statistic)
  
  # Get the speaker data from the 20% test data set
  speakerdat  <- all.dat[all.dat$rownum %in% test.dat$rownum[test.dat$speaker==speaker],]
  # Get the PC scores corresponding with the test data set
  PC.speaker  <- pca$x[all.dat$rownum[all.dat$speaker==speaker] %in% test.dat$rownum[test.dat$speaker==speaker],]
  
  # Predict nasality probabilities and add them to the test data matrix
  test.dat$PLSDA.pred[test.dat$speaker==speaker] <- predict(bin.mod, type="response", newdata=as.data.frame(PC.speaker))
}
# Find the difference (i.e., error) between the predicted values and a perfect response (0,1)
test.dat$PLSDA.diff <- test.dat$PLSDA.pred - test.dat$nasality2


## Model validation (goodness of fit)
# Here, we get the average Chi-squared values of the Hosmer-Lemeshow tests for each model.
# Then, we calculate p-values from the average Chi-squared value and the number of degrees of freedom.
# For the Hosmer-Lemeshow test, the null hypothesis holds that the model fits the data, 
# i.e., a p-value less than 0.05 indicates that the logistic regression model fits the data well
dof <- HL.test$parameter

# First, the model based on the PC-VIR method:
pchisq(mean(HL.tests$PCVIR), df=dof)

# Second, the model based on the PLS-DA method:
pchisq(mean(HL.tests$PLSDA), df=dof)

# Conclusion: both methods result in models that fit the data well


## Model validation (accuracy of prediction)
# Compare the absolute error between the two methods
t.test(abs(test.dat$PCVIR.diff),abs(test.dat$PLSDA.diff))

# Conclusion: the PC-VIR method provides significantly more accurate predictions on new data, compared to the PLS-DA method
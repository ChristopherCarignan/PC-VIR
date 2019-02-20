# Perform binary logistic training on oral and nasalized context.test (in this case = vowel)

logistic_training <- function(ac.data, PC.data, context.0, context.1, context.test){
  # For the purposes of the research questions pursued with this data set:
  # context.0 = oral stop consonants
  # context.1 = nasal consonants
  # context.test = vowels
  
  # Training occurs on vowels that are not adjacent to /h/ or a word boundary, i.e., 
  # CVC, CVN, NVC, or NVN contexts only, where C cannot be /h/
  
  
  ## Take measurements at 10% of the vowel interval for NVC and CVC contexts
  
  # Get the NVC contexts
  # The C that is furthest from the time point does not have to be a stop (but does have to be oral)
  context.1.dat <- ac.data[ac.data$prev %in% context.1 & !(ac.data$post %in% context.1),]
  context.1.dat <- context.1.dat[context.1.dat$post!='h' & context.1.dat$post!='#',] # Exclude /h/ and # contexts
  context.1.dat <- context.1.dat[context.1.dat$phone %in% context.test,] # Extract the vowel data
  context.1.dat$nasality <- 'nasal'
  
  # Get the CVC contexts
  # The C that is furthest from the time point does not have to be a stop (but does have to be oral)
  context.0.dat <- ac.data[ac.data$prev %in% context.0 & !(ac.data$post %in% context.1),]
  context.0.dat <- context.0.dat[context.0.dat$post!='h' & context.0.dat$post!='#',] # Exclude /h/ and # contexts
  context.0.dat <- context.0.dat[context.0.dat$phone %in% context.test,] # Extract the vowel data
  context.0.dat$nasality <- 'oral'
  
  traindata1 <- rbind(context.1.dat, context.0.dat) # Combine the data for training
  traindata1 <- traindata1[traindata1$point=='10',] # Get the 10% time point
  
  
  ## Take measurements at 90% of the vowel interval for CVN and CVC contexts
  
  # Get the CVN contexts
  # The C that is furthest from the time point does not have to be a stop (but does have to be oral)
  context.1.dat <- ac.data[!(ac.data$prev %in% context.1) & ac.data$post %in% context.1,]
  context.1.dat <- context.1.dat[context.1.dat$prev!='h' & context.1.dat$prev!='#',] # Exclude /h/ and # contexts
  context.1.dat <- context.1.dat[context.1.dat$phone %in% context.test,] # Extract the vowel data
  context.1.dat$nasality <- 'nasal'
  
  # Get the CVC contexts
  # The C that is furthest from the time point does not have to be a stop (but does have to be oral)
  context.0.dat <- ac.data[!(ac.data$prev %in% context.1) & ac.data$post %in% context.0,]
  context.0.dat <- context.0.dat[context.0.dat$prev!='h' & context.0.dat$prev!='#',] # Exclude /h/ and # contexts
  context.0.dat <- context.0.dat[context.0.dat$phone %in% context.test,] # Extract the vowel data
  context.0.dat$nasality <- 'oral'
  
  traindata2 <- rbind(context.1.dat,context.0.dat)  # Combine the data for training
  traindata2 <- traindata2[traindata2$point=='90',] # Get the 90% time point
  
  
  ## Take measurements at 50% of the vowel interval for NVN and CVC contexts
  
  # Get the NVN contexts
  # The C that is furthest from the time point does not have to be a stop (but does have to be oral)
  context.1.dat <- ac.data[ac.data$post %in% context.1 & ac.data$prev %in% context.1,]
  context.1.dat <- context.1.dat[context.1.dat$phone %in% context.test,] # Extract the vowel data
  context.1.dat$nasality <- 'nasal'
  
  context.0.dat <- ac.data[ac.data$prev %in% context.0 & ac.data$post %in% context.0,]
  context.0.dat <- context.0.dat[context.0.dat$prev!='h' & context.0.dat$prev!='#' & 
                                   context.0.dat$post!='h' & context.0.dat$post!='#',] # Exclude /h/ and # contexts
  context.0.dat <- context.0.dat[context.0.dat$phone %in% context.test,] # Extract the vowel data
  context.0.dat$nasality <- 'oral'
  
  traindata3 <- rbind(context.1.dat,context.0.dat)  # Combine the data for training
  traindata3 <- traindata3[traindata3$point=='50',] # Get the 50% time point
  
  
  # Combine the three data sets for training
  traindata <- rbind(traindata1,traindata2,traindata3)
  traindata$speaker <- as.factor(traindata$speaker)
  traindata$nasality <- as.factor(traindata$nasality)
  
  # Logistical/binary regression
  mylogit <- c()
  mod.dat <- c()
  for (speaker in unique(traindata$speaker)){
    speakerdat  <- traindata[traindata$speaker==speaker,] # Get the training data for the speaker
    PCs         <- PC.data[[speaker]]$scores # Get the PC scores for the speaker
    PCs         <- PCs[speakerdat$rownum,] # Only include the PC scores that correspond to the training data items
    
    mod.dat[[speaker]] <- cbind.data.frame(speakerdat[,'nasality'],PCs) # Combine PC scores and nasality factor
    colnames(mod.dat[[speaker]])[1] <- 'nasality'
    
    # Relevel the factor so that oral = 0, nasal = 1
    mod.dat[[speaker]]$nasality <- relevel(mod.dat[[speaker]]$nasality, ref="oral")
    
    # Run the logistic regression model
    mylogit[[speaker]] <- glm(nasality ~ ., data = mod.dat[[speaker]], family = "binomial")
    
    # Use the logistic regression model to predict nasal/oral scores for all of the speaker data
    ac.data$pred[ac.data$speaker==speaker] <- predict(mylogit[[speaker]], 
                                                      newdata = as.data.frame(PC.data[[speaker]]$scores), 
                                                      type = "response")
  }
  return(list(ac.data, traindata, mylogit))
}
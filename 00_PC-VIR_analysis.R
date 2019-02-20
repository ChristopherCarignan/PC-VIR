### Principal Components Variable Importance Reconstruction (PC-VIR)

install.packages(c("plsRglm","dplyr","ResourceSelection","lme4","lmerTest","ggpubr","ggplot2"))

load('acoustic_data.Rda')

# Acoustic features of nasality from Styler (2017)
features  <- c('a1.amp','a2.amp','a3.amp',       # Formant harmonic amplitudes
               'p0.amp','a1p0','p0.prom',        # Nasal pole amplitudes
               'p1.amp','a1p1','p1.prom',        # P1 features
               'p2.amp','a1p2',                  # P2 features
               'f1','f2','f3',                   # Formant frequencies
               'f1.width','f2.width','f3.width', # Formant bandwidths
               'h1h2','cog','a3p0')              # Spectral features

# Run the function to perform speaker-wise PCA on the acoustic features
PC.data <- feature_PCA(ac.data, features)

# Lists of phones for the nasal-oral binary training
nasals  <- c('n','m','N','n_d','J')
orals   <- c('d','t','b','p','g','k')
vowels  <- c('a','e','i','o','u','y','j','w')

# Add row number tag for matching training data with PCA data
ac.data$rownum <- rownames(ac.data)

# Run the function to perform the binary training
bin.results <- logistic_training(ac.data, PC.data, orals, nasals, vowels)
ac.data     <- bin.results[[1]] # Acoustic data with oral/nasal prediction scores
train.data  <- bin.results[[2]] # Training data used
logit.mods  <- bin.results[[3]] # Speaker-specific binary models

# Run the function to reconstruct the important variables from the PCA-based regression models
PC.VIR.coeffs <- PC_VIR(PC.data, train.data, logit.mods, features)

# The function also has an optional adjustment for Type I error
PC.VIR.coeffs <- PC_VIR(PC.data, train.data, logit.mods, features, adjust = T)

# Plot the important variables
plot_imp_vars(PC.VIR.coeffs, features)

## Test and validate results using the following function: 
## test_PC-VIR_results.R
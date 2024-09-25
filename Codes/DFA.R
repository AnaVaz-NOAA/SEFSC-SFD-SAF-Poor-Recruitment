# Make correlations between indexes, EOF and recdev
# calculate spectral analyses
library(gplots)
library(RColorBrewer)
library(bcp)
library(gridExtra)
library(dplyr)
library(reshape2)
library(ggplot2)
library(wesanderson)

graphics.off()
# load the file with all covariates and rec devs
CovarSeason_RecDev <- read.csv("./csv_files/CovarSeason_RecDev.csv")

aux <- colnames(CovarSeason_RecDev)
covarNames <- aux[c(2:41)]
spNames <- aux[42:51]

yearsPlot <- 1993:2021
color_palette <- rainbow(length(unique(yearsPlot)))
coloraux = colorRampPalette(brewer.pal(11, "Spectral"))
color_palette = coloraux(length(unique(yearsPlot)))

pcut <- 0.05
# number of covariates 
itot = 5*4 + 3*4 + 2*4 + 1
gam_models <- list()

gam_aic_matrix <- matrix(NA, nrow = itot-1, ncol = 10, 
                         dimnames = list(covarNames, spNames))
yearsPlot <- unique(CovarSeason_RecDev[,1])
pal <- wes_palette("Zissou1", 30, type = "continuous")

# Create directory to save plots
dirName <- paste0("Spearman_Plots_Species")
dir.create(dirName, showWarnings = FALSE)

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames = list(covarNames, spNames))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames = list(covarNames, spNames))

# Define a function to fit DFA and extract results
fit_dfa <- function(data, n_factors) {
  # Ensure the data is in matrix form
  data_matrix <- as.matrix(data)
  
  # Fit DFA model
  model <- dynamac(data_matrix, n_factors = n_factors)
  
  # Return factors and loadings
  return(list(factors = model$factors, loadings = model$loadings))
}

# Initialize storage for DFA results
dfa_results <- list()

# Loop over species and covariates
for (iSpp in 1:10) {
  species_name  <- spNames[iSpp]
  spNamePlot <- gsub(" ", "_", species_name)
  
  # Create a list to store results for each species
  dfa_results[[species_name]] <- list()
  
  for (iCovar in 2:itot) {
    covarNamePlot <- covarNames[iCovar - 1]
    
    # Prepare data for DFA
    x <- CovarSeason_RecDev[, iCovar, drop = FALSE]  # Ensure x is a data frame/matrix
    y <- CovarSeason_RecDev[, iSpp + itot, drop = FALSE]  # Ensure y is a data frame/matrix
    
    # Combine x and y into a single data frame/matrix for DFA
    combined_data <- cbind(x, y)
    
    # Fit DFA model
    dfa_result <- fit_dfa(combined_data, n_factors = 2)  # Adjust n_factors as needed
    
    # Save results
    dfa_results[[iSpp]][[ICovar]] <- dfa_result
  }
}

# Example: Access and print DFA results for a specific species
for (species in names(dfa_results)) {
  cat("Species:", species, "\n")
  for (covar in names(dfa_results[[species]])) {
    cat("Covariate:", covar, "\n")
    cat("Factors:\n")
    print(dfa_results[[species]][[covar]]$factors)
    cat("Loadings:\n")
    print(dfa_results[[species]][[covar]]$loadings)
  }
}

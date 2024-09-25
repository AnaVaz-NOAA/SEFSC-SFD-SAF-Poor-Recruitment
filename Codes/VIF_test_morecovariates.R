# Load necessary packages
library(mgcv)
library(car)  # For the vif function
library(wesanderson)
library(ggplot2)
library(dplyr)

# Load the data
CovarSeason_RecDev <- read.csv("./csv_files/CovarSeason_RecDevsmall_v2.csv")

aux <- colnames(CovarSeason_RecDev)
covarNames <- aux[c(2:11)]
spNames <- aux[12:21]

# Filter rows where all covariates are complete
columns_to_check <- c('X', colnames(CovarSeason_RecDev)[2:11])  
filtered_data <- CovarSeason_RecDev %>%
  filter(complete.cases(select(., all_of(columns_to_check))))

CovarSeason_RecDev <- filtered_data

# Create a directory to save plots
dirName <- "GAM_Plots_Species_VIF_v2"
dir.create(dirName, showWarnings = FALSE)

# check for colinearity between variables
iSpp <- 1
species_name  <- spNames[iSpp]
spNamePlot <- gsub(" ", "_", species_name)

# Loop over species to fit models and create plots
for (iSpp in 1:9) {
  species_name  <- spNames[iSpp]
  spNamePlot <- gsub(" ", "_", species_name)
  
  # Prepare data for the current species
  y <- CovarSeason_RecDev[, iSpp + 11]
  data_combined <- data.frame(CovarSeason_RecDev[, 2:11], y = y)
  data_combined <- na.omit(data_combined)
  
  # Define the response variable and predictor variables
  response_var <- "y"
  predictor_vars <- colnames(data_combined)[-ncol(data_combined)]
  high_vif <- TRUE
  iteration <- 1
  
  while (high_vif && length(predictor_vars) > 1) {
    # Filter out predictors with only one unique value
    valid_vars <- predictor_vars[sapply(data_combined[predictor_vars], function(col) length(unique(col)) > 1)]
    
    if (length(valid_vars) < length(predictor_vars)) {
      removed_vars <- setdiff(predictor_vars, valid_vars)
      cat("Removed predictors with only one unique value:", paste(removed_vars, collapse = ", "), "\n") }
    
    predictor_vars <- valid_vars
    if (length(predictor_vars) == 0) break
    
    formula <- as.formula(paste(response_var, "~", paste(predictor_vars, collapse = " + ")))
    lm_model <- lm(formula, data = data_combined)
    vif_values <- vif(lm_model)
    
    # Save the model and VIF values for the current iteration
    save(gam_model, vif_values, file = paste0(dirName, "/", spNamePlot, "_iteration_", iteration, ".RData"))
    iteration <- iteration + 1
    
    # Check if any VIFs are above the threshold
    max_vif <- max(vif_values, na.rm = TRUE)
    if (max_vif > 2) {  # You can adjust the threshold as needed
      # Remove the predictor with the highest VIF
      high_vif_var <- names(which.max(vif_values))
      predictor_vars <- setdiff(predictor_vars, high_vif_var)
      cat("Removing", high_vif_var, "with VIF =", max_vif, "\n")
    } else {
      high_vif <- FALSE
    } }
  # Final GAM model and remaining predictors
  if (length(predictor_vars) == 0) {
    cat("No valid predictors left for", species_name, "\n")
    next }
  
  final_formula <- as.formula(paste(response_var, "~ s(", paste(predictor_vars, collapse = ") + s("),")"))
  final_model <- gam(final_formula, data = data_combined, method = "REML")
  
  # Save the final model and remaining predictors
  save(final_model, predictor_vars, file = paste0(dirName, "/", spNamePlot, "_final_model.RData"))
  
  # Save summary
  summary_text <- capture.output(summary(final_model))
    file_path <- paste0(dirName, "/", "Gam_model_summary", spNamePlot, ".txt")
  writeLines(summary_text, file_path)
  
  png(filename = paste0(dirName, "/","GAM_Check_", spNamePlot, ".png"),
      width = 1000, height = 1000, res = 200)
  par(mfrow = c(2, 2))
  gam.check(final_model)
  dev.off()
  
  png(filename = paste0(dirName, "/","GAM_plot_", spNamePlot, ".png"),
      width = 1000, height = 1000, res = 200) 
  plot(final_model, pages = 1, all.terms = TRUE, rug = TRUE, residuals = TRUE, pch = 20, cex = 1, col = "#FC4E07",shade = TRUE, shade.col = "#00AFBB")
  dev.off()
}



# check for alias to reduce the file with ALL covariates
# y <- CovarSeason_RecDev[, iSpp + 41]
# data_combined <- data.frame(CovarSeason_RecDev[, 2:41], y = y)
# data_combined <- na.omit(data_combined)
# lm_model <- lm(formula, data = data_combined)
# alias_info <- alias(lm_model)
# write.csv(alias_info$Complete, file = "alias_infosm.csv")

# Example of doing by hand:
lm_model <- lm(y ~ Bottom.Temp.Summer + Bottom.Temp.Winter + 
                   SSH.Summer + SSH.Winter + SST.Summer + SST.Winter + ChlModis50.Fall + 
                   ChlModis50.Spring + ChlModis50.Summer + ChlModis50.Winter + 
                   deepGulf.Summer + deepGulf.Winter, data = data_combined)
# Calculate VIFs
vif_values <- vif(lm_model)

lm_model <- lm(y ~ Bottom.Temp.Summer +
                 SSH.Summer + SSH.Winter + SST.Summer + SST.Winter + ChlModis50.Fall + 
                 ChlModis50.Spring + ChlModis50.Summer + ChlModis50.Winter + 
                 deepGulf.Summer + deepGulf.Winter, data = data_combined)
# Calculate VIFs
vif_values <- vif(lm_model) 

lm_model <- lm(y ~ SSH.Summer + SSH.Winter + SST.Summer + SST.Winter + ChlModis50.Fall + 
                 ChlModis50.Spring + ChlModis50.Summer + ChlModis50.Winter + 
                 deepGulf.Summer + deepGulf.Winter, data = data_combined)
# Calculate VIFs
vif_values <- vif(lm_model) 

lm_model <- lm(y ~ SSH.Summer + SSH.Winter + SST.Summer + SST.Winter + ChlModis50.Fall + 
                 ChlModis50.Summer + ChlModis50.Winter + 
                 deepGulf.Summer + deepGulf.Winter, data = data_combined)
# Calculate VIFs
vif_values <- vif(lm_model) 

gam_model <- gam(y ~ SSH.Summer + SSH.Winter + SST.Summer + SST.Winter + ChlModis50.Fall + 
                   ChlModis50.Summer + ChlModis50.Winter + 
                   deepGulf.Summer + deepGulf.Winter, data = data_combined)
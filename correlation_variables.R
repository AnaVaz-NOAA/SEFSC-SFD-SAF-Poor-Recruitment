library(ggplot2)
library(dplyr)
library(tidyr)

filepath <- ("./data/CNAPS_Shelf_15/")

# get lat and lon
filename <- (gsub(" ","",paste(filepath,"latlon_CNAPS_Shelf.mat")))
latlon   <- readMat(filename)
latlon   <- latlon$latlon
plotyear <- seq(1993,2021)

# load matfile
for (ivar in 1:4) {
  varname <- switch(ivar,
                    'ssh',
                    'bottomT',
                    'sst',
                    'mixedlayer')
  vartitle <- switch(ivar,
                     'SSH',
                     'Bottom Temperature',
                     'SST',
                     'Mixed Layer')
  # work the variable
  # loop all sites and do 1) envPred, 2) spectral analyses for entire time (breakdown in frequencies), 3) trend
  filename <- (gsub(" ","",paste(filepath,"seasonal_avg_",varname,"_CNAPS_Shelf.mat")))
  VarMaux  <- readMat(filename)
  VarM     <- VarMaux$seasonal.avg
  rm(VarMaux)
  
  all_avg_anomalies <- data.frame(Latitude = unique(latlon[,1]))
  
  for (iseason in 1:4){
    seasonname <- switch(iseason,
                         'Winter',
                         'Spring',
                         'Summer',
                         'Fall')
    varSeason <- VarM[ , ,iseason]
    varSeason <- t(varSeason)
    varSeason_df <- as.data.frame(varSeason)
    varSeason_df$Lat <- latlon[, 1]
    varSeason_df$Lon <- latlon[, 2]
    
    # Combine latlon information with VarM
    average_data <- varSeason_df %>%
      group_by(Lat) %>%
      summarize_all(mean, na.rm = TRUE)
    melted_data <- reshape2::melt(average_data, id.vars = c("Lat", "Lon"))
    # Replace 'V' with empty string and convert to numeric
    melted_data$Year <- as.numeric(sub("V", "", melted_data$variable))
    
    # Replace 'variable' with corresponding years from 'plotyear'
    melted_data$variable <- as.character(plotyear[melted_data$Year])
    global_mean <- mean(melted_data$value, na.rm = TRUE)
    # Create the anomaly variable
    melted_data$anomaly <- melted_data$value - global_mean
    minP <- min(melted_data$anomaly)
    maxP <- max(melted_data$anomaly)
    sizeP <- (maxP - minP)/10
    
    plot <- ggplot(data = melted_data, aes(x = variable, y = Lat, fill = anomaly)) +
      geom_tile() +
      scale_fill_stepsn(colors = c("#08306B", "white", "#67000D"),
                        limits = c(minP, maxP),
                        values = rescale(c(minP, 0, maxP)),
                        n.breaks = 20) +
      coord_cartesian(expand = FALSE) +
      scale_x_discrete() +
      labs(title = glue("Anomaly {vartitle} ({min(melted_data$variable)}-{max(melted_data$variable)})")) +
      theme_light() 
    ggsave(gsub(" ","",paste("gg_",varname,"_",seasonname,"_15.png")), plot, width = 10, height = 5, dpi = 300)
    
    # Calculate the average anomaly per latitude and standard deviation
    avg_anomaly <- melted_data %>%
      group_by(Lat) %>%
      summarise(avg_anomaly = mean(anomaly),
                sd_anomaly = sd(anomaly))
    
    # Plot: Average Anomaly per Latitude
    avg_plot <- ggplot(avg_anomaly, aes(x = avg_anomaly, y = Lat)) +
      geom_point(color = "dodgerblue4") +
      geom_errorbarh(aes(xmin = avg_anomaly - sd_anomaly, xmax = avg_anomaly + sd_anomaly), height = 0) +
      labs(x = "Average Anomaly", y = "Latitude", title = paste("Average Anomaly per Latitude")) +
      theme_light()
    ggsave(gsub(" ","",paste("avg_Lat_",varname,"_",seasonname,"_15.png")), avg_plot, width = 5, height = 10, dpi = 300)
    
    all_avg_anomalies[[paste0(seasonname)]] <- avg_anomaly$avg_anomaly
  } 
  write.csv(all_avg_anomalies, file = gsub(" ","",paste("shelf_",varname,"_anomaly_15.csv")), row.names = FALSE)
}

variables <- c("sst", "ssh", "bottomT", "mixedlayer", "GlbColour", "modis", "UpSurf", "UpSub")
seasons <- c("Winter","Spring","Summer", "Fall")

auxdf <- list()
pcut <- 0.01

for (season in seasons){
  # Iterate over each variable
  # create new correlation for each season
  corr_matrix <- matrix(NA, nrow=8, ncol=8, dimnames=list(variables, variables))
  p_matrix    <- matrix(NA, nrow=8, ncol=8, dimnames=list(variables, variables))
  
  for (variable in variables) {
    # Read the data
    filename <- paste0(variable, "_anomaly_15.csv")
    df <- read.csv(filename, stringsAsFactors = FALSE)
    
    # If GlbColour, modis, UpSurf, and UpSub needs to interp lat
    if (variable %in% c("GlbColour", "modis", "UpSurf", "UpSub")) {
      valueadd <- approx(df$Latitude, df[[season]], xout = sstLat)
    } else {
      # dont need to interpolate as are in the right in the same lat
      valueadd <- df[[season]] 
    }
    # new def to the newdf
    auxdf[[variable]] <- valueadd
  }
  newdf <- as.data.frame(auxdf)
  # now loop to correlate
  for (season1 in 1:8) {
    for (season2 in 1:8) {
      # correlation and save coef. and p
      x <- newdf[,season1]
      y <- newdf[,season2]
      
      # correlation and save coef. and p
      aux <- cor.test(x, y, use = "complete.obs")
      corr_matrix[season1,season2] <- aux$estimate
      p_matrix[season1,season2] <- aux$p.value
    } # end one variable 
  }  # end second variable
  
  plot_matrix <- corr_matrix
  plot_matrix[p_matrix > pcut] <- NaN
  # plot the correlation
  colorPlot <- brewer.pal(9, "RdBu")
  
  melted_cor_matrix <- melt(plot_matrix)
  plot <- ggplot(melted_cor_matrix, 
                 aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white", size = 0.5) +  # Add white grid lines
    scale_fill_gradient2(low = "#3B9AB2", 
                         mid = "white", 
                         high = "#F21A00", 
                         na.value = "white", midpoint = 0) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.background = element_rect(fill = "white"),  
      # Set the background color to white
      plot.background = element_rect(fill = "white"), 
      # Set the plot background to white
      panel.grid.major = element_line(color = "lightgray", 
                                      size = 0.5),  
      # Add light gray grid lines
      panel.grid.minor = element_line(color = "lightgray", 
                                      size = 0.5) 
      # Remove minor grid lines
    )
  ggsave(gsub(" ","", paste("Correlation_latitudes_",season,"_15.png")),
         plot, dpi = 300, width = 12, height = 12)
} # end seasons


#------------------------------------------------------------------
# FOR 50 - 300
#------------------------------------------------------------------

variables <- c("sst", "ssh", "bottomT", "mixedlayer", "GlbColour", "modis","UpSurf","UpSub")
seasons <- c("Summer", "Spring", "Fall", "Winter")

variable_data <- list()

# Iterate over each variable
for (variable in variables) {
  # Read the data for the current variable
  filename <- paste0(variable, "_anomaly_50.csv")
  df <- read.csv(filename, stringsAsFactors = FALSE)
  df_long <- pivot_longer(df, cols = -Latitude, names_to = "Season", values_to = "Value")
  df_long$Variable <- variable
  variable_data[[variable]] <- df_long
}

# Combine data frames for all variables
combined_data <- bind_rows(variable_data)

# Plot variables for each season
plot <- ggplot(combined_data, aes(x = Latitude, y = Value, color = Variable)) +
  geom_line() +
  facet_wrap(~ Season, scales = "free") +
  labs(title = "Variables by Season", x = "Latitude", y = "Value", color = "Variable") +
  theme_light()
print(plot)
ggsave("variables_by_season_50.png", plot, width = 12, height = 8, dpi = 300)

max_per_variable <- combined_data %>%
  group_by(Variable) %>%
  summarise(max_value = max(Value, na.rm = TRUE))

# Merge the maximum values back to the combined data
combined_data <- left_join(combined_data, max_per_variable, by = "Variable")

# Normalize the values by dividing each value by its corresponding maximum value
combined_data$Normalized_Value <- combined_data$Value / combined_data$max_value

# Plot variables for each season with normalized values
plot <- ggplot(combined_data, aes(x = Latitude, y = Normalized_Value, color = Variable)) +
  geom_line() +
  facet_wrap(~ Season, scales = "free") +
  labs(title = "Variables by Season (Normalized)", x = "Latitude", y = "Normalized Value", color = "Variable") +
  theme_light()
ggsave("variables_by_season_normalized_50.png", plot, width = 12, height = 8, dpi = 300)

variables <- c("sst", "ssh", "bottomT", "mixedlayer", "GlbColour", "modis", "UpSurf", "UpSub")
seasons <- c("Winter","Spring","Summer", "Fall")

# save the latitude to interpolate
filename <- paste0("sst_anomaly_50.csv")
df <- read.csv(filename, stringsAsFactors = FALSE)
sstLat <- df$Latitude

auxdf <- list()
pcut <- 0.01

for (season in seasons){
  # Iterate over each variable
  # create new correlation for each season
  corr_matrix <- matrix(NA, nrow=8, ncol=8, dimnames=list(variables, variables))
  p_matrix    <- matrix(NA, nrow=8, ncol=8, dimnames=list(variables, variables))
  
  for (variable in variables) {
    # Read the data
    filename <- paste0(variable, "_anomaly_50.csv")
    df <- read.csv(filename, stringsAsFactors = FALSE)
    
    # If GlbColour, modis, UpSurf, and UpSub needs to interp lat
    if (variable %in% c("GlbColour", "modis", "UpSurf", "UpSub")) {
      valueadd <- approx(df$Latitude, df[[season]], xout = sstLat)
      original_data <- data.frame(Latitude = df$Latitude, Value = df[[season]], Type = "Original")
      interpolated_data <- data.frame(Latitude = sstLat, Value = valueadd, Type = "Interpolated")
      # Plot
      plot <- ggplot() +
        geom_point(data = original_data, aes(x = Latitude, y = Value), color = "dodgerblue3") +
        geom_line(data = interpolated_data, aes(x = Latitude, y = Value.y), color = "tomato3") +
        labs(title = "Original (blue) vs Interpolated (red)", x = "Latitude", y = "Value")
      ggsave(gsub(" ","", paste("check_interp_",season,"_",variable,".png")),
             plot, dpi = 300, width = 12, height = 12)
    } else {
      # dont need to interpolate as are in the right in the same lat
      valueadd <- df[[season]] 
    }
    # new def to the newdf
    auxdf[[variable]] <- valueadd
  }
  newdf <- as.data.frame(auxdf)
  # now loop to correlate
  for (season1 in 1:8) {
    for (season2 in 1:8) {
      # correlation and save coef. and p
      x <- newdf[,season1]
      y <- newdf[,season2]
      
      # correlation and save coef. and p
      aux <- cor.test(x, y, use = "complete.obs")
      corr_matrix[season1,season2] <- aux$estimate
      p_matrix[season1,season2] <- aux$p.value
    } # end one variable 
  }  # end second variable
  
  plot_matrix <- corr_matrix
  plot_matrix[p_matrix > pcut] <- NaN
  # plot the correlation
  colorPlot <- brewer.pal(9, "RdBu")
  
  melted_cor_matrix <- melt(plot_matrix)
  plot <- ggplot(melted_cor_matrix, 
                 aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white", size = 0.5) +  # Add white grid lines
    scale_fill_gradient2(low = "#3B9AB2", 
                         mid = "white", 
                         high = "#F21A00", 
                         na.value = "white", midpoint = 0) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.background = element_rect(fill = "white"),  
      # Set the background color to white
      plot.background = element_rect(fill = "white"), 
      # Set the plot background to white
      panel.grid.major = element_line(color = "lightgray", 
                                      size = 0.5),  
      # Add light gray grid lines
      panel.grid.minor = element_line(color = "lightgray", 
                                      size = 0.5) 
      # Remove minor grid lines
    )
  ggsave(gsub(" ","", paste("Correlation_latitudes_",season,"_50.png")),
         plot, dpi = 300, width = 12, height = 12)
} # end seasons

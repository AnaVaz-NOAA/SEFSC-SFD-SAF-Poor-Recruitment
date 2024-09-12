#library(envPred)
# devtools::install_github("beauchamplab/raveio")
library(Hmisc)
library(tidyr)
library(gplots)
library(ggplot2)
library(RColorBrewer)
library(cmocean)
library(dplyr)
library(plotly)
library(R.matlab)
library(reticulate)
library(glue)

# for a readmat function that works with HDF5
#library(raveio)

filepath <- ("~/Stuff/Current/SAtlantic/data/CNAPS_Shelf_15/")

# get lat and lon
filename <- (gsub(" ","",paste(filepath,"latlon_CNAPS_Shelf.mat")))
latlon   <- readMat(filename)
latlon   <- latlon$latlon
plotyear <- seq(1993,2021)

#another way of plotting anomalies
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
  
  all_avg_anomalies <- list()  # Initialize a list to store average anomalies for each season
  
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
    
    # Combine latlon information with VarM and calculate the average by latitude
    average_data <- varSeason_df %>%
      group_by(Lat) %>%
      summarize_all(mean, na.rm = TRUE)
    
    # Reshape data
    melted_data <- reshape2::melt(average_data, id.vars = c("Lat", "Lon"))
    
    # Extract year information
    melted_data$Year <- as.numeric(sub("V", "", melted_data$variable))
    
    # Replace 'variable' with corresponding years from 'plotyear'
    melted_data$variable <- as.character(plotyear[melted_data$Year])
    
    # Calculate the mean value per latitude
    lat_mean <- melted_data %>%
      group_by(Lat) %>%
      summarize(lat_mean_value = mean(value, na.rm = TRUE))
    
    # Merge lat_mean with melted_data
    melted_data <- merge(melted_data, lat_mean, by = "Lat")
    
    # Create the anomaly variable by latitude
    melted_data$anomaly <- melted_data$value - melted_data$lat_mean_value
    
    # Define plot range
    minP <- min(melted_data$anomaly)
    maxP <- max(melted_data$anomaly)
    sizeP <- (maxP - minP) / 10
    
    # Plot Anomalies
    plot <- ggplot(data = melted_data, aes(x = variable, y = Lat, fill = anomaly)) +
      geom_tile() +
      scale_fill_stepsn(colors = c("#08306B", "white", "#67000D"),
                        limits = c(minP, maxP),
                        values = scales::rescale(c(minP, 0, maxP)),
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
    
    # Store the average anomalies for each season
    all_avg_anomalies[[paste0(seasonname)]] <- avg_anomaly$avg_anomaly
  }
  
  # Save all average anomalies to a CSV file
  write.csv(all_avg_anomalies, file = gsub(" ","",paste(varname,"_anomaly_15.csv")), row.names = FALSE)
  
}


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
  write.csv(all_avg_anomalies, file = gsub(" ","",paste(varname,"_anomaly_15.csv")), row.names = FALSE)
}

# for a readmat function that works with HDF5
#library(raveio)
filepath <- ("./data/CNAPS_Shelf_50/")

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
    ggsave(gsub(" ","",paste("gg_",varname,"_",seasonname,"_50.png")), plot, width = 10, height = 5, dpi = 300)

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
    ggsave(gsub(" ","",paste("avg_Lat_",varname,"_",seasonname,"_50.png")), avg_plot, width = 5, height = 10, dpi = 300)
    
    avg_plot <- ggplot(avg_anomaly, aes(x = avg_anomaly, y = Lat)) +
      geom_point(color = "dodgerblue4") +
      geom_ribbon(aes(xmin = avg_anomaly - sd_anomaly, xmax = avg_anomaly + sd_anomaly), alpha = 0.3) +
      labs(x = "Average Anomaly", y = "Latitude", title = paste("Average Anomaly per Latitude")) +
      theme_light()
    ggsave(gsub(" ","",paste("avg_Lat_",varname,"_",seasonname,"_50_shade.png")), avg_plot, width = 5, height = 10, dpi = 300)

    all_avg_anomalies[[paste0(seasonname)]] <- avg_anomaly$avg_anomaly
  } 
  write.csv(all_avg_anomalies, file = gsub(" ","",paste(varname,"_anomaly_50.csv")), row.names = FALSE)
}
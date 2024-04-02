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

# load matfile
for (ivar in 1:2) {
  varname <- switch(ivar,
                    'GlbColour',
                    'modis')
    
  filepath <- (gsub(" ","",paste("./CHL_",varname,"_mat_15_300/")))
    
  # get lat and lon
  filename <- (gsub(" ","",paste(filepath,"latlon_Chl_Shelf.mat")))
  latlon   <- readMat(filename)
  latlon   <- latlon$latlon
  
  if (ivar == 1) {
    plotyear <- seq(1998,2021)
    # work the variable
    # loop all sites and do 1) envPred, 2) spectral analyses for entire time (breakdown in frequencies), 3) trend
    filename <- (gsub(" ","",paste(filepath,"seasonal_avg_CHL_Shelf_CNAPS.mat")))
    VarMaux  <- readMat(filename)
    VarM     <- VarMaux$seasonal.avg
    rm(VarMaux)
  } else {
    plotyear <- seq(2003,2022)
  }
  vartitle <- switch(ivar,
                     'Chl-a GlobColour',
                     'Chl-a Modis')
  all_avg_anomalies <- data.frame(Latitude = unique(latlon[,1]))
  
  for (iseason in 1:4){
    seasonname <- switch(iseason,
                      'Winter',
                      'Spring',
                      'Summer',
                      'Autumn')
    seasonlabel <- switch(iseason,
                         'Winter',
                         'Spring',
                         'Summer',
                         'Fall')

    if (ivar == 1) {
      varSeason <- VarM[ , ,iseason]
    } else {
      filename <- (gsub(" ","",paste(filepath,"chl_modis_shelf_",seasonname,".mat")))
      print(filename)
      VarMaux  <- readMat(filename)
      varSeason  <- VarMaux$varM
      rm(VarMaux)
    }
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
    ggsave(gsub(" ","",paste("gg_Chl_",varname,"_",seasonname,"_15.png")), plot, width = 10, height = 5, dpi = 300)
    
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
    
   all_avg_anomalies[[paste0(seasonlabel)]] <- avg_anomaly$avg_anomaly
  } 
  write.csv(all_avg_anomalies, file = gsub(" ","",paste(varname,"_anomaly_15.csv")), row.names = FALSE)
}

# load matfile
for (ivar in 1:2) {
  varname <- switch(ivar,
                    'GlbColour',
                    'modis')
  
  filepath <- (gsub(" ","",paste("./CHL_",varname,"_mat_50_300/")))
  
  # get lat and lon
  filename <- (gsub(" ","",paste(filepath,"latlon_Chl_Shelf.mat")))
  latlon   <- readMat(filename)
  latlon   <- latlon$latlon
  
  if (ivar == 1) {
    plotyear <- seq(1998,2021)
    # work the variable
    # loop all sites and do 1) envPred, 2) spectral analyses for entire time (breakdown in frequencies), 3) trend
    filename <- (gsub(" ","",paste(filepath,"seasonal_avg_CHL_Shelf_CNAPS.mat")))
    VarMaux  <- readMat(filename)
    VarM     <- VarMaux$seasonal.avg
    rm(VarMaux)
  } else {
    plotyear <- seq(2003,2022)
  }
  vartitle <- switch(ivar,
                     'Chl-a GlobColour',
                     'Chl-a Modis')
  all_avg_anomalies <- data.frame(Latitude = unique(latlon[,1]))
  
  for (iseason in 1:4){
    seasonname <- switch(iseason,
                         'Winter',
                         'Spring',
                         'Summer',
                         'Autumn')
    seasonlabel <- switch(iseason,
                          'Winter',
                          'Spring',
                          'Summer',
                          'Fall')
    if (ivar == 1) {
      varSeason <- VarM[ , ,iseason]
    } else {
      filename <- (gsub(" ","",paste(filepath,"chl_modis_shelf_",seasonname,".mat")))
      print(filename)
      VarMaux  <- readMat(filename)
      varSeason  <- VarMaux$varM
      rm(VarMaux)
    }
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
    ggsave(gsub(" ","",paste("gg_Chl_",varname,"_",seasonname,"_50.png")), plot, width = 10, height = 5, dpi = 300)
    
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
    
    all_avg_anomalies[[paste0(seasonlabel)]] <- avg_anomaly$avg_anomaly
  } 
  write.csv(all_avg_anomalies, file = gsub(" ","",paste(varname,"_anomaly_50.csv")), row.names = FALSE)
}
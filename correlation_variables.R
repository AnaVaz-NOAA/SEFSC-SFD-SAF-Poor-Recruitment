library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(R.matlab)

filepath <- ("./data/CNAPS_Shelf_15/")

# Convert data to matrix and transpose
dataux <- read.csv("./data/HM_UI_subsurface.csv", header = FALSE)
data_matrix <- t(as.matrix(dataux))

# Create a sequence of months,  years and lats
months <- rep(1:12, each = 28)
years <- rep(1993:2020, times = 12)
latitudes <- seq(28, 35.44, by = 0.04)

# Define latitude ranges
latitude_ranges <- cut(latitudes, breaks = c(27, 30, 31, 32, 33, 34, 36),
                       labels = c("28-30", "30-31", "31-32", "32-33", "33-34", "34-35"))

# Create a data frame with latitude range, month, year, and upwelling
data <- data.frame(Latitude = rep(latitude_ranges, each = length(years)),
                   Month = months,
                   Year = years,
                   Upwelling = as.vector(data_matrix))

# Define seasons
seasons <- data.frame(
  Season = c("Winter", "Spring", "Summer", "Fall"),
  Month = c(1, 4, 7, 10, 2, 5, 8, 11, 3, 6, 9, 12)
)
data <- merge(data, seasons, by = "Month")

# Calculate seasonal averages
seasonal_averages <- data %>%
  group_by(Latitude, Year, Season) %>%
  summarise(AvgUp = mean(Upwelling, na.rm = TRUE))

# Create a data frame with all latitude ranges
all_avg_anomalies <- data.frame(Latitude = unique(latitude_ranges))

for (iseason in 1:4) {
  seasonname <- switch(iseason,
                       'Winter',
                       'Spring',
                       'Summer',
                       'Fall')
  
  # Filter seasonal averages for the specified season
  season_data <- filter(seasonal_averages, Season == seasonname)
  
  # Create an empty plot
  anomaly_plot <- ggplot() +
    labs(title = paste("Upwelling Subsurface Anomalies: ", seasonname),
         x = "Year", y = "Upwelling Subsurface Anomaly") +
    theme_light()
  
  # Create an empty data frame to store the data
  season_data_combined <- data.frame()
  
  # Loop through latitude ranges
  for (lat_range in unique(season_data$Latitude)) {
    # Filter data for the specific latitude range and season
    lat_season_data <- filter(season_data, Latitude == lat_range)
    
    # Calculate long-term average (climatology) for this latitude range and season
    climatology <- mean(lat_season_data$AvgUp)
    
    # Calculate anomalies
    lat_season_data$Anomaly <- lat_season_data$AvgUp - climatology
    
    # Add data to combined data frame
    season_data_combined <- rbind(season_data_combined, lat_season_data)
    
    # Add anomaly curve to the plot
    # Plot anomalies over time
    anomaly_plot <- anomaly_plot +
      geom_line(data = lat_season_data, aes(x = Year, y = Anomaly, color = as.factor(Latitude))) +
      scale_color_brewer(palette = "Paired")  # Adjust color palette as needed
  }
  
  # Save the combined data frame to CSV
  write.csv(season_data_combined, file = paste0("Season_", seasonname, "_UpSub.csv"), row.names = FALSE)
  
  # Save the plot
  ggsave(paste0("Upwelling_Subsurface_Anomalies_Season_", seasonname, ".png"), 
         anomaly_plot, width = 8, height = 6, dpi = 300)
}

#------------------------------------------------------------------

# Convert data to matrix and transpose
dataux <- read.csv("./data/HM_UI_surface.csv", header = FALSE)
data_matrix <- t(as.matrix(dataux))

# Create a sequence of months,  years and lats
months <- rep(1:12, each = 28)
years <- rep(1993:2020, times = 12)
latitudes <- seq(28, 35.48, by = 0.04)

# Define latitude ranges
latitude_ranges <- cut(latitudes, breaks = c(27, 30, 31, 32, 33, 34, 36),
                       labels = c("28-30", "30-31", "31-32", "32-33", "33-34", "34-35"))

# Create a data frame with latitude range, month, year, and upwelling
data <- data.frame(Latitude = rep(latitude_ranges, each = length(years)),
                   Month = months,
                   Year = years,
                   Upwelling = as.vector(data_matrix))

# Define seasons
seasons <- data.frame(
  Season = c("Winter", "Spring", "Summer", "Fall"),
  Month = c(1, 4, 7, 10, 2, 5, 8, 11, 3, 6, 9, 12)
)
data <- merge(data, seasons, by = "Month")

# Calculate seasonal averages
seasonal_averages <- data %>%
  group_by(Latitude, Year, Season) %>%
  summarise(AvgUp = mean(Upwelling, na.rm = TRUE))

# Create a data frame with all latitude ranges
all_avg_anomalies <- data.frame(Latitude = unique(latitude_ranges))

for (iseason in 1:4) {
  seasonname <- switch(iseason,
                       'Winter',
                       'Spring',
                       'Summer',
                       'Fall')
  
  # Filter seasonal averages for the specified season
  season_data <- filter(seasonal_averages, Season == seasonname)
  
  # Create an empty plot
  anomaly_plot <- ggplot() +
    labs(title = paste("Upwelling Surface Anomalies: ", seasonname),
         x = "Year", y = "Surface Upwelling Anomaly") +
    theme_light()
  
  # Create an empty data frame to store the data
  season_data_combined <- data.frame()
  
  # Loop through latitude ranges
  for (lat_range in unique(season_data$Latitude)) {
    # Filter data for the specific latitude range and season
    lat_season_data <- filter(season_data, Latitude == lat_range)
    
    # Calculate long-term average (climatology) for this latitude range and season
    climatology <- mean(lat_season_data$AvgUp)
    
    # Calculate anomalies
    lat_season_data$Anomaly <- lat_season_data$AvgUp - climatology
    
    # Add data to combined data frame
    season_data_combined <- rbind(season_data_combined, lat_season_data)
    
    # Add anomaly curve to the plot
    # Plot anomalies over time
    anomaly_plot <- anomaly_plot +
      geom_line(data = lat_season_data, aes(x = Year, y = Anomaly, color = as.factor(Latitude))) +
      scale_color_brewer(palette = "Paired")  # Adjust color palette as needed
  }
  
  # Save the combined data frame to CSV
  write.csv(season_data_combined, file = paste0("Season_", seasonname, "_UpSurf.csv"), row.names = FALSE)
  
  # Save the plot
  ggsave(paste0("Upwelling_Surface_Anomalies_Season_", seasonname, ".png"), 
         anomaly_plot, width = 8, height = 6, dpi = 300)
}

library(Hmisc)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(cmocean)
library(maps)
library(tidyverse)
library(mapdata)
library(marmap)
library(dplyr)
library(lubridate)

amoc <- read.csv("./csv_files/newmoc2share.txt")
trip <- read.csv("./csv_files/tripole_idx.txt")

amoc$NewTime <- parse_date_time(amoc$Time, orders = c("dmy HMS", "dmY HMS"), tz = "UTC")

get_season <- function(date) {
  month <- month(date)
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Fall")
  }
}

amoc$Season <- sapply(amoc$NewTime, get_season)
amoc$Year   <- year(amoc$NewTime)

# Group by Season and Year, AMOC Avg
seasonal_avg_by_year <- amoc %>%
  group_by(Year, Season) %>%
  summarise(Avg_AMOC = mean(MOC_new, na.rm = TRUE)) %>%
  arrange(Year, Season)

climatology <- seasonal_avg_by_year %>%
  group_by(Season) %>%
  summarise(LongTerm_Avg = mean(Avg_AMOC, na.rm = TRUE))

# Merge long-term averages 
seasonal_anomalies <- seasonal_avg_by_year %>%
  left_join(climatology, by = "Season") %>%
  mutate(Anomaly = Avg_AMOC - LongTerm_Avg)

# Function to plot anomalies for each season
plot_anomaly <- function(season_data, season_name) {
  ggplot(season_data, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
    geom_bar(stat = "identity", position = "identity", width = 0.8) +
    scale_fill_manual(values = c("TRUE" = "tomato3", "FALSE" = "steelblue3"), guide = FALSE) +
    labs(title = paste("Anomalies for", season_name),
         x = "Year", y = "Anomaly") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Split the data by season
seasons <- unique(seasonal_anomalies$Season)

# Loop through each season and plot anomalies
for (season in seasons) {
  season_data <- seasonal_anomalies %>% filter(Season == season)
  print(plot_anomaly(season_data, season))
}

# Function to create ribbon plot of anomalies for each season
plot_ribbon_anomaly <- function(season_data, season_name) {
  ggplot(season_data, aes(x = Year)) +
    geom_ribbon(aes(ymin = 0, ymax = ifelse(Anomaly > 0, Anomaly, 0)), fill = "tomato3", alpha = 0.8) +
    geom_ribbon(aes(ymin = ifelse(Anomaly < 0, Anomaly, 0), ymax = 0), fill = "steelblue3", alpha = 0.8) +
    geom_line(aes(y = Anomaly), color = "gray60") +  # Add a black line to show the anomaly curve
    labs(title = paste("AMOC Anomaly ", season_name),
         x = "Year", y = "Anomaly") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Create a list to store the plots for each season
seasonal_plots <- list()

# Loop through each season and create the ribbon plots
for (season in unique(seasonal_anomalies$Season)) {
  season_data <- seasonal_anomalies %>% filter(Season == season)
  plot <- plot_ribbon_anomaly(season_data, season)
  seasonal_plots[[season]] <- plot
  print(plot)  # This will display each plot
}

# Optional: Save each plot as a separate file (e.g., PNG or PDF)
for (season in names(seasonal_plots)) {
  ggsave(filename = paste0("anomaly_amoc_", season, ".png"),
         plot = seasonal_plots[[season]], width = 8, height = 5)
}

#-------------------------------------------------------------------

trip$NewTime <- parse_date_time(trip$Time, orders = c("dmy", "dmY"), tz = "UTC")

trip$Season <- sapply(trip$NewTime, get_season)
trip$Year   <- year(trip$NewTime)

seasonal_avg_by_year <- trip %>%
  group_by(Year, Season) %>%
  summarise(Avg_Trip = mean(TripoleIndex, na.rm = TRUE)) %>%
  arrange(Year, Season)

climatology <- seasonal_avg_by_year %>%
  group_by(Season) %>%
  summarise(LongTerm_Avg = mean(Avg_Trip, na.rm = TRUE))

# Merge long-term averages 
seasonal_anomalies <- seasonal_avg_by_year %>%
  left_join(climatology, by = "Season") %>%
  mutate(Anomaly = Avg_Trip - LongTerm_Avg)

# Function to plot anomalies for each season
plot_anomaly <- function(season_data, season_name) {
  ggplot(season_data, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
    geom_bar(stat = "identity", position = "identity", width = 0.8) +
    scale_fill_manual(values = c("TRUE" = "tomato3", "FALSE" = "steelblue3"), guide = FALSE) +
    labs(title = paste("Tripole Anomaly ", season_name),
         x = "Year", y = "Anomaly") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Split the data by season
seasons <- unique(seasonal_anomalies$Season)

# Loop through each season and plot anomalies
for (season in seasons) {
  season_data <- seasonal_anomalies %>% filter(Season == season)
  print(plot_anomaly(season_data, season))
}

# Function to create ribbon plot of anomalies for each season
plot_ribbon_anomaly <- function(season_data, season_name) {
  ggplot(season_data, aes(x = Year)) +
    geom_ribbon(aes(ymin = 0, ymax = ifelse(Anomaly > 0, Anomaly, 0)), fill = "tomato3", alpha = 0.8) +
    geom_ribbon(aes(ymin = ifelse(Anomaly < 0, Anomaly, 0), ymax = 0), fill = "steelblue3", alpha = 0.8) +
    geom_line(aes(y = Anomaly), color = "gray60") +  
    labs(title = paste("Tripole Anomaly ", season_name),
         x = "Year", y = "Anomaly") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Create a list to store the plots for each season
seasonal_plots <- list()

# Loop through each season and create the ribbon plots
for (season in unique(seasonal_anomalies$Season)) {
  season_data <- seasonal_anomalies %>% filter(Season == season)
  plot <- plot_ribbon_anomaly(season_data, season)
  seasonal_plots[[season]] <- plot
  print(plot)  # This will display each plot
}

# Optional: Save each plot as a separate file (e.g., PNG or PDF)
for (season in names(seasonal_plots)) {
  ggsave(filename = paste0("anomaly_tripole_", season, ".png"),
         plot = seasonal_plots[[season]], width = 8, height = 5)
}



# Calculate annual averages
annual_avgT <- trip %>%
  mutate(Year = year(NewTime)) %>%  
  group_by(Year) %>%
  summarise(Avg_Trip = mean(TripoleIndex, na.rm = TRUE))  

long_term_avgT <- mean(annual_avgT$Avg_Trip, na.rm = TRUE)

# Calculate anomalies
annual_avgT <- annual_avgT %>%
  mutate(Anomaly_T = Avg_Trip)  

# Calculate annual averages
annual_avgA <- amoc %>%
  mutate(Year = year(NewTime)) %>%  
  group_by(Year) %>%
  summarise(Avg_AMOC = mean(MOC_new, na.rm = TRUE))  

long_term_avgA <- mean(annual_avgA$Avg_AMOC, na.rm = TRUE)

# Calculate anomalies
annual_avgA <- annual_avgA %>%
  mutate(Anomaly_A = Avg_AMOC - long_term_avgA) 

# Create a complete year sequence based on the range of years in the two datasets
complete_years <- data.frame(Year = seq(min(c(annual_avgT$Year, annual_avgA$Year)), 
                                        max(c(annual_avgT$Year, annual_avgA$Year)), 
                                        by = 1))

# Join the complete years with annual averages for TripoleIndex
annual_avgT_complete <- complete_years %>%
  left_join(annual_avgT, by = "Year")

# Join the complete years with annual averages for MOC_new
annual_avgA_complete <- complete_years %>%
  left_join(annual_avgA, by = "Year")

# Combine both datasets into one
combined_avg <- annual_avgT_complete %>%
  rename(Avg_Trip = Avg_Trip, Anomaly_T = Anomaly_T) %>%
  left_join(annual_avgA_complete, by = "Year") %>%
  mutate(Anomaly_A = ifelse(is.na(Anomaly_A), NA, Anomaly_A),
         Anomaly_T = ifelse(is.na(Anomaly_T), NA, Anomaly_T))
         
         
ggplot(combined_avg, aes(x = Year)) +
  geom_ribbon(aes(ymin = 0, ymax = ifelse(Anomaly_A > 0, Anomaly_A, 0)), fill = "tomato3", alpha = 0.8) +  # Positive anomalies
  geom_ribbon(aes(ymin = ifelse(Anomaly_A < 0, Anomaly_A, 0), ymax = 0), fill = "steelblue3", alpha = 0.8) +  # Negative anomalies
  labs(title = "Yearly AMOC Anomalies",
       x = "Year",
       y = "Anomaly") +
  theme_minimal()
ggsave("AMOCAnomalyYear.png")

ggplot(combined_avg, aes(x = Year)) +
  geom_ribbon(aes(ymin = 0, ymax = ifelse(Anomaly_T > 0, Anomaly_T, 0)), fill = "tomato3", alpha = 0.8) +  # Positive anomalies
  geom_ribbon(aes(ymin = ifelse(Anomaly_T < 0, Anomaly_T, 0), ymax = 0), fill = "steelblue3", alpha = 0.8) +  # Negative anomalies
  labs(title = "Yearly Tripole Anomalies",
       x = "Year",
       y = "Anomaly") +
  theme_minimal()
ggsave("TripoleAnomalyYear.png")

ggplot(combined_avg, aes(x = Year, y = Anomaly_T)) +
  geom_bar(aes(fill = ifelse(Anomaly_T > 0, "tomato3", "steelblue3")), 
           stat = "identity", alpha = 0.7) +
  scale_fill_identity() + 
  labs(title = "TripoleIndex Anomalies",
       x = "Year",
       y = "Anomaly") +
  theme_minimal() +
  xlim(min(combined_avg$Year), max(combined_avg$Year)) +  
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))
ggsave("TripoleAnomalyYear.png")

ggplot(combined_avg, aes(x = Year, y = Anomaly_A)) +
  geom_bar(aes(fill = ifelse(Anomaly_A > 0, "tomato3", "steelblue3")), 
           stat = "identity", alpha = 0.8) +
  scale_fill_identity() + 
  labs(title = "AMOC Transport Anomalies",
       x = "Year",
       y = "Anomaly") +
  theme_minimal() +
  xlim(min(combined_avg$Year), max(combined_avg$Year)) +  
  ylim(-2.5, 2.5)
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))
ggsave("AMOCAnomalyYear.png")

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
library(tidyr)

# WRANGLING DATA PORTION 
Upw    <- read.csv("upwelling.csv")

# Create a new column for seasons
Upw$season <- ifelse(Upw$MON %in% c(4, 5, 6), "Spring",
                     ifelse(Upw$MON %in% c(7, 8, 9), "Summer",
                            ifelse(Upw$MON %in% c(10, 11, 12), "Fall", "Winter")))

#----------------------------------
# SEASONAL Upwelling Surface
seasonal_averages <- Upw %>%
  group_by(YR, season) %>%
  summarise(avg_UISurf  = mean(UISurf))

# Pivot the data to wide format
seasonal_surf <- seasonal_averages %>%
  pivot_wider(names_from = season, values_from = avg_UISurf)

# Write the result to a new CSV file
write.csv(seasonal_surf, "seasonal_surface_upwelling.csv", row.names = FALSE)

#----------------------------------
# SEASONAL Upwelling Subsurface
seasonal_averages <- Upw %>%
  group_by(YR, season) %>%
  summarise(avg_UISub = mean(UISub))

# Pivot the data to wide format
seasonal_sub <- seasonal_averages %>%
  pivot_wider(names_from = season, values_from = avg_UISub)

# Write the result to a new CSV file
write.csv(seasonal_sub, "seasonal_deep_upwelling.csv", row.names = FALSE)

#----------------------------------
# SPAWNING SEASON
# Create a new column for spawning seasons
Upw$seasonSp <- ifelse(Upw$MON %in% c(2, 3, 4), "Winter",
                     ifelse(Upw$MON %in% c(6, 7, 8), "Summer",
                            "no"))

#----------------------------------
# SEASONAL Upwelling Surface
seasonal_averages <- Upw %>%
  group_by(YR, seasonSp) %>%
  summarise(avg_UISurf  = mean(UISurf))

# Pivot the data to wide format
seasonal_surf <- seasonal_averages %>%
  pivot_wider(names_from = seasonSp, values_from = avg_UISurf)

# Write the result to a new CSV file
write.csv(seasonal_surf, "seasonalSp_surface_upwelling.csv", row.names = FALSE)

#----------------------------------
# SEASONAL Upwelling Subsurface
seasonal_averages <- Upw %>%
  group_by(YR, seasonSp) %>%
  summarise(avg_UISub = mean(UISub))

# Pivot the data to wide format
seasonal_sub <- seasonal_averages %>%
  pivot_wider(names_from = seasonSp, values_from = avg_UISub)

# Write the result to a new CSV file
write.csv(seasonal_sub, "seasonalSp_deep_upwelling.csv", row.names = FALSE)

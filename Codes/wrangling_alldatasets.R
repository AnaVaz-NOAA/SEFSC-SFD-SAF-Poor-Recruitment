# Make correlations between indexes, Covar and recdev
# calculate spectral analyses
library(RColorBrewer)
library(dplyr)
library(reshape2)
library(ggplot2)
library(wesanderson)
library(dplyr)

# Read in the recruitment deviations and covariate datasets
RecDev <- read.csv("./csv_files/RecruitmentResiduals_Feb23.csv")

Season <- read.csv("./csv_files/Anomaly_Seasonal.csv")
Season_ChlShelf15 <- read.csv("./csv_files/Anomaly_Seasonal_ChlShelf50.csv")
Season_ChlShelf50 <- read.csv("./csv_files/Anomaly_Seasonal_ChlShelf50.csv")
Season_ChlModis15 <- read.csv("./csv_files/Anomaly_Seasonal_ChlModis15.csv")
Season_ChlModis50 <- read.csv("./csv_files/Anomaly_Seasonal_ChlModis50.csv")
deepUp <- read.csv("./csv_files/seasonal_deep_upwelling.csv")
surfUp <- read.csv("./csv_files/seasonal_surface_upwelling.csv")

Season_ChlShelf15 <- Season_ChlShelf15 %>% rename_with(~ gsub("Chl.", "ChlGlob15.", .), -X)
Season_ChlShelf50 <- Season_ChlShelf50 %>% rename_with(~ gsub("Chl.", "ChlGlob50.", .), -X)
Season_ChlModis15 <- Season_ChlModis15 %>% rename_with(~ gsub("Chl.", "ChlModis15.", .), -X)
Season_ChlModis50 <- Season_ChlModis50 %>% rename_with(~ gsub("Chl.", "ChlModis50.", .), -X)

# Rename columns for upwelling data
deepUp <- deepUp %>%
  rename(X = YR) %>%
  rename_with(~ paste0("deepGulf.", .), -X)

surfUp <- surfUp %>%
  rename(X = YR) %>%
  rename_with(~ paste0("surfGulf.", .), -X)

# Merge datasets by the common column "X"
CovarSeason_RecDev <- Season %>%
  full_join(Season_ChlShelf50, by = "X") %>%
  full_join(Season_ChlModis15, by = "X") %>%
  full_join(Season_ChlModis50, by = "X") %>%
  full_join(deepUp, by = "X") %>%
  full_join(surfUp, by = "X") %>%
  full_join(RecDev, by = "X")

# Save the combined data frames for later use
write.csv(CovarSeason_RecDev, "CovarSeason_RecDev.csv", row.names = FALSE)

#-------------------------------------------------------------------
# For all spawning datasets

SpSeason <- read.csv("./csv_files/Anomaly_Spawning_Seasonal.csv")
SpSeason_ChlSpShelf50 <- read.csv("./csv_files/Anomaly_Spawning_Seasonal_ChlSpShelf50.csv")

CovarSpSeason_RecDev <- SpSeason %>%
  full_join(SpSeason_ChlSpShelf50, by = "X") %>%
  full_join(Season_ChlModis15, by = "X") %>%
  full_join(Season_ChlModis50, by = "X") %>%
  full_join(deepUp, by = "X") %>%
  full_join(surfUp, by = "X") %>%
  full_join(RecDev, by = "X")

write.csv(CovarSpSeason_RecDev, "./csv_files/CovarSpSeason_RecDev.csv", row.names = FALSE)
# Load the required library
library(dplyr)
library(rhdf5)
library(lubridate)

# Read the CSV file
videoT <- read.csv("video_temp.csv")

# Get the last position for each unique event
last_positions <- videoT %>%
  group_by(Event) %>%
  slice_tail(n = 1)

tempM <- last_positions %>%
  select(Start_Latitude, Start_Longitude, Start_Depth, LastOfTemp, Date)

tempM <- tempM %>%
  rename(
    lat = Start_Latitude,
    lon = Start_Longitude,
    depth = Start_Depth,
    temp = LastOfTemp,
    date = Date
  )

#install.packages("lubridate")

# Assuming your date column is in the "date" column of the tempM data frame
tempM <- tempM %>%
  mutate(
    date = mdy(date),  # Convert to Date object assuming month-day-year format
    year = year(date),
    month = month(date),
    day = day(date)
  )

write.csv(tempM, "bottomTvideo.csv", row.names = FALSE)


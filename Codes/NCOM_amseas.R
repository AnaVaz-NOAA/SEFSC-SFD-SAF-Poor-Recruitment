# Read the file as lines
# from https://www.ncei.noaa.gov/thredds-coastal/dodsC/ncom_amseas_agg_20130405_20201216/AmSeas_Apr_05_2013_to_Dec_16_2020_best.ncd?time[0:1:20132]

lines <- readLines("~/Stuff/years_ncom_amseas.csv")
data  <- paste(lines, collapse = ",")

# Step 1: Convert data to numeric
dataaux <- as.numeric(strsplit(data, ",")[[1]])

# Step 2: Find missing values
difference   <- diff(dataaux)
missing_days <- which(difference > 3)
difference[missing_days]

# Convert data in hours to dates
origin <- as.POSIXct("2013-04-05 00:00:00", tz = "UTC")
data_in_dates <- origin + dataaux*3600

data_in_dates[missing_days]
data_in_dates[missing_days+1]
# > data_in_dates[missing_days]
# [1] "2015-04-04 UTC" "2016-10-02 UTC" "2019-10-11 UTC" "2019-11-29 UTC"
# > data_in_dates[missing_days+1]
# [1] "2016-01-01 UTC" "2016-10-03 UTC" "2019-11-02 UTC" "2019-12-04 UTC"

# from https://www.ncei.noaa.gov/thredds-coastal/dodsC/ncom_amseas_agg/AmSeas_Dec_17_2020_to_Current_best.ncd?time[0:1:7200]

lines <- readLines("~/Stuff/years_ncom_amseas_new.csv")
data  <- paste(lines, collapse = ",")

# Step 1: Convert data to numeric
dataaux <- as.numeric(strsplit(data, ",")[[1]])

# Step 2: Find missing values
difference   <- diff(dataaux)
missing_days <- which(difference > 3)
difference[missing_days]

# Convert data in hours to dates
origin <- as.POSIXct("2020-12-15 00:00:00.000", tz = "UTC")
data_in_dates <- origin + dataaux*3600

data_in_dates[missing_days]
data_in_dates[missing_days+1]
# No missing dates

# > data_in_dates[1]
#[1] "2020-12-15 UTC"
#> data_in_dates[7201]
#[1] "2023-06-03 UTC"
data_in_dates[7201]

# Read the file as lines
# from https://www.ncei.noaa.gov/thredds-coastal/dodsC/ncom_us_east_agg_20130405_20201216/US_East_Apr_05_2013_to_Dec_16_2020_best.ncd?time[0:1:19038]

lines <- readLines("~/Stuff/years_ncom_amseas.csv")
data  <- paste(lines, collapse = ",")

# Step 1: Convert data to numeric
dataaux <- as.numeric(strsplit(data, ",")[[1]])

# Step 2: Find missing values
difference   <- diff(dataaux)
missing_days <- which(difference > 3)
difference[missing_days]

# Convert data in hours to dates
origin <- as.POSIXct("2013-04-05 00:00:00", tz = "UTC")
data_in_dates <- origin + dataaux*3600

data_in_dates[missing_days]
data_in_dates[missing_days+1]
# > data_in_dates[missing_days]
# [1] "2015-04-04 UTC" "2016-10-02 UTC" "2019-10-11 UTC" "2019-11-29 UTC"
# > data_in_dates[missing_days+1]
# [1] "2016-01-01 UTC" "2016-10-03 UTC" "2019-11-02 UTC" "2019-12-04 UTC"

library(digitize)
digitize("~/Desktop/AMOC.png")
calibration <- ReadAndCal(filepath)

# Digitize the data points from the plot
data <- DigitData(col = 1)
print(data)
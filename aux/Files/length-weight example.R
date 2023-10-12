
# load packages
library(ggplot2)

setwd("//192.168.2.11/Jeff_K/Stock Assessment Training/Intermediate 2023-2024/Part 1 Mock Data Workshop/02 Life History Parameters") # change the directory location here to where you have the data file below located

lw_data <- read.csv("~/Downloads/Files/length-weight data.csv")

summary(lw_data)

lw_data <- lw_data[is.finite(lw_data$WEIGHT),]

plot(lw_data$WEIGHT~lw_data$LENGTH)

plot(log(lw_data$WEIGHT)~log(lw_data$LENGTH))

lw <- lm(log(WEIGHT)~log(LENGTH), data=lw_data) # log transform WEIGHT=a*LENGTH^b to log(WEIGHT)=log(a)+b*log(LENGTH)
summary(lw)

alpha <- as.numeric(exp(coef(lw)[1]))
beta <- as.numeric(coef(lw)[2]) 

plot(resid(lw))
abline(h=0, col="red")

lw_pred <- data.frame(LENGTH=seq(from=min(lw_data$LENGTH), to=max(lw_data$LENGTH), by=1))

lw_pred$WEIGHT <- exp(coef(lw)[1]+coef(lw)[2]*log(lw_pred$LENGTH)) # exponentiate to make predictions of WEIGHT on original scale
#lw_pred$WEIGHT <- alpha*lw_pred$LENGTH^beta

plot(x=lw_data$LENGTH,y=lw_data$WEIGHT)
lines(x=lw_pred$LENGTH, y=lw_pred$WEIGHT, col="red")

ggplot() + 
  geom_point(data=lw_data, aes(x=LENGTH,y=WEIGHT)) + 
  geom_line(data=lw_pred, aes(x=LENGTH, y=WEIGHT),color="red") +
  labs(x="Length (mm)", y="Weight (kg)") +
  theme_bw() 

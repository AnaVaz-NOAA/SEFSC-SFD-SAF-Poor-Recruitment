
# load packages
library(FSA)
library(fishmethods)
library(ggplot2)

setwd("//192.168.2.11/Jeff_K/Stock Assessment Training/Intermediate 2023-2024/Part 1 Mock Data Workshop/02 Life History Parameters") # change the directory location here to where you have the data file below located

biodata <- read.csv("growth data.csv") 

summary(biodata)

plot(x=biodata$Age, y=biodata$TL_cm)

#estimate growth
vB <- nls(formula=TL_cm~Linf*(1-exp(-(K*(Age-t0)))), data=biodata)

?vbStarts # get starting parameter guesses with vbStarts function in FSA package
vb_start <- vbStarts(TL_cm~Age, data=biodata)
vb_start

vB <- nls(formula=TL_cm~Linf*(1-exp(-(K*(Age-t0)))), data=biodata, start=vb_start)

summary(vB)

plot(resid(vB)~biodata$Age)
abline(h=0, col="red")

ages <- seq(from=min(biodata$Age), to=max(biodata$Age), by=1)

coef(vB)

vB_curve <- data.frame(Age=ages, TL_cm=coef(vB)[1] * (1 - exp(-(coef(vB)[2] * (ages-coef(vB)[3])))))

plot(x=biodata$Age, y=biodata$TL_cm)
lines(x=vB_curve$Age, y=vB_curve$TL_cm, col="red")

#compare growth by sex
table(biodata$Sex, useNA="ifany")

plot(x=biodata$Age, y=biodata$TL_cm, pch=19, col=as.factor(biodata$Sex))
legend("topright",
       legend = levels(factor(biodata$Sex)),
       pch = 19,
       col = factor(levels(factor(biodata$Sex))))

?growthlrt # compare growth by sex with growthlrt
sex_compare <- growthlrt(len=biodata$TL_cm, age=biodata$Age, group=biodata$Sex)
sex_compare

parameters <- as.data.frame(sex_compare$'model H2'$coefficients)
parameters

F_curve <- data.frame(Age=ages, TL_cm=parameters$Estimate[1] * (1 - exp(-(parameters$Estimate[3] * (ages-parameters$Estimate[4])))))
F_curve$Sex <- "F"

M_curve <- data.frame(Age=ages, TL_cm=(parameters$Estimate[1]+parameters$Estimate[2]) * (1 - exp(-(parameters$Estimate[3] * (ages-(parameters$Estimate[4]+parameters$Estimate[5]))))))
M_curve$Sex <- "M"

curves <- rbind(F_curve, M_curve)

plot(x=curves$Age, y=curves$TL_cm, pch=19, col=as.factor(curves$Sex))
legend("topleft",
       legend = levels(factor(curves$Sex)),
       pch = 19,
       col = factor(levels(factor(curves$Sex))))


ggplot() + geom_line(data=curves, aes(x=Age, y=TL_cm, colour=Sex)) + labs(x="Age", y="Length (cm)")+ theme_bw()

ggplot() + geom_line(data=curves, aes(x=Age, y=TL_cm, colour=Sex)) + geom_point(data=biodata, aes(x=Age, y=TL_cm, colour=Sex)) + labs(x="Age", y="Length (cm)")+ theme_bw()

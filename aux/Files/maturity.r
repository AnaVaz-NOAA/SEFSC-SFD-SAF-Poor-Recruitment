#This script will perform logistic regression on river herring maturity data and generate a maturity-at-length curve

#First, make sure RH_maturity is in same folder as maturity.r and set directory location to same source file (Session-Set Working Directory) 
RH_data <-read.csv("RH_maturity.csv",header=T)
head(RH_data) #the first few lines of dataset
tail(RH_data) #the last few lines of dataset

#Notice that this data contains info on both Alewife and Blueback herring. It also includes males and females. For now, let's subset the data and concentrate on female Alewife.

fem_ales=subset(RH_data,RH_data$SPECIES=="Alewife" & RH_data$SEX=="F")

#Now, visualize your data!
plot(Mature ~ Lgt, data=fem_ales,ylab="Mature (0=no,1=yes)",xlab="Length",main="Female maturity at length")

#Use a generalized linear model function in R to perform logistic regression on these data
model = glm(Mature ~ Lgt,family=binomial(logit), data=fem_ales)

#Plot  observed and predicted values
plot(Mature~ Lgt, data=fem_ales,ylab="Mature (0=no,1=yes)",main="Observed and predicted proportion of females mature at length")
pred_Lgt = seq(from=100, to=300, by=10)
pred_mat = predict(model,newdata=list(Lgt=pred_Lgt),type="r")
points(pred_Lgt, pred_mat, type="l", col="red")

#Examine results in more detail
#quick summary of what was done
model
#more details
summary(model)

#Pull out your model parameters 
#Eqn is y = [exp(b0 + b1x)] / [1 + exp(b0 + b1x)] 
#The intercept is your b0 and Length is your b1
model$coefficients

b0 <- as.numeric(model$coefficients[1])
b1 <- as.numeric(model$coefficients[2])

#Show original data and predicted curve on top using our newly estimated parameters
curve(exp(b0+b1*x)/(1+exp(b0+b1*x)),100,300,ylab="Mature (0=no,1=yes)",xlab="Length",main="Observed and predicted female maturity at length",type="l", col="red")
points(fem_ales$Mature~fem_ales$Lgt,)

#Create a variable to hold the percent mature that you want to calculate
p_mat <- 0.5 

#Rearrange the equation in line 31 to solve for x (Length at p_mat)
Length_at_p <- (log(p_mat/(1-p_mat))- b0)/b1


#You can add lines to your plot to represent that 50% mature intersection
abline(h=0.50) #add horizontal line at p(mature) = 0.5
abline(v=Length_at_p) #add vertical line at Length = Length_at_p


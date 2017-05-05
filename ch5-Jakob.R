# Outcome: log-transformed serum bilirubin.
# Trajectory: do at least a linear analysis, but you can test non-linear 
# relationships too (maybe a scatterplot could help to decide) 
# Explore the three different time-scales (if you think a priori that some of them don’t make sense then
# argue instead of fitting the model):
# Time since the begining of the study would be the prefered time sclae because we aim to assess the
# effect of treatment compared to placebo, rather than secular trends or patterns of changes in the 
# life course

# The main aim is to assess the effect of treatment compared to placebo
library(nlme) # fits mixed-effects models
library(cmprsk)
library(Epi)
#create a log-transformed serum bilirrubin
#explore
data <- pbc2
data$logbilirubin <- log10(data$serBilir)
data$yearSq <- data$year^2
plot(data$year[data$drug=="D-penicil"],data$logbilirubin[data$drug=="D-penicil"], col="red")
points(data$year[data$drug=="placebo"],data$logbilirubin[data$drug=="placebo"], col="blue")

#year ---
model1 <- lme(logbilirubin ~ drug*year+age, data = data, random= ~ 1|id, method="REML", na.action=na.omit)
summary(model1)

x <- seq(0,15, by=1)

x.pred1 <- cbind(1,0,x,x,0)  
x.pred2 <- cbind(1,1,x,x,1)

newdata <- rbind(x.pred1, x.pred2)

preddata <- matrix(data = NA, nrow = 0, ncol = 5)  #initialising matrix of predictions

for (j in 1:2){                                                     
    
    index1 <- 1 + (j-1)*(dim(newdata)[1])/2                                 
    index2 <- j*(dim(newdata)[1])/2
    
    y.pred <- newdata[index1:index2,] %*% fixef(model1)    #predicted values
    
    y.se <- ci.lin(model1,newdata[index1:index2,])[,2]     #std err of the mean
    y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
    y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
    
    temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
    preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

# create data frame object with meaningful column names
preddata <- data.frame(preddata)
names(preddata) <- c("time","logBil","lcb","ucb","drug")

plot(preddata[preddata$drug=="1", c("time", "logBil")], col = "red")
points(preddata[preddata$drug=="2", c("time", "logBil")], col = "blue")

#
##age ---
data$AgeAtCheck <- data$age+data$year
model1 <- lme(logbilirubin ~ drug*AgeAtCheck, data = data, random= ~ 1|id, method="REML", na.action=na.omit)
summary(model1)

x <- seq(25,84, by=3)

x.pred1 <- cbind(1,0,x,0)  
x.pred2 <- cbind(1,1,x,1)

newdata <- rbind(x.pred1, x.pred2)

preddata <- matrix(data = NA, nrow = 0, ncol = 5)  #initialising matrix of predictions

for (j in 1:2){                                                     
    
    index1 <- 1 + (j-1)*(dim(newdata)[1])/2                                 
    index2 <- j*(dim(newdata)[1])/2
    
    y.pred <- newdata[index1:index2,] %*% fixef(model1)    #predicted values
    
    y.se <- ci.lin(model1,newdata[index1:index2,])[,2]     #std err of the mean
    y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
    y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
    
    temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
    preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

# create data frame object with meaningful column names
preddata <- data.frame(preddata)
names(preddata) <- c("time","logBil","lcb","ucb","drug")

plot(preddata[preddata$drug=="1", c("time", "logBil")], col = "red")
points(preddata[preddata$drug=="2", c("time", "logBil")], col = "blue")



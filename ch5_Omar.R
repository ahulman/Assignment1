getwd()

setwd("C:/Users/au574377/Documents/PhD_AU/R/Assignment1")

library(JM)
library (Epi)
library(nlme)

#update.packages(checkBuilt = TRUE)
data.frame (pbc2)

# Outcome: log-transformed serum bilirubin.
# Trajectory: do at least a linear analysis, but you can test non-linear 
# relationships too (maybe a scatterplot could help to decide) 
# Explore the three different time-scales (if you think a priori that some of them don’t make sense then
# argue instead of fitting the model):
    # Time since the begining of the study would be the prefered time sclae because we aim to assess the
    # effect of treatment over log transformed bilirrubin levels comparing with placebo, rather than the assessment
    # of secular trends (calendar years time axis) or patterns of changes in the life course (age time axis).
    

# The main aim is to assess the effect of treatment compared to placebo

#create a log-transformed serum bilirrubin

pbc2$logserbil <- log(pbc2$serBilir)

#Primary biliary cirrhosis. Carey EJ. The Lancet.Volume 386, Issue 10003, 17–23 October 2015, Pages 1565–1575
    #Many histological staging systems have been developed, but Ludwig's is the most widely used:
        # Stage 1=portal inflammation, stage 2=extension to the periportal areas, 
        # stage 3=septal fibrosis or inflammatory bridging, and stage 4=cirrhosis
# A unique feature of primary biliary cirrhosis is the development of varices before the onset of cirrhosis
# Screening for varices is initiated when clinical or histological evidence of cirrhosis is reported.
    # Whether screening for varices should begin before the diagnosis of cirrhosis remains uncertain.
        # Platelet counts of less than 140 000 or less than 200 000 have been suggested as a threshold for the 
        # initiation of variceal screening, as has a Mayo risk score greater than 4·1.4
# The strongest predictor of death or liver transplantation was alkaline phosphatase more than 2 times
    # the upper limit of normal, 1 year after study enrolment (C statistic 0·71, 95% CI 0·69–0·73).
    # These markers are therefore useful surrogate endpoints for clinical trials or other assessment of
    # response to therapy. Male sex and young age at presentation are predictors of non-response to UDCA


#Subsetting for stratyfied analysis

men <- subset(pbc2, sex=="male")
women <- subset(pbc2, sex=="female")

#General Model
model1 <- lme(logserbil ~ years*drug + sex,random=~1|id, data=pbc2, method="REML", na.action=na.omit)
summary(model1)
ci.lin(model1)

#Figure for the general model
par(mfrow=c(1,2))
x <- seq(0,15,by=1)                                           #range of time

x.pred1 <- cbind(1,x, 0, 0,0)      # class 1
x.pred2 <- cbind(1,x, 1, 0,x)  # class 2

newdata <- rbind(x.pred1, x.pred2)                                      

preddata <- matrix(data = NA, nrow = 0, ncol = 5)

ng = 2
for (j in 1:ng){                                                     
  
  index1 <- 1 + (j-1)*(dim(newdata)[1])/ng                                 
  index2 <- j*(dim(newdata)[1])/ng
  
  y.pred <- newdata[index1:index2,] %*% fixef(model1)    #predicted values
  
  y.se <- ci.lin(model1,newdata[index1:index2,])[,2]     #std err of the mean
  y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
  y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
  
  temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
  preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

preddata <- data.frame(preddata)
names(preddata) <- c("time","y","cil","ciu","class")

#plot
cols <- c("black", "gray")

xlab = expression('Years')

plot(c(-9),c(-9),type="l",xlab="Years", ylab=expression('Log serum bilirrubin'), xlim=c(0,15), ylim=c(-2,2.5),lwd=3,xaxt="n",yaxt="n")
title("Model 1")
axis(1,at=seq(0,15,1), labels = seq(0, 15, 1))
axis(2,at=seq(-2,2.5,0.5))

for (i in 1:2) {
  lines(preddata$time[preddata$class==i],
        preddata$y[preddata$class==i],
        lwd=3, col = cols[i])
}

# CIs
for (i in 1:2) {
  lines(preddata$time[preddata$class==i],
        preddata$ciu[preddata$class==i],
        lwd=1,lty=2,col=cols[i])
  lines(preddata$time[preddata$class==i],
        preddata$cil[preddata$class==i],
        lwd=1,lty=2,col=cols[i])
}

legend(-0.7,-1,
       c("Control group", "Treatment group"),
       lty=c(1,1,1),
       lwd=c(5,5,5),
       col=cols,
       bty = "n",cex = 1, seg.len = 1)
#**************************************************
#Fully adjusted model
model2 <- lme(logserbil ~ years*drug + sex + spiders + alkaline + histologic,random=~1|id, data=pbc2, method="REML", na.action=na.omit)
summary(model2)
ci.lin(model2)

#Figure for the fully adjusted model

x.pred1 <- cbind(1,x, 0,0,0,0,0,0)      # class 1
x.pred2 <- cbind(1,x, 1,0,0,0,0,x)  # class 2

newdata <- rbind(x.pred1, x.pred2)                                      

preddata <- matrix(data = NA, nrow = 0, ncol = 5)

ng = 2
for (j in 1:ng){                                                     
  
  index1 <- 1 + (j-1)*(dim(newdata)[1])/ng                                 
  index2 <- j*(dim(newdata)[1])/ng
  
  y.pred <- newdata[index1:index2,] %*% fixef(model2)    #predicted values
  
  y.se <- ci.lin(model2,newdata[index1:index2,])[,2]     #std err of the mean
  y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
  y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
  
  temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
  preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

preddata <- data.frame(preddata)
names(preddata) <- c("time","y","cil","ciu","class")

#plot
cols <- c("black", "gray")

xlab = expression('Years')

plot(c(-9),c(-9),type="l",xlab="Years", ylab=expression('Log serum bilirrubin'), xlim=c(0,15), ylim=c(-2,2.5),lwd=3,xaxt="n",yaxt="n")
title("Model 2")
axis(1,at=seq(0,15,1), labels = seq(0, 15, 1))
axis(2,at=seq(-2,2.5,0.5))

for (i in 1:2) {
  lines(preddata$time[preddata$class==i],
        preddata$y[preddata$class==i],
        lwd=3, col = cols[i])
}

# CIs
for (i in 1:2) {
  lines(preddata$time[preddata$class==i],
        preddata$ciu[preddata$class==i],
        lwd=1,lty=2,col=cols[i])
  lines(preddata$time[preddata$class==i],
        preddata$cil[preddata$class==i],
        lwd=1,lty=2,col=cols[i])
}


#Stratified analysis by sex
  #Men
menlogserbil1 <- lme(logserbil ~ years*drug,random=~1|id, data=men, method="REML", na.action=na.omit)
summary(menlogserbil1)
ci.lin(menlogserbil1)


#Figure
par(mfrow=c(2,2))
x <- seq(0,15,by=1)                                           #range of time

x.pred1 <- cbind(1,x, 0,0)      # class 1
x.pred2 <- cbind(1,x, 1,x)  # class 2

newdata <- rbind(x.pred1, x.pred2)                                      

preddata <- matrix(data = NA, nrow = 0, ncol = 5)

ng = 2
for (j in 1:ng){                                                     
  
  index1 <- 1 + (j-1)*(dim(newdata)[1])/ng                                 
  index2 <- j*(dim(newdata)[1])/ng
  
  y.pred <- newdata[index1:index2,] %*% fixef(menlogserbil1)    #predicted values
  
  y.se <- ci.lin(menlogserbil1,newdata[index1:index2,])[,2]     #std err of the mean
  y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
  y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
  
  temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
  preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

preddata <- data.frame(preddata)
names(preddata) <- c("time","y","cil","ciu","class")

#plot
cols <- c("black", "gray")

xlab = expression('Years')

plot(c(-9),c(-9),type="l",xlab="Years", ylab=expression('Log serum bilirrubin'), xlim=c(0,15), ylim=c(-1.5,2.5),lwd=3,xaxt="n",yaxt="n")
title("Men")
axis(1,at=seq(0,15,1), labels = seq(0, 15, 1))
axis(2,at=seq(-1.5,2.5,0.5))

for (i in 1:2) {
  lines(preddata$time[preddata$class==i],
        preddata$y[preddata$class==i],
        lwd=3, col = cols[i])
}

# CIs
for (i in 1:2) {
  lines(preddata$time[preddata$class==i],
        preddata$ciu[preddata$class==i],
        lwd=1,lty=2,col=cols[i])
  lines(preddata$time[preddata$class==i],
        preddata$cil[preddata$class==i],
        lwd=1,lty=2,col=cols[i])
}

legend(0,0.2,
       c("Control group", "Treatment group"),
       lty=c(1,1,1),
       lwd=c(5,5,5),
       col=cols,
       bty = "n",cex = 1, seg.len = 1)


  #Women
womenlogserbil1 <- lme(logserbil ~ years*drug ,random=~1|id, data=women, method="REML", na.action=na.omit)
summary(womenlogserbil1)
ci.lin(womenlogserbil1)

#Figure
x.pred1 <- cbind(1,x, 0,0)      # class 1
x.pred2 <- cbind(1,x, 1,x)  # class 2

newdata <- rbind(x.pred1, x.pred2)                                      

preddata <- matrix(data = NA, nrow = 0, ncol = 5)

ng = 2
for (j in 1:ng){                                                     
  
  index1 <- 1 + (j-1)*(dim(newdata)[1])/ng                                 
  index2 <- j*(dim(newdata)[1])/ng
  
  y.pred <- newdata[index1:index2,] %*% fixef(womenlogserbil1)    #predicted values
  
  y.se <- ci.lin(womenlogserbil1,newdata[index1:index2,])[,2]     #std err of the mean
  y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
  y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
  
  temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
  preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

preddata <- data.frame(preddata)
names(preddata) <- c("time","y","cil","ciu","class")

#plot
cols <- c("black", "gray")

xlab = expression('Years')

plot(c(-9),c(-9),type="l",xlab="Years", ylab=expression('Log serum bilirrubin'), xlim=c(0,15), ylim=c(-1.5,2.5),lwd=3,xaxt="n",yaxt="n")
title("Women")
axis(1,at=seq(0,15,1), labels = seq(0, 15, 1))
axis(2,at=seq(-1.5,2.5,0.5))

for (i in 1:2) {
  lines(preddata$time[preddata$class==i],
        preddata$y[preddata$class==i],
        lwd=3, col = cols[i])
}

# CIs
for (i in 1:2) {
  lines(preddata$time[preddata$class==i],
        preddata$ciu[preddata$class==i],
        lwd=1,lty=2,col=cols[i])
  lines(preddata$time[preddata$class==i],
        preddata$cil[preddata$class==i],
        lwd=1,lty=2,col=cols[i])
}

#Fully adjusted stratifyed models
menlogserbil2 <- lme(logserbil ~ years*drug + spiders + alkaline + histologic,random=~1|id, data=men, method="REML", na.action=na.omit)
summary(menlogserbil2)
ci.lin(menlogserbil2)

#Figure
x.pred1 <- cbind(1,x, 0,0,0,0,0)      # class 1
x.pred2 <- cbind(1,x, 1,0,0,0,x)  # class 2

newdata <- rbind(x.pred1, x.pred2)                                      

preddata <- matrix(data = NA, nrow = 0, ncol = 5)

ng = 2
for (j in 1:ng){                                                     
    
    index1 <- 1 + (j-1)*(dim(newdata)[1])/ng                                 
    index2 <- j*(dim(newdata)[1])/ng
    
    y.pred <- newdata[index1:index2,] %*% fixef(menlogserbil2)    #predicted values
    
    y.se <- ci.lin(menlogserbil2,newdata[index1:index2,])[,2]     #std err of the mean
    y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
    y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
    
    temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
    preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

preddata <- data.frame(preddata)
names(preddata) <- c("time","y","cil","ciu","class")

#plot
cols <- c("black", "gray")

xlab = expression('Years')

plot(c(-9),c(-9),type="l",xlab="Years", ylab=expression('Log serum bilirrubin'), xlim=c(0,15), ylim=c(-2,2.5),lwd=3,xaxt="n",yaxt="n")
title("Men")
axis(1,at=seq(0,15,1), labels = seq(0, 15, 1))
axis(2,at=seq(-2,2.5,0.5))

for (i in 1:2) {
    lines(preddata$time[preddata$class==i],
          preddata$y[preddata$class==i],
          lwd=3, col = cols[i])
}

# CIs
for (i in 1:2) {
    lines(preddata$time[preddata$class==i],
          preddata$ciu[preddata$class==i],
          lwd=1,lty=2,col=cols[i])
    lines(preddata$time[preddata$class==i],
          preddata$cil[preddata$class==i],
          lwd=1,lty=2,col=cols[i])
}


womenlogserbil2 <- lme(logserbil ~ years*drug + spiders + alkaline + histologic,random=~1|id, data=women, method="REML", na.action=na.omit)
summary(womenlogserbil2)
ci.lin(womenlogserbil2)

#Figure
x.pred1 <- cbind(1,x, 0,0,0,0,0)      # class 1
x.pred2 <- cbind(1,x, 1,0,0,0,x)  # class 2

newdata <- rbind(x.pred1, x.pred2)                                      

preddata <- matrix(data = NA, nrow = 0, ncol = 5)

ng = 2
for (j in 1:ng){                                                     
    
    index1 <- 1 + (j-1)*(dim(newdata)[1])/ng                                 
    index2 <- j*(dim(newdata)[1])/ng
    
    y.pred <- newdata[index1:index2,] %*% fixef(womenlogserbil2)    #predicted values
    
    y.se <- ci.lin(womenlogserbil2,newdata[index1:index2,])[,2]     #std err of the mean
    y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
    y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
    
    temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
    preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

preddata <- data.frame(preddata)
names(preddata) <- c("time","y","cil","ciu","class")

#plot
cols <- c("black", "gray")

xlab = expression('Years')

plot(c(-9),c(-9),type="l",xlab="Years", ylab=expression('Log serum bilirrubin'), xlim=c(0,15), ylim=c(-2,2.5),lwd=3,xaxt="n",yaxt="n")
title("Women")
axis(1,at=seq(0,15,1), labels = seq(0, 15, 1))
axis(2,at=seq(-2,2.5,0.5))

for (i in 1:2) {
    lines(preddata$time[preddata$class==i],
          preddata$y[preddata$class==i],
          lwd=3, col = cols[i])
}

# CIs
for (i in 1:2) {
    lines(preddata$time[preddata$class==i],
          preddata$ciu[preddata$class==i],
          lwd=1,lty=2,col=cols[i])
    lines(preddata$time[preddata$class==i],
          preddata$cil[preddata$class==i],
          lwd=1,lty=2,col=cols[i])
}

#Models with interaction terms between sex and treatment 
modeli1 <- lme(logserbil ~ years*drug + sex*drug,random=~1|id, data=pbc2, method="REML", na.action=na.omit)
summary(modeli1)
ci.lin(modeli1)

#Figure
par(mfrow=c(1,2))
x <- seq(0,15,by=1)                                           #range of time

x.pred1 <- cbind(1,x, 0,0,0,0)      # class 1
x.pred2 <- cbind(1,x, 1,0,x,1)  # class 2

newdata <- rbind(x.pred1, x.pred2)                                      

preddata <- matrix(data = NA, nrow = 0, ncol = 5)

ng = 2
for (j in 1:ng){                                                     
    
    index1 <- 1 + (j-1)*(dim(newdata)[1])/ng                                 
    index2 <- j*(dim(newdata)[1])/ng
    
    y.pred <- newdata[index1:index2,] %*% fixef(modeli1)    #predicted values
    
    y.se <- ci.lin(modeli1,newdata[index1:index2,])[,2]     #std err of the mean
    y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
    y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
    
    temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
    preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

preddata <- data.frame(preddata)
names(preddata) <- c("time","y","cil","ciu","class")

#plot
cols <- c("black", "gray")

xlab = expression('Years')

plot(c(-9),c(-9),type="l",xlab="Years", ylab=expression('Log serum bilirrubin'), xlim=c(0,15), ylim=c(-2,2.5),lwd=3,xaxt="n",yaxt="n")
title("Model 1")
axis(1,at=seq(0,15,1), labels = seq(0, 15, 1))
axis(2,at=seq(-2,2.5,0.5))

for (i in 1:2) {
    lines(preddata$time[preddata$class==i],
          preddata$y[preddata$class==i],
          lwd=3, col = cols[i])
}

# CIs
for (i in 1:2) {
    lines(preddata$time[preddata$class==i],
          preddata$ciu[preddata$class==i],
          lwd=1,lty=2,col=cols[i])
    lines(preddata$time[preddata$class==i],
          preddata$cil[preddata$class==i],
          lwd=1,lty=2,col=cols[i])
}

legend(-0.7,-1,
       c("Control group", "Treatment group"),
       lty=c(1,1,1),
       lwd=c(5,5,5),
       col=cols,
       bty = "n",cex = 1, seg.len = 1)

modeli2 <- lme(logserbil ~ years*drug + sex*drug + spiders + alkaline + histologic,random=~1|id, data=pbc2, method="REML", na.action=na.omit)
summary(modeli2)
ci.lin(modeli2)

#Figure
x.pred1 <- cbind(1,x, 0,0,0,0,0,0,0)      # class 1
x.pred2 <- cbind(1,x, 1,0,0,0,0,x,1)  # class 2

newdata <- rbind(x.pred1, x.pred2)                                      

preddata <- matrix(data = NA, nrow = 0, ncol = 5)

ng = 2
for (j in 1:ng){                                                     
    
    index1 <- 1 + (j-1)*(dim(newdata)[1])/ng                                 
    index2 <- j*(dim(newdata)[1])/ng
    
    y.pred <- newdata[index1:index2,] %*% fixef(modeli2)    #predicted values
    
    y.se <- ci.lin(modeli2,newdata[index1:index2,])[,2]     #std err of the mean
    y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
    y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
    
    temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
    preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

preddata <- data.frame(preddata)
names(preddata) <- c("time","y","cil","ciu","class")

#plot
cols <- c("black", "gray")

xlab = expression('Years')

plot(c(-9),c(-9),type="l",xlab="Years", ylab=expression('Log serum bilirrubin'), xlim=c(0,15), ylim=c(-2,2.5),lwd=3,xaxt="n",yaxt="n")
title("Model2")
axis(1,at=seq(0,15,1), labels = seq(0, 15, 1))
axis(2,at=seq(-2,2.5,0.5))

for (i in 1:2) {
    lines(preddata$time[preddata$class==i],
          preddata$y[preddata$class==i],
          lwd=3, col = cols[i])
}

# CIs
for (i in 1:2) {
    lines(preddata$time[preddata$class==i],
          preddata$ciu[preddata$class==i],
          lwd=1,lty=2,col=cols[i])
    lines(preddata$time[preddata$class==i],
          preddata$cil[preddata$class==i],
          lwd=1,lty=2,col=cols[i])
}



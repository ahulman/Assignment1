getwd()

setwd("C:/Users/au574377/Documents/PhD_AU/R/Assignment1")

library(JM)
library (Epi)
library(nlme)

#update.packages(checkBuilt = TRUE)

pbc2 <- data.frame (pbc2)

pbc2$logserbil <- log(pbc2$serBilir)

pbc2$t0 <- pbc2$years-pbc2$year

#Trajectories
model1 <- lme(logserbil ~ t0*drug + sex,random=~1|id, data=pbc2, method="ML", na.action=na.omit)
summary(model1)
ci.lin(model1)

#Figure 
par(mfrow=c(1,1))
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

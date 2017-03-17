# code recycled from Dorte Vistisen

library(nlme) # fits mixed-effects models
library(Epi)

mGluc_f <- lme(gluc_f ~ time*ethnic51  + time2*ethnic51 + time3*ethnic51 + I(endage-60) + sex+ I(bmi-28) + lgrlump + physiclv + hdiet, random= ~ 1|stno, data = completeData, method="ML",na.action=na.omit)
summary(mGluc_f)
x <- seq(-20,0,by=0.5)

x.pred1 <- cbind(1,x,0,x^2,x^3,0,0,0,0,0,0,0,0,0,0,0,0)  
x.pred2 <- cbind(1,x,1,x^2,x^3,0,0,0,0,0,0,0,0,0,x,x^2,x^3)

newdata <- rbind(x.pred1, x.pred2)

preddata <- matrix(data = NA, nrow = 0, ncol = 5)  #initialising matrix of predictions

for (j in 1:2){                                                     
    
    index1 <- 1 + (j-1)*(dim(newdata)[1])/2                                 
    index2 <- j*(dim(newdata)[1])/2
    
    y.pred <- newdata[index1:index2,] %*% fixef(mGluc_f)    #predicted values
    
    y.se <- ci.lin(mGluc_f,newdata[index1:index2,])[,2]     #std err of the mean
    y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
    y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
    
    temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
    preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

# create data frame object with meaningful column names
preddata <- data.frame(preddata)
names(preddata) <- c("time","y","cil","ciu","class")


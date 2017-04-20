#Challenge 5

library(JM)
library(Epi)

summary(pbc2)

# Outcome: log-transformed serum bilirubin.
# Explore the three different time-scales (if you think a priori that some of them 
#don’t make sense then argue instead of fitting the model).
#     Time since entry at visit date - most appropriaye 
#     age at vist date
#     chronolgic calendar time

# The main aim is to assess the effect of treatment compared to placebo.

#First we transform serum bilirubin:

pbc2$logbilir <- log(pbc2$serBilir)

#First we make the basic model:
m1 <- lme(logbilir ~ drug , random = ~ 1|id , data = pbc2 , method = "ML" , na.action = na.omit)

summary(m1)

#pbc2$year: number of years between enrollment and this visit date.
m2 <- lme(logbilir ~ year * drug , random = ~ 1|id , data = pbc2 , method = "REML" , na.action = na.omit)
summary(m2)
summary(pbc2$year)

x <- seq(0,15,by=1)

x.pred1 <- cbind(1,x,0,0)  
x.pred2 <- cbind(1,x,1,0)

newdata <- rbind(x.pred1, x.pred2)

preddata <- matrix(data = NA, nrow = 0, ncol = 5)  #initialising matrix of predictions

for (j in 1:2){                                                     
  
  index1 <- 1 + (j-1)*(dim(newdata)[1])/2                                 
  index2 <- j*(dim(newdata)[1])/2
  
  y.pred <- newdata[index1:index2,] %*% fixef(m2)    #predicted values
  
  y.se <- ci.lin(m2,newdata[index1:index2,])[,2]     #std err of the mean
  y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
  y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
  
  temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
  preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

# create data frame object with meaningful column names
preddata <- data.frame(preddata)
names(preddata) <- c("time","logbilir","cil","ciu","drug")

#drawing some figures of the results
plot(c(0,0) , main = "bmi trajectorie", xlab = "years from start" , ylab = "log bilirubin" ,
     xlim = c(0,15),
     ylim = c(-2,2),
)
# add points by sex
points(preddata[preddata$drug==1 ,c("time","logbilir")], col = "blue")
points(preddata[preddata$drug==2,c("time","logbilir")], col = "red")
lines(preddata[preddata$drug==1 ,c("time","logbilir")], col = "blue")
lines(preddata[preddata$drug==2,c("time","logbilir")], col = "red")
legend(0,2, c("placebo","drug"), col = c("blue","red"), pch = 1)

#confidence intercal is also plotted
lines(preddata[preddata$drug==1 ,c("time","cil")], col = "blue")
lines(preddata[preddata$drug==1 ,c("time","ciu")], col = "blue")

lines(preddata[preddata$drug==2 ,c("time","cil")], col = "red")
lines(preddata[preddata$drug==2 ,c("time","ciu")], col = "red")








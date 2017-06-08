#ch6_lasse
library(JM)
library(Epi)

pbc2<- data.frame(pbc2)
summary(pbc2)


#1) Check out whether the deceased or the transplant group is larger and keep that for the rest of the exercise
length(unique(pbc2$id[pbc2$status=="transplanted"]))
length(unique(pbc2$id[pbc2$status=="dead"]))
length(unique(pbc2$id[pbc2$status=="alive"]))

pbc2 <- subset(pbc2, (pbc2$status=="dead" | pbc2$status=="alive" ))
              
#Set time=0 at event occurence and fit log-serum bilirubin trajectories "backwards in time" (see Tabak et al., Lancet 2009)
pbc2$logbilir <- log(pbc2$serBilir)

pbc2$time_to_end_fup <- 0 - (pbc2$years - pbc2$year)

#We fit a basic
m1 <- lme(logbilir ~ time_to_end_fup  , random = ~ 1|id , data = pbc2 , method = "REML" , na.action = na.omit)

summary(m1)


#pbc2$year: number of years between enrollment and this visit date.
m2 <- lme(logbilir ~ year * drug + sex , random = ~ 1|id , data = pbc2 , method = "REML" , na.action = na.omit)
summary(m2)
summary(pbc2$time_to_end_fup)

x <- seq(-15,0,by=1)

x.pred1 <- cbind(1,x,0,0,0)  
x.pred2 <- cbind(1,x,1,0,0)

newdata <- rbind(x.pred1, x.pred2)


#Compare the treatment and the placebo group
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
     xlim = c(-15,0),
     ylim = c(-2,2),
)
# add points by sex
points(preddata[preddata$drug==1 ,c("time","logbilir")], col = "blue")
points(preddata[preddata$drug==2,c("time","logbilir")], col = "red")
lines(preddata[preddata$drug==1 ,c("time","logbilir")], col = "blue")
lines(preddata[preddata$drug==2,c("time","logbilir")], col = "red")
legend(-5,2, c("placebo","drug"), col = c("blue","red"), pch = 1)

#confidence intercal is also plotted
lines(preddata[preddata$drug==1 ,c("time","cil")], col = "blue")
lines(preddata[preddata$drug==1 ,c("time","ciu")], col = "blue")

lines(preddata[preddata$drug==2 ,c("time","cil")], col = "red")
lines(preddata[preddata$drug==2 ,c("time","ciu")], col = "red")











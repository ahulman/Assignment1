#LASSE
#Set the random seed to 123 so that we all get the same results. set.seed(123)
library(Epi)
library(dplyr)

set.seed(123)

N<- 10128
ID <- c(1:N)

age_0 <- runif(N, min = 40, max = 70)

#0=kvinder 1=mænd
sex <- rep.int(0,N*4107/10128)
sex <- append(sex, rep.int(1,N*6021/10128))

data <- data.frame( "id" =ID, "sex" = sex )

data$sex <- factor(data$sex , levels = c(0,1) , labels=c("female" , "male") )

data <- cbind(data , age_0)

#2
#Follow-up visits are planned every 5 years, but as in reality they do not happen exactly 5 years apart. 
#Therefore let's add some random noise at each phase the following way. Create a follow-up time variable 
#for each phase: fup_0 is 0 for everyone, becaause that's baseline. fup_n=fup_0 + 5n + u, where n=1,2,3 
#and u is a random number from a uniform distribution between -0.5 and 0.5. This way each study phase lasts 
#for a year.
#time at follow-up
data$fup_0 <- 0

u <- runif(10128 , -.5,.5)
r <- runif(10128 , -.5,.5)
s <- runif(10128 , -.5,.5)
hist(u)

data$fup_1 <- data$fup_0 + 5*1 + u
data$fup_2 <- data$fup_0 + 5*2 + r
data$fup_3 <- data$fup_0 + 5*3 + s

#
#3 Calculate the corresponding ages at each phase i.e. age_1, age_2, age_3. (age_0 had already been calculated)

data$age_1 = age_0 + data$fup_1 
data$age_2 = age_0 + data$fup_2 
data$age_3 = age_0 + data$fup_3 

#cleaning
data$fup_0 <- NULL
data$fup_1 <- NULL
data$fup_2 <- NULL
data$fup_3 <- NULL
#
# 4 Assuming that the study started in 1990 (create an examination "date" for everyone from 
#a uniform distribution between 1990 and 1991) 
startdate <-  cal.yr( "1990/01/01", format="%Y/%m/%d" )
data$doEnt <- runif(10128 , startdate, startdate+1)
#and calculate year of birth for everyone (called it yob).
data$yob <- data$doEnt - data$age_0

#5
#Calculate BMI at each phase using the following equations:
#Men: 2.04 + 0.944 age - 0.008 age^2 - 0.08 (yob - 1950) + E, where E is random noise from N(mean=0,sd=3.5)
#Women: -14.4 + 1.549 age - 0.013 age^2 + 0.08 (yob - 1950) + E, where E is random noise from N(mean=0,sd=3.5)
#Make sure that the random noise is different at each phase, otherwise you will get a perfect quadratic curve for each individual
#Age changes from phase to phase, baseline age doesn't!
x<- cal.yr( "1950/01/01", format="%Y/%m/%d" )

data$bmi_0 <- ifelse(data$sex=="male",2.04 + 0.944*data$age_0 - 0.008*(data$age_0^2) - 0.08*(data$yob-1950) + rnorm(N, mean = 0 , sd=3.5), 
                     -14.4 + 1.549*data$age_0 - 0.013*(data$age_0^2) - 0.08*(data$yob-1950)  + rnorm(N, 0 , 3.5))

data$bmi_1 <- ifelse(data$sex=="male",2.04 + 0.944*data$age_1 - 0.008*(data$age_1^2) - 0.08*(data$yob-1950) + rnorm(N, mean = 0 , sd=3.5), 
                     -14.4 + 1.549*data$age_1 - 0.013*(data$age_1^2) - 0.08*(data$yob-1950)  + rnorm(N, 0 , 3.5))

data$bmi_2 <- ifelse(data$sex=="male",2.04 + 0.944*data$age_2 - 0.008*(data$age_2^2) - 0.08*(data$yob-1950) + rnorm(N, mean = 0 , sd=3.5), 
                     -14.4 + 1.549*data$age_2 - 0.013*(data$age_2^2) - 0.08*(data$yob-1950)  + rnorm(N, 0 , 3.5))

data$bmi_3 <- ifelse(data$sex=="male",2.04 + 0.944*data$age_3 - 0.008*(data$age_3^2) - 0.08*(data$yob-1950) + rnorm(N, mean = 0 , sd=3.5), 
                     -14.4 + 1.549*data$age_3 - 0.013*(data$age_3^2) - 0.08*(data$yob-1950)  + rnorm(N, 0 , 3.5))

data <- data[c("id" , "sex" , "yob" , "doEnt" , "age_0", "age_1", "age_2", "age_3",
               "bmi_0" , "bmi_1" , "bmi_2" , "bmi_3" )]
#6
#Simulate loss to follow-up (MCAR or MAR assumption? look up what this means) the following way:
#  Assume that if someone does not attend a phase then (s)he will never return (000x is ok, 0x00 is not ok)
#Proportion of participants at each phase, men: 100%, 80%, 70%, 60% (or as close as possible if N is not a whole number)
#Proportion of participants at each phase, women: 100%, 90%, 80%, 70%
#  Hint: generate the data for each phase for everyone, but then change the ones not attending to NA (both age and BMI)

# data$ltf = loss to follow-up
table(data$sex)

data$ltf_1 <- NA

data$ltf_1[data$sex =="male"] <- rbinom(sum(data$sex =="male") , 1, 8/10)
data$ltf_1[data$sex =="male"] <- ifelse(data$ltf_1[data$sex=="male"]== 0 , NA , 1)
table(data$ltf_1[data$sex=="male"],useNA="ifany")

data$ltf_1[data$sex =="female"] <- rbinom(sum(data$sex =="female") , 1, 9/10)
data$ltf_1[data$sex =="female"] <- ifelse(data$ltf_1[data$sex=="female"]== 0 , NA , 1)
table(data$ltf_1[data$sex=="female"],useNA="ifany")

data$ltf_2 <- NA

data$ltf_2[data$sex =="male" & !is.na(data$ltf_1)] <- rbinom( (sum(data$sex =="male" & data$ltf_1==1 , na.rm=T)) , 1, 7/8)
data$ltf_2[data$sex =="male"] <- ifelse(data$ltf_2[data$sex=="male"]== 0 , NA , 1)
table(data$ltf_2[data$sex=="male"],useNA="ifany")

data$ltf_2[data$sex =="female" & !is.na(data$ltf_1)] <- rbinom( (sum(data$sex =="female" & data$ltf_1==1 , na.rm=T)) , 1, 8/9)
data$ltf_2[data$sex =="female"] <- ifelse(data$ltf_2[data$sex=="female"]== 0 , NA , 1)
table(data$ltf_2[data$sex=="female"],useNA="ifany")

data$ltf_3 <- NA

data$ltf_3[data$sex =="male" & !is.na(data$ltf_2)] <- rbinom( (sum(data$sex =="male" & !is.na(data$ltf_2) ) ) , 1, 6/7)
data$ltf_3[data$sex =="male"] <- ifelse(data$ltf_3[data$sex=="male"]== 0 , NA , 1)
table(data$ltf_3[data$sex=="male"],useNA="ifany")


data$ltf_3[data$sex =="female" & !is.na(data$ltf_2)] <- rbinom( (sum(data$sex =="female" & !is.na(data$ltf_2) ) ) , 1, 7/8)
data$ltf_3[data$sex =="female"] <- ifelse(data$ltf_3[data$sex=="female"]== 0 , NA , 1)
table(data$ltf_3[data$sex=="female"],useNA="ifany")



head(data)

tail(data)

data$age_1 <- ifelse(data$ltf_1==1,data$age_1 , NA)
data$age_2 <- ifelse(data$ltf_2==1,data$age_2 , NA)
data$age_3 <- ifelse(data$ltf_3==1,data$age_3 , NA)

data$bmi_1 <- ifelse(data$ltf_1==1,data$bmi_1 , NA)
data$bmi_2 <- ifelse(data$ltf_2==1,data$bmi_2 , NA)
data$bmi_3 <- ifelse(data$ltf_3==1,data$bmi_3 , NA)

#cleaning
data$ltf_1 <- NULL
data$ltf_2 <- NULL
data$ltf_3 <- NULL

# Now you have a so-called wide dataset (each row represents one individual), 
# but for the trajectory analysis, you need to transform it to long format 
# (read more about it in ALDA, p.17, ALDA=Applied Longitudinal Data Analysis). 
# Use the reshape function in R.

data_long <- reshape(data,
                     idvar = "id",
                     varying = list(c("age_0","age_1","age_2","age_3"), c("bmi_0","bmi_1","bmi_2","bmi_3") ),
                     v.names = c("age","bmi"),
                     timevar = "fup",
                     direction = "long")

data_long <- arrange(data_long , id)

# You are ready to fit the first BMI trajectories
#Materials:

# Challenge 4

#  R codes for all chapters of ALDA: http://stats.idre.ucla.edu/r/examples/alda/
#  All materials, including presentations: http://gseacademic.harvard.edu/alda/
#  I uploaded the code that I explained today to our github (codeCI.R).
# Don't hesitate to e-mail me while I am in Malaga. I will follow your progress on github.

# code recycled from Dorte Vistisen

library(nlme) # fits mixed-effects models
library(Epi)

#quadratic and cubic terms of age
data_long$age2 <- data_long$age^2
data_long$age3 <- data_long$age^3


mbmi <- lme(bmi ~  age*sex + age2*sex + age3*sex , random= ~ 1|id, data = data_long, method="ML",na.action=na.omit)
summary(mbmi)
summary(data_long$age)

x <- seq(40,86,by=1)

x.pred1 <- cbind(1,x,0,x^2,x^3,0,0,0)  
x.pred2 <- cbind(1,x,1,x^2,x^3,x,x^2,x^3)

newdata <- rbind(x.pred1, x.pred2)

preddata <- matrix(data = NA, nrow = 0, ncol = 5)  #initialising matrix of predictions

for (j in 1:2){                                                     
  
  index1 <- 1 + (j-1)*(dim(newdata)[1])/2                                 
  index2 <- j*(dim(newdata)[1])/2
  
  y.pred <- newdata[index1:index2,] %*% fixef(mbmi)    #predicted values
  
  y.se <- ci.lin(mbmi,newdata[index1:index2,])[,2]     #std err of the mean
  y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
  y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
  
  temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
  preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

# create data frame object with meaningful column names
preddata <- data.frame(preddata)
names(preddata) <- c("age","bmi","cil","ciu","sex")

#drawing some figures of the results
plot(c(0,0) , main = "bmi trajectorie", xlab = "age" , ylab = "bmi" ,
     xlim = c(40,86),
     ylim = c(24,33),
     )
# add points by sex
points(preddata[preddata$sex==2,c("age","bmi")], col = "blue")
points(preddata[preddata$sex==1,c("age","bmi")], col = "red")
lines(preddata[preddata$sex==2,c("age","bmi")], col = "blue")
lines(preddata[preddata$sex==1,c("age","bmi")], col = "red")
legend(40,32.5, c("Men","Women"), col = c("blue","red"), pch = 1)



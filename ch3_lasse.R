LASSE
#Set the random seed to 123 so that we all get the same results. set.seed(123)
library(Epi)

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

data <- data[c("id" , "sex" , "yob" , "doEnt" , "fup_0", "fup_1" , "fup_2" , "fup_3" , "age_0", "age_1", "age_2", "age_3",
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
table(data$ltf_1[data$sex=="male"])
data$ltf_1[data$sex =="male"] <- ifelse(data$ltf_1[data$sex=="male"]== 0 , NA , 1)

data$ltf_2 <- NA

data$ltf_2[data$sex =="male" & data$ltf_1==1] <- rbinom(sum(data$sex =="male" & data$ltf_1==1) , 1, 7/8)
table(data$ltf_2[data$sex=="male"])

data$ltf_2[data$sex =="male" & data$ltf_1==1] <- ifelse( data$ltf_1[data$sex=="male"] == 1 ,  rbinom(sum(data$sex =="male" & data$ltf_1==1) , 1, 0.7) , 0 )


data$ltf_2 <- ifelse(data$sex =="male" & data$ltf_1==0 , rbinom(N , 1, 0.7) , rbinom(N , 1, 0.9) )
table(data$ltf_2[data$sex=="male"])

head(data)

tail(data)

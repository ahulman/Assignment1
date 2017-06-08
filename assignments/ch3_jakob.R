
set.seed(123)
    N <- 10128
    
    id <- c(1:10128)
    
    
    #creates a sex variable for men and appends women
    sex <- rep.int(0,6021)
    sex <- append(sex, rep.int(1,4107))
    
    #creates dataframe from sex and id vectors
    dataContinuous = data.frame("sex" = sex, "id" = id)
    dataContinuous$fup_0 <- 1990.5+runif(10128,-0.5,0.5)
    dataContinuous$age_0 <- sample(40:60,10128, replace = TRUE)
    
    
    dataCategorical = data.frame(sex, id)
    
    
    ###Analysis
    total <- merge(dataContinuous,dataCategorical, by=c("id","sex"))
    str(total$sex)
    
    total$sex <- factor (total$sex,
                         levels = c(0, 1),
                         labels = c("male", "female"))
    
    
    
#corresponding age
    total$age_1 <- total$age_0 + 5 + runif(10128,-0.5,0.5)
    total$age_2 <- total$age_1 + 5 + runif(10128,-0.5,0.5)
    total$age_3 <- total$age_2 + 5 + runif(10128,-0.5,0.5)
    
    #study starts 1990. Calculate yob
    total$yob <- 1990-total$age_0
    

#calculate BMI at each follow up
    
    total$BMI_fup0 <- ifelse(total$sex == "male", 
            2.04+0.944*total$age_0 -0.008*(total$age_0^2)-0.08*(total$yob -1950) + rnorm(N, 0, 3.5), 
            -14.4 + 1.549*total$age_0 -0.013*((total$age_0)^2)+ 0.08*(total$yob-1950) + rnorm(N, 0, 3.5)
    )
    
    
    total$BMI_fup1 <- ifelse(total$sex=="male", 
           2.04+0.944*total$age_1-0.008*(total$age_1)^2-0.08*(total$yob -1950) + rnorm(N, 0, 3.5), 
           -14.4 + 1.549*total$age_1-0.013*(total$age_1)^2+ 0.08*(total$yob-1950) + rnorm(N, 0, 3.5)
    )            
   
    
    total$BMI_fup2 <- ifelse(total$sex == "male", 
           2.04+0.944*total$age_2-0.008*(total$age_2)^2-0.08*(total$yob -1950) + rnorm(N, 0, 3.5), 
           -14.4 + 1.549*total$age_2-0.013*(total$age_2)^2+ 0.08*(total$yob-1950) + rnorm(N, 0, 3.5)
    )    
    
    total$BMI_fup3 <- ifelse(total$sex == "male", 
           2.04+0.944*total$age_3-0.008*(total$age_3)^2-0.08*(total$yob -1950) + rnorm(N, 0, 3.5), 
           -14.4 + 1.549*total$age_3-0.013*(total$age_3)^2+ 0.08*(total$yob-1950) + rnorm(N, 0, 3.5)
    )   
    
##simulate loss to follow-up----
    #generates noshow1 for men and women
    total$noshow1<- ifelse(total$sex=="male",rbinom(N, 1, 0.8), rbinom(N, 1, 0.9))
    #generates nowshow2 for men and women
    total$noshow2 <- ifelse(total$noshow1 =="0",  0, rbinom(N, 1, ifelse(total$sex=="male", 7/8, 8/9))) 
    
    total$noshow3 <- ifelse(total$sex=="male", 
                            ifelse(total$noshow2 =="0",  0, rbinom(N, 1, 6/7)), 
                            ifelse(total$noshow2 =="0",  0, rbinom(N, 1, 7/8))
                             )       

#changes values to NA if noshow

total$BMI_fup1 <- ifelse(total$noshow1=="0", NA, total$BMI_fup1)
total$age_1 <- ifelse(total$noshow1=="0", NA, total$age_1)   

total$BMI_fup2 <- ifelse(total$noshow2=="0", NA, total$BMI_fup2)
total$age_2 <- ifelse(total$noshow2=="0", NA, total$age_2)   

total$BMI_fup3 <- ifelse(total$noshow3=="0", NA, total$BMI_fup3)
total$age_3 <- ifelse(total$noshow3=="0", NA, total$age_3)   


#reshapes into a longformat version of database

total_long <- reshape(total, direction="long", varying= c(list(4:7),list(9:12)), sep = "_", 
        idvar="id", timevar=c("follow up"))
        
#challenge 4

# code recycled from Dorte Vistisen
library(nlme) # fits mixed-effects models
library(cmprsk)
library(Epi)

#Cuadraticand cuboc terms of age
total_long$age_0 <- as.numeric(total_long$age_0)
total_long$BMI_fup0 <- as.numeric(total_long$BMI_fup0)
total_long$age_squared <-total_long$age_0^2
total_long$age_qubic <-total_long$age_0^3


bmi_traj <- lme(BMI_fup0 ~ age_0*sex  + age_squared*sex  +age_qubic*sex + age_0 + sex, random= ~ 1|id, data = total_long, method="ML", na.action=na.omit)

summary(bmi_traj)
summary(total_long$BMI_fup0)

x <- seq(40,75, by=1)

x.pred1 <- cbind(1,x,0,x^2,x^3,0, 0, 0)  
x.pred2 <- cbind(1,x,1,x^2,x^3,x, x^2,x^3)

newdata <- rbind(x.pred1, x.pred2)

preddata <- matrix(data = NA, nrow = 0, ncol = 5)  #initialising matrix of predictions

for (j in 1:2){                                                     
    
    index1 <- 1 + (j-1)*(dim(newdata)[1])/2                                 
    index2 <- j*(dim(newdata)[1])/2
    
    y.pred <- newdata[index1:index2,] %*% fixef(bmi_traj)    #predicted values
    
    y.se <- ci.lin(bmi_traj,newdata[index1:index2,])[,2]     #std err of the mean
    y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
    y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
    
    temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
    preddata <- rbind(preddata,temp)                    #appending predictions for all groups
}

# create data frame object with meaningful column names
preddata <- data.frame(preddata)
names(preddata) <- c("age","bmi","lcb","ucb","sex")

col1 <- rgb(76, 89, 136, 255, maxColorValue = 255) # blue, Diabetologia style
col2 <- rgb(193, 88, 88, 255, maxColorValue = 255) # red, Diabetologia style
# add points by sex=1
plot(preddata[preddata$sex=="1", c("age", "bmi")], col = col1, xlab = "Age (year)", ylab = "BMI (kg/m2)", title("BMI vs. Age for men and women"))
lines(preddata[preddata$sex=="1", c("age", "bmi")], col = col1)
lines(preddata[preddata$sex=="1", c("age", "ucb")], col = col1)
lines(preddata[preddata$sex=="1", c("age", "lcb")], col = col1)
#adds text
mtext("men", col = col1, line = -5)
mtext("women", col = col2, line = -6)
# add points by sex=2
points(preddata[preddata$sex=="2", c("age", "bmi")], col = col2)
lines(preddata[preddata$sex=="2", c("age", "bmi")], col = col2)
lines(preddata[preddata$sex=="2", c("age", "ucb")], col = col2)
lines(preddata[preddata$sex=="2", c("age", "lcb")], col = col2)

#preddata$sex <- factor(preddata$sex,
                         #   levels = c(1, 2),
                          #  labels = c("male","female"))
#class(preddata$sex)
qplot(age, bmi, ucb, lcb, data = preddata, facets = sex )

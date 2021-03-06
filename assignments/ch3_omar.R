## Challenge 3
getwd()

set.seed(123)

N <- 1:10128

id <- c (1:10128)

sex <-  c(rep.int(0, 6021), (rep.int(1, 4107))) 

ch3_df <-data.frame(id, sex)

ch3_df$age_0 <- runif(N, min = 40, max = 70) 

ch3_df$fup_0 <- 0

ch3_df$n <- 1:3

ch3_df$u <- runif(N, min = -0.5, max = 0.5) 

ch3_df$fup_1 <- ch3_df$fup_0 + 5 + ch3_df$u

ch3_df$fup_2 <- ch3_df$fup_0 + 10 + ch3_df$u

ch3_df$fup_3 <- ch3_df$fup_0 + 15 + ch3_df$u

#Age
ch3_df$age_1 <- ch3_df$age_0 +  ch3_df$fup_1

ch3_df$age_2 <- ch3_df$age_0 +  ch3_df$fup_2

ch3_df$age_3 <- ch3_df$age_0 +  ch3_df$fup_3

#Year of birth

ch3_df$exam_yr <- runif(N,min=1990, max=1991)

ch3_df$yob <- ch3_df$exam_yr - ch3_df$age_0

# phase specific BMI 

ch3_df$bmi_0 <- ifelse(sex==0, 2.04 + 0.944 * ch3_df$age_0 - 0.008 * ch3_df$age_0^2 - 0.08 * (ch3_df$yob - 1950) + rnorm (N, 0, 3.5), 
                      -14.4 + 1.549 * ch3_df$age_0 - 0.013 * ch3_df$age_0^2 + 0.08 * (ch3_df$yob - 1950) + rnorm(N, 0, 3.5))

ch3_df$bmi_1 <- ifelse(sex==0, 2.04 + 0.944 * ch3_df$age_1 - 0.008 * ch3_df$age_1^2 - 0.08 * (ch3_df$yob - 1950) + rnorm (N, 0, 3.5), 
                      -14.4 + 1.549 * ch3_df$age_1 - 0.013 * ch3_df$age_1^2 + 0.08 * (ch3_df$yob - 1950) + rnorm(N, 0, 3.5))

ch3_df$bmi_2 <- ifelse(sex==0, 2.04 + 0.944 * ch3_df$age_2 - 0.008 * ch3_df$age_2^2 - 0.08 * (ch3_df$yob - 1950) + rnorm (N, 0, 3.5), 
                      -14.4 + 1.549 * ch3_df$age_2 - 0.013 * ch3_df$age_2^2 + 0.08 * (ch3_df$yob - 1950) + rnorm(N, 0, 3.5))

ch3_df$bmi_3 <- ifelse(sex==0, 2.04 + 0.944 * ch3_df$age_3 - 0.008 * ch3_df$age_3^2 - 0.08 * (ch3_df$yob - 1950) + rnorm (N, 0, 3.5), 
                      -14.4 + 1.549 * ch3_df$age_3 - 0.013 * ch3_df$age_3^2 + 0.08 * (ch3_df$yob - 1950) + rnorm(N, 0, 3.5))

#Lost to follow-up identifyed with 0
ch3_df$fup_3_miss <- rbinom (N, 1, ifelse(ch3_df$sex==0, 0.60, 0.70))
ch3_df$fup_2_miss <- ifelse(ch3_df$fup_3_miss==0, rbinom(N, 1, ifelse(ch3_df$sex==0, 10/40, 10/30)), 1)
ch3_df$fup_1_miss <- ifelse(ch3_df$fup_2_miss==0, rbinom(N, 1, ifelse(ch3_df$sex==0, 10/30, 10/20)), 1)

ch3_df[1:100,c("fup_1_miss","fup_2_miss","fup_3_miss")]
table(ch3_df$fup_1_miss)
table(ch3_df$fup_2_miss)
table(ch3_df$fup_3_miss)


#BMI and age specific follow-up to missing
ch3_df$bmi_1 <- ifelse(ch3_df$fup_1_miss==0, ch3_df$bmi_1==NA, ch3_df$bmi_1)
ch3_df$bmi_2 <- ifelse(ch3_df$fup_1_miss==0, ch3_df$bmi_2==NA, ch3_df$bmi_2)
ch3_df$bmi_3 <- ifelse(ch3_df$fup_1_miss==0, ch3_df$bmi_3==NA, ch3_df$bmi_3)


ch3_df$age_1 <- ifelse(ch3_df$fup_1_miss == 0, ch3_df$age_1 == NA, ch3_df$age_1)
ch3_df$age_2 <- ifelse(ch3_df$fup_1_miss == 0, ch3_df$age_2 == NA, ch3_df$age_2)
ch3_df$age_3 <- ifelse(ch3_df$fup_1_miss == 0, ch3_df$age_3 == NA, ch3_df$age_3)

#Change follow up 0 to missing
#ch3_df$fup_3_miss <- ifelse(ch3_df$fup_1_miss==0, ch3_df$fup_3_miss==NA, ch3_df$fup_3_miss)
#ch3_df$fup_2_miss <- ifelse(ch3_df$fup_1_miss==0, ch3_df$fup_2_miss==NA, ch3_df$fup_2_miss)
#ch3_df$fup_3_miss <- ifelse(ch3_df$fup_2_miss==0, ch3_df$fup_3_miss==NA, ch3_df$fup_3_miss)
#ch3_df$fup_1_miss[ch3_df$fup_1_miss == 0 & is.numeric(ch3_df$fup_1_miss)] <- NA
#ch3_df$fup_2_miss[ch3_df$fup_2_miss == 0 & is.numeric(ch3_df$fup_2_miss)] <- NA
#ch3_df$fup_3_miss[ch3_df$fup_3_miss == 0 & is.numeric(ch3_df$fup_3_miss)] <- NA

#Reshape
ch3_long <- reshape(ch3_df, idvar="id", direction="long", varying= c("age_0", "age_1", "age_2", "age_3", "bmi_0", "bmi_1", "bmi_2", "bmi_3"), sep = "_", 
                    timevar= c("fup"), drop = c("fup_0", "fup_1", "fup_2", "fup_3", "fup_1_miss", "fup_2_miss", "fup_3_miss", "n", "u", "exam_yr", "yob"))

#challenge 4

# code recycled from Dorte Vistisen
library(nlme)
library(Epi)

#Cuadraticand cuboc terms of age
ch3_long$age2 <-ch3_long$age^2
ch3_long$age3 <-ch3_long$age^3


bmi_traj <- lme(bmi ~ age*sex  + age2*sex + age3*sex , random= ~ 1|id, data = ch3_long, method="ML", na.action=na.omit)

summary(bmi_traj)
summary(ch3_long$age)
summary(ch3_long$bmi)
x <- seq(40,86, by=1)

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

#Figure
col1 <- rgb(76, 89, 136, 255, maxColorValue = 255) # blue, Diabetologia style
col2 <- rgb(193, 88, 88, 255, maxColorValue = 255) # red, Diabetologia style

plot(c(0,0), xlab = "Age (year)", ylab = "BMI (kg/m2)", xlim = c(40,90), ylim = c(20,32), xaxt="n",yaxt="n",bty = "n")

# define x and y axis
axis(1, at = seq(0, 87, 5))
axis(2,at = seq(0, 40,2), las=1, cex=2)

# add points by sex
lines(preddata[preddata$sex=="1",c("age","bmi")], col = col1)
lines(preddata[preddata$sex=="2",c("age","bmi")], col = col2)
legend(40, 22, c("Men","Women"), col = c(col1,col2), pch = 1)


# CIs
for (i in 1:2) {
  lines(preddata$age[preddata$sex==i],
        preddata$lcb[preddata$sex==i],
        lwd=1,lty=2,col=col[i])
  lines(preddata$age[preddata$sex==i],
        preddata$ucb[preddata$sex==i],
        lwd=1,lty=2,col=col[i])
}

lines(preddata$age[preddata$sex==1],
      preddata$lcb[preddata$sex==1],
      lwd=1,lty=2,col=col1)
lines(preddata$age[preddata$sex==1],
      preddata$ucb[preddata$sex==1],
      lwd=1,lty=2,col=col1)

lines(preddata$age[preddata$sex==2],
      preddata$lcb[preddata$sex==2],
      lwd=1,lty=2,col=col2)
lines(preddata$age[preddata$sex==2],
      preddata$ucb[preddata$sex==2],
      lwd=1,lty=2,col=col2)

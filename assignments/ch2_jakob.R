###Creates SimCohort

simCohort <- function(N,S){
    set.seed(S)
    ###Jakob

       
    id <- c(1:N)

    
    #creates a sex variable for first 600 and appends last 400
    sex <- rep.int(0,N*6/10)
    sex <- append(sex, rep.int(1,N*4/10))
    
    #creates dataframe from SEX and ID vectors
    dataContinuous = data.frame("sex" = sex, "id" = id)
    
    dataContinuous$age <- runif(N, min = 40, max = 70)
    

    dataContinuous$bmi <- ifelse(dataContinuous$sex==0, 21 + 0.1*dataContinuous$age + rnorm(N, 0 , 2), 20 + 0.15*dataContinuous$age + rnorm(N, 0 , 2.5))
    
        ###OMAR

    dataContinuous$bmi <- ifelse(dataContinuous$sex==0, 21 + 0.1*dataContinuous$age + rnorm(N, 0 , 2), 20 + 0.15*dataContinuous$age + rnorm(N, 0 , 2.5))
    ###OMAR

    #Output data frame
    dataCategorical = data.frame(sex, id)
    
    #Generate ethnicity
    dataCategorical$ethnic <-rbinom(N, 1, 0.05)
    
    #Generate smoking status
    smoke <- c(0, 1, 2)
    
    dataCategorical$smoke <- ifelse(dataCategorical$sex == 0, sample(smoke, N, replace = TRUE, prob = c(0.5, 0.3, 0.2)), sample(smoke, N, replace = TRUE, prob = c(0.6, 0.3, 0.1)))    

    dataCategorical$smoke <- ifelse(dataCategorical$sex == 0, sample(smoke, N, replace = TRUE, prob = c(0.5, 0.3, 0.2)), sample(smoke, N, replace = TRUE, prob = c(0.6, 0.3, 0.1)))
    
    dataCategorical$smoke <- factor (dataCategorical$smoke,
                                     levels = c(0, 1, 2),
                                     labels = c("never", "ex", "current"))
    ###Analysis
    total <- merge(dataContinuous,dataCategorical, by=c("id","sex"))
    
    #change numeric storage type to factor
    
    total$ethnic <- factor (total$ethnic,
                            levels = c(0, 1),
                            labels = c("non-white", "white"))
    
    total$sex <- factor (total$sex,
                         levels = c(0, 1),
                         labels = c("male", "female"))
    
    return(total)
}
#creates sample cohorts

    simCohort(20000,1)
    
    
    cohort1 <- simCohort(100, 123)
    
    cohort2 <- simCohort(10000, 987)


#plots associations to examine

#plots associations

    plot(cohort1$age,cohort1$bmi)
    plot(cohort1$sex,cohort1$bmi)
    
    plot(cohort2$age,cohort2$bmi)
    plot(cohort2$sex,cohort2$bmi)



# cohort1 -----------------------------------------------------------------


#creates linear models with single explanatory variable
    
    bmi.mod <- lm(bmi ~ age, cohort1)
    plot(cohort1$age,cohort1$bmi)
    mean.bmi <- mean(cohort1$bmi)
    abline(bmi.mod, col="red")
    abline(h=mean.bmi, col="blue")
    summary(bmi.mod)

    bmi.mod <- lm(bmi ~ sex, cohort1)
    
    bmi.mod <- lm(bmi ~ age, cohort2)
    plot(cohort2$age,cohort2$bmi)
    mean.bmi <- mean(cohort2$bmi)
    abline(bmi.mod, col="red")
    abline(h=mean.bmi, col="blue")
    summary(bmi.mod)
#blue line indicate null-hypothesis, positiv slope indicates age affects BMI

##this plot indicates sex influences BMI
    plot(cohort2$sex,cohort2$bmi)

##linear models with multiple explanatory variables

#plots the BMI by explanatory variables from cohort1
    coplot(bmi~age|sex,cohort1)
#This plot indicates that sex affects the intersection of BMI, let's examine the slope for men and women:
    summary(lm(bmi~age, sex == "male", data=cohort1))
    summary(lm(bmi~age, sex == "female", data=cohort1))
#from the coeefficiens it appears that for men an increase of 1 year increases BMI with 0,09 abd for women it's 0,11
#The intersects are different indicating a difference in bmi at the same age


# Cohort2----
#plots the BMI by explanatory variables from cohort2
    coplot(bmi~age|sex,cohort2)

#This plot indicates that sex affects atleast the deviation, let's examine the slope for men and women:
    summary(lm(bmi~age, sex == "male", data=cohort2))
    summary(lm(bmi~age, sex == "female", data=cohort2))

## the second cohort a closer to the true slope value, representing a a larger sample size. It is also
#noted a larger standard error for women

#multiple linear regression, notice the difference in slope and intersection. Cohort2 converges towards the true value
#of bmi at year 40 (know from the equation)

    bmiMultiRegression <- lm(bmi~age*sex, cohort1)
    bmiMultiRegression
    plot(cohort1$age, fitted(bmiMultiRegression))
    
    bmiMultiRegression <- lm(bmi~age*sex, cohort2)
    bmiMultiRegression
    plot(cohort2$age, fitted(bmiMultiRegression))
#notice that for females, the age effect is increased by 0,05 per year. Being female however reduces BMI with 0,9

#creates linear models
    bmi.mod <- lm(formula = bmi ~ age, cohort1)
    bmi.mod

    bmi.mod <- lm(formula = bmi ~ sex, cohort1)
    bmi.mod
    plot(bmi.mod)


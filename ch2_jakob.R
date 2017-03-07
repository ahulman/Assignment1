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
    
    dataContinuous$bmi <- ifelse(dataContinuous$sex==0, 21 + 0.1*dataContinuous$age + rnorm(1, 0 , 2), 20 + 0.15*dataContinuous$age + rnorm(1, 0 , 2.5))
    ###OMAR
    #Output data frame
    dataCategorical = data.frame(sex, id)
    
    #Generate ethnicity
    dataCategorical$ethnic <-rbinom(N, 1, 0.05)
    
    #Generate smoking status
    smoke <- c(0, 1, 2)
    
    dataCategorical$smoke <- ifelse(dataCategorical$sex == 0, sample(smoke, 1, replace = TRUE, prob = c(0.5, 0.3, 0.2)), sample(smoke, 1, replace = TRUE, prob = c(0.6, 0.3, 0.1)))
    
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

#plots associations
plot(cohort1$age,cohort1$bmi)
plot(cohort1$sex,cohort1$bmi)

plot(cohort2$age,cohort2$bmi)
plot(cohort2$sex,cohort2$bmi)

#creates linear models
bmi.mod <- lm(formula = bmi ~ age, cohort1)
bmi.mod

bmi.mod <- lm(formula = bmi ~ sex, cohort1)
bmi.mod
plot(bmi.mod)


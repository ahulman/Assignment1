#creating the function: simCohort

simCohort <- function (N,S) {
    id <- c(1:N)
    set.seed(S)
    sex <- c(rep.int(0, N*6/10), (rep.int(1, N*4/10)))
    
    total <- data.frame(sex, id)
    total$sex <- factor (total$sex,
                         levels = c(0, 1),
                         labels = c("male", "female"))
    
    total$ethnic <-rbinom(N, 1, 0.05)
    total$ethnic <- factor (total$ethnic,
                            levels = c(0, 1),
                            labels = c("non-white", "white"))
    
    smokevalues <- c(0, 1, 2)
    total$smoke <- ifelse(total$sex == 0, sample(smokevalues, 600, replace = TRUE, prob = c(0.5, 0.3, 0.2)), sample(smokevalues, 400, replace = TRUE, prob = c(0.6, 0.3, 0.1)))
    
    total$smoke <- factor (total$smoke,
                                     levels = c(0, 1, 2),
                                     labels = c("never", "ex", "current"))
    
    total$age <-runif(N, min = 40, max = 70) 
    total$bmi <- ifelse(total$sex=="male", 21 + 0.1*total$age + rnorm(N, 0 , 2), 
                       20 + 0.15*total$age + rnorm(N, 0 , 2.5))

    return(total)
    }

cohort1 <- simCohort(100,123)
cohort2 <- simCohort(10000 , 987)

#there are six variables as in the total dataframe, so all the variables
#are create int the function

# Examine the association between age and sex (exposures) and bmi (continuous outcome). Use the lm function.
model1_cohort1 <- lm(bmi ~ age + sex , data=cohort1)
summary(model1_cohort1)

model1_cohort2 <- lm(bmi ~ age + sex , data=cohort2)
summary(model1_cohort2)

library(Epi)
ci.lin(model1_cohort1)
ci.lin(model1_cohort2)

#using glm:
glm_cohort1 <- glm(bmi~age + sex , data=cohort1 , family=gaussian)
summary(glm_cohort1)

#Examine whether sex modifies the effect of age on bmi. Include an interaction between age and sex in the model.
model2_cohort1 <- lm(bmi ~ age * sex , data=cohort1)
summary(model2_cohort1)

model2_cohort2 <- lm(bmi ~ age * sex , data=cohort2)
summary(model2_cohort2)

plot(model1_cohort2)


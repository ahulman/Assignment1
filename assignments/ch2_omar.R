##CHALLENGE 2
    # WRITE A FUNCTION CALLED *simCohort that takes two parameters: *N* (number of individuals in the cohort) and 
    # *seed* (seed for the random number generation).

simCohort <- function (N, seed) {
            set.seed(seed)
            id <-c(1:N)
            sex <-c(rep.int(0, N*6/10), (rep.int(1, N*4/10)))
            data <-data.frame(sex, id)
            data$sex <- factor (data$sex,
                                 levels = c(0, 1),
                                 labels = c("male", "female"))
            data$ethnic <-rbinom(N, 1, 0.05)
            data$ethnic <- factor (data$ethnic,
                                    levels = c(0, 1),
                                    labels = c("non-white", "white"))
            data$age <-runif(N, min = 40, max = 70) 
            data$bmi <- ifelse(data$sex=="male", 21 + 0.1*data$age + rnorm(N, 0 , 2), 
                              20 + 0.15*data$age + rnorm(N, 0 , 2.5))
            smokevalues <-c(0, 1, 2) 
            data$smoke <- ifelse(data$sex == 0, sample(smokevalues, 600, replace = TRUE, prob = c(0.5, 0.3, 0.2)), 
                                 sample(smokevalues, 400, replace = T, prob = c(0.6, 0.3, 0.1)))
            data$smoke <- factor (data$smoke,
                                             levels = c(0, 1, 2),
                                             labels = c("never", "ex", "current"))
            data
}

    #Simulate 2 cohorts
cohort1 <- simCohort(100, 123)

    #Display the first part of data frame
head(cohort1)

cohort2 <- simCohort(10000, 987)

head(cohort2)

    #Examine the association between age and sex (exposures) and bmi (continuous outcome). Use the *lm* function

        #Age and sex adjusted linear regression models
model1_c1 <- lm(bmi ~ sex+age, cohort1)

model1_c2 <- lm(bmi ~ sex+age, cohort2)    

        #Age and sex adjusted linear regression models including an age*sex interaction term

model2_c1 <- lm(bmi ~ sex+age + sex:age, cohort1)

model2_c2 <- lm(bmi ~ sex*age, cohort2)

        #Explore differences between models
summary (model1_c1)
summary (model2_c1)
summary (model1_c2)
summary (model2_c2)

#Bonus exercise. Scatterplot wih the regression line
plot(model1_c1)
plot(model2_c1)
plot(model1_c2)
plot(model2_c2)


#To display 95% CI, install epi package
#install.packages(Epi)
#library(Epi)

#ci.lin(model1_c1)

#To make aesthetics figures install ggplot2 package
#install.packages("ggplot2")
#library(ggplot2)



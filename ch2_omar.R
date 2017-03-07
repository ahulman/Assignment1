
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
            data$bmi <- ifelse(data$sex==0, 21 + 0.1*data$age + rnorm(1, 0 , 2), 
                              20 + 0.15*data$age + rnorm(1, 0 , 2.5))
            smokevalues <-c(0, 1, 2) 
            data$smoke <- ifelse(data$sex == 0, sample(data$smokevalues, 1, replace = TRUE, prob = c(0.5, 0.3, 0.2)), 
                                 sample(data$smokevalues, 1, replace = 1, prob = c(0.6, 0.3, 0.1)))
            data$smoke <- factor (dataCategorical$smoke,
                                             levels = c(0, 1, 2),
                                             labels = c("never", "ex", "current"))
            data
}

cohort1 <- simCohort(100, 123)

head(cohort1)

cohort2 <- simCohort(10000, 987)


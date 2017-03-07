
simCohort <- function (N, seed) {
            set.seed(seed)
            id <-c(1:N)
            sex <- c(rep.int(0, N*6/10), (rep.int(1, N*4/10)))
            data <- data.frame(sex, id)
            data$ethnic <- 
            data$age <- 
            data$smoke <- 
            data$bmi <- 
            data
        }

cohort1 <- simCohort(100, 123)

cohort2 <- simCohort(10000, 987)

source('ch1_analysis.R')

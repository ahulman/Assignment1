## Categorical variables

#Set the random numbers to choose the same sample of individuals
set.seed(1)

#Generate ID
id <-c(1:N)

#Generate sex
sex <- c(rep.int(0, N*6/10), (rep.int(1, N*4/10)))

#Output data frame
dataCategorical = data.frame(sex, id)

#Generate ethnicity
dataCategorical$ethnic <-rbinom(N, 1, 0.05)

#Generate smoking status
smoke <- c(0, 1, 2)

dataCategorical$smoke <- ifelse(dataCategorical$sex == 0, sample(smoke, 0.6*N, replace = TRUE, prob = c(0.5, 0.3, 0.2)), sample(smoke, 0.4*N, replace = TRUE, prob = c(0.6, 0.3, 0.1)))

dataCategorical$smoke <- factor (dataCategorical$smoke,
                   levels = c(0, 1, 2),
                   labels = c("never", "ex", "current"))




#creates a cohort of size n. Fill in n
N <- 10000

# creates vector with ID 1-1000
ID <- c(1:N)

#creates a sex variable for first 600 and appends last 400
SEX <- rep.int(0,N*6/10)
SEX <- append(SEX, rep.int(1,N*4/10))

#creates dataframe from SEX and ID vectors
dataContinuous <- data.frame("sex" = SEX, "id" = ID)
set.seed(1)
 
dataContinuous$age <- runif(N, min = 40, max = 70)

dataContinuous$bmi <- ifelse(dataContinuous$sex==0, 21 + 0.1*dataContinuous$age + rnorm(1, 0 , 2), 20 + 0.15*dataContinuous$age + rnorm(1, 0 , 2.5))

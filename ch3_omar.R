## Challenge 3

set.seed(123)

N <-1:10128

id <- c(1:N)

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

ch3_df$bmi0 <- ifelse(sex==0, 2.04 + 0.944 * ch3_df$age_0 - 0.008 * ch3_df$age_0^2 - 0.08 * (ch3_df$yob - 1950) + rnorm (N, 0, 3.5), 
                      -14.4 + 1.549 * ch3_df$age_0 - 0.013 * ch3_df$age_0^2 + 0.08 * (ch3_df$yob - 1950) + rnorm(N, 0, 3.5))

ch3_df$bmi1 <- ifelse(sex==0, 2.04 + 0.944 * ch3_df$age_1 - 0.008 * ch3_df$age_1^2 - 0.08 * (ch3_df$yob - 1950) + rnorm (N, 0, 3.5), 
                      -14.4 + 1.549 * ch3_df$age_1 - 0.013 * ch3_df$age_1^2 + 0.08 * (ch3_df$yob - 1950) + rnorm(N, 0, 3.5))

ch3_df$bmi2 <- ifelse(sex==0, 2.04 + 0.944 * ch3_df$age_2 - 0.008 * ch3_df$age_2^2 - 0.08 * (ch3_df$yob - 1950) + rnorm (N, 0, 3.5), 
                      -14.4 + 1.549 * ch3_df$age_2 - 0.013 * ch3_df$age_2^2 + 0.08 * (ch3_df$yob - 1950) + rnorm(N, 0, 3.5))

ch3_df$bmi3 <- ifelse(sex==0, 2.04 + 0.944 * ch3_df$age_3 - 0.008 * ch3_df$age_3^2 - 0.08 * (ch3_df$yob - 1950) + rnorm (N, 0, 3.5), 
                      -14.4 + 1.549 * ch3_df$age_3 - 0.013 * ch3_df$age_3^2 + 0.08 * (ch3_df$yob - 1950) + rnorm(N, 0, 3.5))


#Lost to follow-up
ch3_df$fup_1_miss <- ifelse(sex==0, rbinom (N, 1, 0.80), rbinom (N, 1, 0.90))
ch3_df$fup_2_miss  <- ifelse(sex==0 & ch3_df$fup_1_miss==0, rbinom (N, 1, 0.70), rbinom (N, 1, 0.80))
ch3_df$fup_3_miss  <- ifelse(sex==0 & ch3_df$fup_1_miss==0 & ch3_df$fup_2_miss==0, rbinom (N, 1, 0.60), rbinom (N, 1, 0.70))




#BMI and age specific follow-up to missing
ch3_df$bmi1 <- ifelse(ch3_df$fup_1_miss == 0, ch3_df$bmi1== NA, ch3_df$bmi1)
ch3_df$bmi2 <- ifelse(ch3_df$fup_2_miss == 0, ch3_df$bmi2== NA, ch3_df$bmi2)
ch3_df$bmi3 <- ifelse(ch3_df$fup_3_miss == 0, ch3_df$bmi3== NA, ch3_df$bmi3)

ch3_df$age_1 <- ifelse(ch3_df$fup_1_miss == 0, ch3_df$age_1== NA, ch3_df$age_1)
ch3_df$age_2 <- ifelse(ch3_df$fup_2_miss == 0, ch3_df$age_2== NA, ch3_df$age_2)
ch3_df$age_3 <- ifelse(ch3_df$fup_3_miss == 0, ch3_df$age_3== NA, ch3_df$age_3)

#Change 0 to missing values
ch3_df$fup_1_miss[ch3_df$fup_1_miss == 0 & is.numeric(ch3_df$fup_1_miss)] <- NA
ch3_df$fup_2_miss[ch3_df$fup_2_miss == 0 & is.numeric(ch3_df$fup_2_miss)] <- NA
ch3_df$fup_3_miss[ch3_df$fup_3_miss == 0 & is.numeric(ch3_df$fup_3_miss)] <- NA


table()
summary(ch3_df$bmi1)

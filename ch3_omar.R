## Challenge 3

set.seed(123)

N <-1:10128

id <- c(1:N)

sex <-  c(rep.int(0, 6021), (rep.int(1, 4107))) 

ch3_df <-data(id, sex)

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

#Date of examination

ch3_df$exam_yr <- runif(N,min=1990, max=1991)



ch3_df$yob_num <- ch3_df$exam_yr - ch3_df$age_0

ch3_df$yob <- as.Date(ch3_df$yob_num, format= "%Y")

************************************************************
ch3_df$exam_yr2 <- as.Date(ch3_df$exam_yr, format="%Y")

ch3_df$exam_date <- as.Date(ch3_df$exam_yr, format= "%Y")


ch3_df$exam_yr <- as.numeric(ch3_df$exam_date)


#library(lubridate)

#Date of birth
ch3_df$exam_date_num <-  ymd("ch3_df$exam_date")


ch3_df$yob_num2 <-as.character(ch3_df$yob_num)


yob <- Assuming that the study started in 1990 (create an examination "date" for everyone from a uniform distribution between 1990 and 1991) and calculate year of birth for everyone (called it *yob*).

ch3_df$date

table(ch3_df$yob)
summary(ch3_df$yob_num)

##set Wd

#source ch1_simContinuous and ch1_simCategorical

source('ch1_simContinuous.R')
source('ch1_simCategorical.R')

# merge two data frames by ID, 
total <- merge(dataContinuous,dataCategorical, by=c("id","sex"))

#change numeric storage type to factor

total$ethnic <- factor (total$ethnic,
                 levels = c(0, 1),
                 labels = c("non-white", "white"))

total$sex <- factor (total$sex,
                        levels = c(0, 1),
                        labels = c("male", "female"))

#make summary of continuous variables use tapply summary Calculate descriptive statistics overall (e.g. summary function) and by sex (summary within a tapply) using the summary/tapply and table/prop.table functions for continuous and categorical variables, respectively.

#* Median (Q1-Q3) for continuous variables
#* Frequency (%) for categorical variables

summary(total)

#Descriptive statistics stratified by sex
tapply(total$age, total$sex, summary)

tapply(total$bmi, total$sex, summary)

prop.table(table(total$sex, total$ethnic), margin = 1)

prop.table(table(total$sex, total$smoke), margin = 1)

tapply(total$age, total$sex, hist)


tapply(total$age, total$sex, summary)

tapply(total$bmi, total$sex, summary)

prop.table(table(total$sex, total$ethnic), margin = 1)

prop.table(table(total$sex, total$smoke), margin = 1)

tapply(total$age, total$sex, hist)



set.seed(123)
    N <- 10128
    
    id <- c(1:10128)
    
    
    #creates a sex variable for men and appends women
    sex <- rep.int(0,6021)
    sex <- append(sex, rep.int(1,4107))
    
    #creates dataframe from sex and id vectors
    dataContinuous = data.frame("sex" = sex, "id" = id)
    dataContinuous$fup_0 <- 1990.5+runif(10128,-0.5,0.5)
    dataContinuous$age_0 <- sample(40:60,10128, replace = TRUE)
    
    
    dataCategorical = data.frame(sex, id)
    
    
    ###Analysis
    total <- merge(dataContinuous,dataCategorical, by=c("id","sex"))
    str(total$sex)
    
    total$sex <- factor (total$sex,
                         levels = c(0, 1),
                         labels = c("male", "female"))
    
    
    
#corresponding age
    total$age_1 <- total$age_0 + 5 + runif(10128,-0.5,0.5)
    total$age_2 <- total$age_1 + 5 + runif(10128,-0.5,0.5)
    total$age_3 <- total$age_2 + 5 + runif(10128,-0.5,0.5)
    
    #study starts 1990. Calculate yob
    total$yob <- 1990-total$age_0
    

#calculate BMI at each follow up
    
    total$BMI_fup0 <- ifelse(total$sex == "male", 
            2.04+0.944*total$age_0 -0.008*(total$age_0^2)-0.08*(total$yob -1950) + rnorm(N, 0, 3.5), 
            -14.4 + 1.549*total$age_0 -0.013*((total$age_0)^2)+ 0.08*(total$yob-1950) + rnorm(N, 0, 3.5)
    )
    
    
    total$BMI_fup1 <- ifelse(total$sex=="male", 
           2.04+0.944*total$age_1-0.008*(total$age_1)^2-0.08*(total$yob -1950) + rnorm(N, 0, 3.5), 
           14.4 + 1.549*total$age_1-0.013*(total$age_1)^2+ 0.08*(total$yob-1950) + rnorm(N, 0, 3.5)
    )            
   
    
    total$BMI_fup2 <- ifelse(total$sex == "male", 
           2.04+0.944*total$age_2-0.008*(total$age_2)^2-0.08*(total$yob -1950) + rnorm(N, 0, 3.5), 
           14.4 + 1.549*total$age_2-0.013*(total$age_2)^2+ 0.08*(total$yob-1950) + rnorm(N, 0, 3.5)
    )    
    
    total$BMI_fup3 <- ifelse(total$sex == "male", 
           2.04+0.944*total$age_3-0.008*(total$age_3)^2-0.08*(total$yob -1950) + rnorm(N, 0, 3.5), 
           14.4 + 1.549*total$age_3-0.013*(total$age_3)^2+ 0.08*(total$yob-1950) + rnorm(N, 0, 3.5)
    )   
    
#simulate loss to follow-up
    #generates noshow1 for men and women
    total$noshow1<- ifelse(total$sex=="male",rbinom(N, 1, 0.8), rbinom(N, 1, 0.9))
    #generates nowshow2 for men and women
    total$noshow2 <- ifelse(total$sex=="male", 
                            ifelse(total$noshow1 =="0",  0, rbinom(N, 1, 7/8)), 
                            ifelse(total$noshow1 =="0",  0, rbinom(N, 1, 8/9))
                            )
    
    total$noshow3 <- ifelse(total$sex=="male", 
                            ifelse(total$noshow2 =="0",  0, rbinom(N, 1, 6/7)), 
                            ifelse(total$noshow2 =="0",  0, rbinom(N, 1, 7/8))
                             )       
#changes values to NA if noshow

total$BMI_fup1 <- ifelse(total$noshow1=="0", "NA", total$BMI_fup1)
total$age_1 <- ifelse(total$noshow1=="0", "NA", total$age_1)   

total$BMI_fup2 <- ifelse(total$noshow2=="0", "NA", total$BMI_fup2)
total$age_2 <- ifelse(total$noshow2=="0", "NA", total$age_2)   

total$BMI_fup3 <- ifelse(total$noshow3=="0", "NA", total$BMI_fup3)
total$age_3 <- ifelse(total$noshow3=="0", "NA", total$age_3)   


#reshapes a longformat version of database

total_long <- reshape(total, direction="long", varying= c(list(4:7),list(9:12)), sep = "_", 
        idvar="id", timevar=c("follow up"))
        


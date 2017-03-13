
set.seed(123)

    
    
    
    id <- c(1:10128)
    
    
    #creates a sex variable for men and appends women
    sex <- rep.int(0,6021)
    sex <- append(sex, rep.int(1,4107))
    
    #creates dataframe from sex and id vectors
    dataContinuous = data.frame("sex" = sex, "id" = id)
    
    dataContinuous$age <- runif(10128, min = 40, max = 60)
    
    
    dataCategorical = data.frame(sex, id)
    
    
    ###Analysis
    total <- merge(dataContinuous,dataCategorical, by=c("id","sex"))
    str(total$sex)
    
    total$sex <- factor (total$sex,
                         levels = c(0, 1),
                         labels = c("male", "female"))
#follow at 0,5,10, 15 years
    total$fup_0 <- 0
    total$fup_1 <- total$fup_0 + 5 + runif(10128,-0.5,0.5)
    total$fup_2 <- total$fup_0 + 10 + runif(10128,-0.5,0.5)
    total$fup_3 <- total$fup_0 + 15 + runif(10128,-0.5,0.5)
#corresponding age
    total$age1 <- total$age+ total$fup_1
    total$age2 <- total$age+ total$fup_2
    total$age3 <- total$age+ total$fup_3

#study starts 1990. Calculate yob
    total$yob <- 1990
    total$yob <- as.Date(total$yob)
    
    
rm(list=ls())#clear environment
set.seed(1234)
N <- 750000

id <- c(1:N)

#log(x) curve

par(mfrow=c(1,1), oma=c(0,0,0,0))
curve(log10(x), from = 1, to = 10, 
      main = "Guidelines Adherence",
      xlab = "effort", 
      ylab = "proportion adhering to guidelines",
      xaxt = "n")
points(x = 10^0.5, y = 0.5, pch=0)
points(x = 10^0.6, y = 0.6, pch=6)
points(x = 10^0.7, y = 0.7, pch=8)



#creates a sex variable for men and appends women
treated <- rep.int(0,125000)
treated <- append(treated, rep.int(1,125000))
treated <- append(treated, rep.int(0,100000))
treated <- append(treated, rep.int(1,150000))
treated <- append(treated, rep.int(0,75000))
treated <- append(treated, rep.int(1,175000))

groupname <- rep.int(1,250000)
groupname <- c(groupname, rep.int(2,250000))
groupname <- c(groupname, rep.int(3,250000))                

#creates dataframe from sex and id vectors

data = data.frame(treated, id, groupname)
class(data$treated)
data$treated <- factor(data$treated, levels = c(0,1), labels = c("untreated","treated"))
data$groupname <- factor(data$groupname, levels = c(1,2,3), labels = c("group 1", "group 2", "group 3"))

#generates death to overall  
data$year_0 <- 1

for (i in 1:40){ 
    if (i == 1){
        n <- ifelse(data$treated=="treated", rbinom(N, 1, 1-0.035/48), rbinom(N, 1, 1-0.05/48))
    } 
    else {
        n <- ifelse(data$treated=="treated", 
                    ifelse(data[[paste0("year_", i-1)]] == 0,  0, rbinom(N, 1, 1-0.035/48)), 
                    ifelse(data[[paste0("year_", i-1)]] == 0,  0, rbinom(N, 1, 1-0.05/48))
        )
    }
    data[[paste0("year_", i)]] <- n 
}
gc() #clears memory

data_long <- reshape(data, direction="long", varying= c(list(4:44)), sep = "_", 
                     idvar="id", timevar=c("year"))
class(data_long$year)  
data_long$year <- as.numeric(data_long$year)
data_long$year <- data_long$year -1

### plots improvement in effort
par(mfrow=c(1,2), oma=c(0,0,2,0))
logdataframe <- 1:100
logdataframe <- as.numeric(logdataframe)
logdataframe <- as.data.frame(logdataframe)
logdataframe$log <- log10(logdataframe$logdataframe)
class(logdataframe$log)


#plots
mysum <- function(x){
    250000-sum(x)
}
##plot all cause mortality
plot(aggregate(year_0~year,data = subset(data_long, groupname=="group 1"), FUN= function(z) mysum(z)), 
     
     main = "All Cause Mortality",
     
     pch=20, 
     col= "red",
     xlab= "Quarters",
     ylab= "accummulated all-cause mortality")
text(x=16, y=1, labels= "Current 50% treated")
points(x=32, y=1, pch=20, col="red")
text(x=16, y=1000, labels= "60% treated")
points(x=32, y=1000, pch=20, col ="blue")
text(x=16, y=2800, labels= "70% treated")
points(x=32, y=2800, pch=20, col="green")

points(aggregate(year_0~year,data = subset(data_long, groupname=="group 2"), FUN= function(z) mysum(z)), 
       col="blue",
       pch=20)
points(aggregate(year_0~year,data = subset(data_long, groupname=="group 3"), FUN= function(z) mysum(z)), 
       col="green",
       pch=20)
title("Expected outcome of improvement in LLD adherence in Denmark", outer=TRUE, sub = "Simulation on Danish population with indication for LLD treatment")

survivortable <- 1
as.data.frame(survivortable)

group1 <- aggregate(year_0~year,data = subset(data_long, groupname=="group 1"), FUN= function(z) mysum(z))
group2 <- aggregate(year_0~year,data = subset(data_long, groupname=="group 2"), FUN= function(z) mysum(z))
group3 <- aggregate(year_0~year,data = subset(data_long, groupname=="group 3"), FUN= function(z) mysum(z))
groups <- data.frame(group1, group2, group3)
groups$year.1 <- NULL
groups$year.2 <- NULL
colnames(groups) <- c("year","group 1", "group 2", "group 3")


####---- CVD EVENT
#generates death to overall  
data$year_0 <- 1
for (i in 1:40){ 
    if (i == 1){
        n <- ifelse(data$treated=="treated", rbinom(N, 1, 1-0.016/48), rbinom(N, 1, 1-0.03/48))
    } 
    else {
        n <- ifelse(data$treated=="treated", 
                    ifelse(data[[paste0("year_", i-1)]] == 0,  0, rbinom(N, 1, 1-0.016/48)), 
                    ifelse(data[[paste0("year_", i-1)]] == 0,  0, rbinom(N, 1, 1-0.03/48))
        )
    }
    data[[paste0("year_", i)]] <- n 
}


data_long <- reshape(data, direction="long", varying= c(list(4:44)), sep = "_", 
                     idvar="id", timevar=c("year"))
class(data_long$year)  
data_long$year <- as.numeric(data_long$year)
data_long$year <- data_long$year -1

#plots
mysum <- function(x){
    250000-sum(x)
}
plot(aggregate(year_0~year,data = subset(data_long, groupname=="group 1"), FUN= function(z) mysum(z)), 
     main = "Myocardial infarction, Stroke, CV Death ",
     
     pch=20, 
     col="red",
     xlab= "Quarters",
     ylab= "accumulated MI, Stroke,  CV Death")
points(aggregate(year_0~year,data = subset(data_long, groupname=="group 2"), FUN= function(z) mysum(z)), 
       col="blue",
       pch=20)
points(aggregate(year_0~year,data = subset(data_long, groupname=="group 3"), FUN= function(z) mysum(z)), 
       col="green",
       pch=20)
text(x=4, y=1, labels= "Current 50% treated")
points(x=8, y=1, pch=20, col="red")
text(x=4, y=500, labels= "60% treated")
points(x=8, y=500, pch=20, col="blue")
text(x=4, y=1000, labels= "70% treated")
points(x=8, y=1000, pch=20, col="green")



survivortable <- 1
as.data.frame(survivortable)

group1 <- aggregate(year_0~year,data = subset(data_long, groupname=="group 1"), FUN= function(z) mysum(z))
group2 <- aggregate(year_0~year,data = subset(data_long, groupname=="group 2"), FUN= function(z) mysum(z))
group3 <- aggregate(year_0~year,data = subset(data_long, groupname=="group 3"), FUN= function(z) mysum(z))
groupsCVD <- data.frame(group1, group2, group3)
groupsCVD$year.1 <- NULL
groupsCVD$year.2 <- NULL
colnames(groupsCVD) <- c("year","group 1", "group 2", "group 3")
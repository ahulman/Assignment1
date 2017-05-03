# Challenge 6 (trajectory before event occurence)
#* Check out whether the deceased or the transplant group is larger and keep that for the rest of the exercise
#* Set time=0 at event occurence and fit log-serum bilirubin trajectories "backwards in time" (see Tabak et al., Lancet 2009)
#* Compare the treatment and the placebo group
#* Let me know if something is not clear

#We aim to discuss your solutions next Thursday (May 4). If any urgent matter comes up that limits your ability to work on the project, let me know in time. 


    #* Check out whether the deceased or the transplant group is larger and keep that for the rest of the exercise
    class(data$status)
    
    summary(data$status)

    dataDead <- subset(data, status=="dead")
    #* Set time=0 at event occurence and fit log-serum bilirubin trajectories "backwards in time" (see Tabak et al., Lancet 2009)
    dataDead$yearsBeforeEvent <- dataDead$year - dataDead$years
    
    model1 <- lme(logbilirubin ~ drug*yearsBeforeEvent , data = dataDead, random= ~ 1|id, method="REML", na.action=na.omit)
    summary(model1)
    
    x <- seq(-15,0, by=1)
    
    x.pred1 <- cbind(1,0,x,0)  
    x.pred2 <- cbind(1,1,x,x)
    
    newdata <- rbind(x.pred1, x.pred2)
    
    preddata <- matrix(data = NA, nrow = 0, ncol = 5)  #initialising matrix of predictions
    
    for (j in 1:2){                                                     
        
        index1 <- 1 + (j-1)*(dim(newdata)[1])/2                                 
        index2 <- j*(dim(newdata)[1])/2
        
        y.pred <- newdata[index1:index2,] %*% fixef(model1)    #predicted values
        
        y.se <- ci.lin(model1,newdata[index1:index2,])[,2]     #std err of the mean
        y.cil <- y.pred - 1.96*y.se                         #lower ci of mean
        y.ciu <- y.pred + 1.96*y.se                         #upper ci of mean
        
        temp <- cbind(x,y.pred,y.cil,y.ciu,j)               #saving predictions with CI for class "j"
        preddata <- rbind(preddata,temp)                    #appending predictions for all groups
    }
    
    # create data frame object with meaningful column names
    preddata <- data.frame(preddata)
    names(preddata) <- c("time","logBil","lcb","ucb","drug")
    
    plot(preddata[preddata$drug=="1", c("time", "logBil")], col = "red")
    lines(preddata[preddata$drug=="1", c("time", "logBil")], col = "red")
    points(preddata[preddata$drug=="1", c("time", "lcb")], col = "red")
    points(preddata[preddata$drug=="1", c("time", "ucb")], col = "red")
    points(preddata[preddata$drug=="2", c("time", "logBil")], col = "blue")
    lines(preddata[preddata$drug=="2", c("time", "logBil")], col = "blue")
    points(preddata[preddata$drug=="2", c("time", "lcb")], col = "blue")
    points(preddata[preddata$drug=="2", c("time", "ucb")], col = "blue")
    
    
par()
par(mfrow=c(1,1))

col1 <- rgb(76, 89, 136, 255, maxColorValue = 255) # blue, Diabetologia style
col2 <- rgb(193, 88, 88, 255, maxColorValue = 255) # red, Diabetologia style


# define simCohort first (either run the other script or use source)

cohort1 <- simCohort(100, 123)
model <- lm(bmi~age*sex, cohort1)
agePred <- c(40,70)
pred <- predict(model, newdata = data.frame(sex = c("male","male","female","female"), age = c(agePred,agePred)))

# create an initial empty plot
plot(c(0,0), xlab = "Age (year)", ylab = "BMI (kg/m2)", xlim = c(40,70), ylim = c(25,32), xaxt="n",yaxt="n",bty = "n")
# define x and y axis
axis(1, at = seq(40, 70, 10))
axis(2,at = seq(25,32,1), las=1, cex=2)
# add points by sex
points(cohort1[cohort1$sex=="male",c("age","bmi")], col = col1)
points(cohort1[cohort1$sex=="female",c("age","bmi")], col = col2)
lines(agePred,pred[1:2], col = col1)
lines(agePred,pred[3:4], col = col2)
legend(40,32, c("Men","Women"), col = c(col1,col2), pch = 1)

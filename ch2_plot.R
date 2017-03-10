# define simCohort first (either run the other script or use source)

cohort1 <- simCohort(100, 123)
model <- lm(bmi~age*sex, cohort1)

# create an initial empty plot
plot(c(0,0), xlab = "Age (year)", ylab = "BMI (kg/m2)", xlim = c(40,70), ylim = c(25,32), xaxt="n",yaxt="n",bty = "n")
# define x and y axis
axis(1, at = seq(40, 70, 10))
axis(2,at = seq(25,32,1), las=1, cex=2)
# add points by sex
points(cohort1[1:60,c("age","bmi")], col = "blue")
points(cohort1[61:100,c("age","bmi")], col = "red")
# predict values for regression line
agePred <- c(40,70)
pred <- predict(model, newdata = data.frame(sex = c("male","male","female","female"), age = c(agePred,agePred)))
lines(agePred,pred[1:2], col = "blue")
lines(agePred,pred[3:4], col = "red")

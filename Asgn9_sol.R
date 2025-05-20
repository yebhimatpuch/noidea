#Question 1
# A pipe manufacturing organization produces different kinds of pipes. We are given
# the monthly data of the wall thickness of certain types of pipes (data is available on
#                                                                   LMS Clt-data.csv).
# The organization has an analysis to perform and one of the basic assumption of that
# analysis is that the data should be normally distributed.
# You have the following tasks to do:
# (a) Import the csv data file in R.
# (b) Validate data for correctness by counting number of rows and viewing the top
# ten rows of the dataset.
# (c) Calculate the population mean and plot the observations by making a histogram.
# (d) Mark the mean computed in last step by using the function abline.

# a)
df <- read.csv(file.choose())
summary(df)
head(df)

#b)
dim(df)
head(df, 10)

# c)
m <- mean(df$Wall.Thickness)
cat("Population Mean:", m)
hist(df$Wall.Thickness, col = 'skyblue')
#using labels
hist(df$Wall.Thickness, main = 'Distribution of wall thickness', xlab = 'Wall Thickness (mm)', ylab = 'Frequency', col = 'skyblue' )

#d
abline(v = m, col = "red", lty = 2, lwd = 2)


#Question 2
# (a) Draw sufficient samples of size 10, calculate their means, and plot them in R
# by making histogram. Do you get a normal distribution.
# (b) Now repeat the same with sample size 50, 500 and 9000. Can you comment on
# what you observe.


#a
s10 <- c()

for(i in 1:10){
  s10[i] <- mean(sample (df$Wall.Thickness, 10, TRUE))
  print(s10)
}

hist(s10, main = 'Mean of 10 samples' )


#b
par(mfrow = c(1,3))

s50 <- c()
for(i in 1:50){
  s50[i] <- mean(sample(df$Wall.Thickness, 50, TRUE))
}
hist(s50)
abline(v=mean(s50), col = 'red', lty = 2, lwd = 3)


s500 <- c()
for(i in 1:500){
  s500[i] <- mean(sample(df$Wall.Thickness, 500, TRUE))
}
hist(s500)
abline(v=mean(s500), col = 'blue', lty = 2, lwd = 3)


s9000 <- c()
for(i in 1:9000){
  s9000[i] <- mean(sample(df$Wall.Thickness, 9000, TRUE))
}
hist(s9000)
abline(v=mean(s9000), col = 'darkgreen', lty = 2, lwd = 3)







#The following table gives information on ages and cholesterol levels for a random
#sample of 10 men
#Age 58 69 43 39 63 52 47 31 74 36
#Cholesterol 189 235 193 177 154 191 213 165 198 181
#Plot the scatter diagram and a regression line that will enable us to predict Cholesterol
#level on age. Further, estimate the cholesterol level of a 60 year-old man.
#11


age <- c(58, 69, 43, 39, 63, 52, 47, 31, 74, 36)
cholesterol <- c(189, 235, 193, 177, 154, 191, 213, 165, 198, 181)

men_data <- data.frame(Age=age, Cholesterol = cholesterol)

plot(men_data$Age, men_data$Cholesterol, 
     xlab = "Age", ylab = "Cholesterol Level",
     main = "Age vs Cholesterol Level",
     pch = 19, col = "blue")

# add regression line
regression_line <- lm(Cholesterol ~ Age, data = men_data)
abline(regression_line, col = "red", lwd = 2)

# b.
predicted_chol <- predict(regression_line, newdata = data.frame(Age = 60))
cat("Predicted cholesterol level for a 60 year-old man:", predicted_chol)


#3. A research methodology course has recently been added to the PhD curriculum at
#the Thapar Institute of Engineering and Technology, Patiala. To evaluate its effec-
#  tiveness, students take a test on formulating research problems and writing research
#papers both before and after completing the course. Below are the marks for a ran-
#  dom sample of ten students:
#  Before the test 145 173 158 141 167 159 154 167 145 153
#After the test 155 167 156 149 168 162 158 169 157 161
#Assume that the differences between the pre-course and post-course test scores are
#normally distributed, and a high score on the test indicates a strong level of assertive-
#  ness. Do the collected data, at 5% level of significance, provide enough #evidence to
#conclude that research scholars become more assertive after completing the course?

install.packages("BSDA")
library(BSDA)

version$version.string


# 3. Paired t-test for Research Methodology Course Effectiveness

before_scores <- c(145, 173, 158, 141, 167, 159, 154, 167, 145, 153)
after_scores  <- c(155, 167, 156, 149, 168, 162, 158, 169, 157, 161)

paired_test_result <- t.test(after_scores, before_scores,
                             paired = TRUE,
                             conf.level = 0.95)     

print(paired_test_result)

# Optional: Add interpretation based on the p-value
alpha <- 0.05
p_value <- paired_test_result$p.value

cat("\nSignificance Level (alpha):", alpha, "\n")
cat("P-value:", format(p_value, scientific = FALSE), "\n") # Format p-value for readability

if (p_value < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0).\n")
  cat("There is statistically significant evidence at the 5% level to conclude that the scores after the course are higher than before.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis (H0).\n")
  cat("There is not enough statistically significant evidence at the 5% level to conclude that the scores after the course are higher than before.\n")
}








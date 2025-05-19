## 1. Part 1
# a) 
data <- read.csv("Clt-data.csv")

# b.
nrow(data)
head(data, 10)

# c. 
mean_thickness <- mean(data$Wall.Thickness)

# Histogram
hist(data$Wall.Thickness, 
     breaks = 30,
     col = "lightblue", 
     main = "Histogram of Pipe Wall Thickness", 
     xlab = "Wall Thickness")


# d.
abline(v = mean_thickness, col = "red", lwd = 2)

# Part 2 a.
set.seed(123)
sample_means_10 <- replicate(1000, {
  sample_data <- sample(data$Wall.Thickness, size = 10, replace = TRUE)
  mean(sample_data)
})

hist(sample_means_10, 
     breaks = 30, 
     col = "lightgreen", 
     main = "Sampling Distribution (n = 10)",
     xlab = "Sample Mean")

# b. 
sample_means_50 <- replicate(1000, mean(sample(data$Wall.Thickness, size = 50, replace = TRUE)))
hist(sample_means_50, breaks = 30, col = "red", 
     main = "Sampling Distribution (n = 50)", xlab = "Sample Mean")

sample_means_500 <- replicate(1000, mean(sample(data$Wall.Thickness, size = 500, replace = TRUE)))
hist(sample_means_500, breaks = 30, col = "purple", 
     main = "Sampling Distribution (n = 500)", xlab = "Sample Mean")

sample_means_9000 <- replicate(1000, mean(sample(data$Wall.Thickness, size = 9000, replace = TRUE)))
hist(sample_means_9000, breaks = 30, col = "orange", 
     main = "Sampling Distribution (n = 9000)", xlab = "Sample Mean")


## 2. 
age <- c(58, 69, 43, 39, 63, 52, 47, 31, 74, 36)
cholesterol <- c(189, 235, 193, 177, 154, 191, 213, 165, 198, 181)

men_data <- data.frame(Age = age, Cholesterol = cholesterol)

# a. 
plot(men_data$Age, men_data$Cholesterol, 
     xlab = "Age", ylab = "Cholesterol Level",
     main = "Age vs Cholesterol Level",
     pch = 19, col = "blue")

# Add regression line
regression_line <- lm(Cholesterol ~ Age, data = men_data)
abline(regression_line, col = "red", lwd = 2)
``
# b.
predicted_chol <- predict(regression_line, newdata = data.frame(Age = 60))
cat("Predicted cholesterol level for a 60 year-old man:", predicted_chol)


# 3. 
install.packages("BSDA")
library(BSDA)

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

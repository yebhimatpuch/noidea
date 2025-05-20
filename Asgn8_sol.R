# For a word that has sample, random, replicate, simulate use 'r'
# xbar - u / sigma * root n 

# rnorm, r binom

# TEMPLATE
set.seed(123)
n <- 30
num_samples <- 1000
mu <- 100
sigma <- 15
sample_means <- replicate(num_samples, {
  sample <- rnorm(n, mean = mu, sd = sigma)
  mean(sample)
})
mean(sample_means)
var(sample_means)

# Empirical mean
# Empirical variance





# Poison Distribution: 1. . If X ∼ Poisson(λ = 4), simulate the sampling distribution of the mean for sample size n = 50. Compare theoretical and empirical mean and variance.

set.seed(123)
n <- 50 
num_samples <- 1000
lambda <- 4

sample_means <- replicate(num_samples,{
  sample <- rpois(n, lambda)
  mean(sample)
}) 

empirical_mean = mean(sample_means)
empirical_variance = var(sample_means)

theoretical_mean <- lambda
theoretical_variance <- lambda/n

cat("Empirical Mean:", empirical_mean, "\n")
cat("Theoretical Mean:", theoretical_mean, "\n")
cat("Empirical Variance:", empirical_variance, "\n")
cat("Theoretical Variance:", theoretical_variance, "\n")



# Poison Distribution: 2. How does the distribution of sample means change as λ increases 
set.seed(123)
n <- 50
num_samples <- 1000
lambda_values <- c(1, 4, 10, 20)

# Plot distribution of sample means for different lambda
par(mfrow = c(2,2))

for(lambda in lambda_values){
  sample_means <- replicate(num_samples, {
    sample <- rpois(n, lambda)
    mean(sample)
  })
  
  hist(sample_means,       
       main = paste("λ =", lambda),
       xlab = "Sample Mean",
       col = "lightblue",
       probability = TRUE,
       breaks = 30)
  
  curve(dnorm(x, mean = lambda, sd = sqrt(lambda/n)), add = TRUE, col = "red", lwd = 2)
  
}


# Exponential Distribution 1.
#For X ∼ Exponential(λ = 1.5), simulate sample means and check normality for n = 10 and n = 50.

set.seed(123)
lambda <- 1.5
mu <- 1/lambda
var_theoretical <- 1/(lambda^2)

num_samples <- 1000

n_values <- c(10,50)

for(n in n_values){
  sample_means <- replicate(num_samples, {
    sample <- rexp(n, rate = lambda)
    mean(sample)
  })
  
  cat("\n--- Sample size:", n, "---\n")
  cat("Empirical Mean:", mean(sample_means), "\n")
  cat("Theoretical Mean:", mu, "\n")
  
  cat("Empirical Variance:", var(sample_means), "\n")
  cat("Theoretical Variance:", var_theoretical / n, "\n")
}



# Exponential Distribution 2.  Does the Central Limit Theorem apply here? How does sample size affect the result?

set.seed(123)
num_samples <- 1000
lambda <- 1.5
n_values <- c(10,30,50,100)

par(mfrow = c(2,2))

for(n in n_values){
  sample_means <- replicate(num_samples, {
    sample <- rexp(n, rate = lambda)
    mean(sample)
  })
  
  hist(sample_means, probability = TRUE,
       main = paste("n =", n),
       xlab = "Sample Mean", col = "skyblue", breaks = 30)
  
  curve(dnorm(x, mean = 1/lambda, sd = sqrt((1/lambda^2)/n)), 
        add = TRUE, col = "red", lwd = 2)

}


# Normal Distribution
#Use X ∼ N (70, 10^2). Simulate the sampling distribution of the mean for different values of n and confirm that it remains normal.

set.seed(123)
num_samples <- 1000
mu <- 70
sigma <- 10

n_values <- c(10,30,50,100)

for(n in n_values){
  sample_means <- replicate(num_samples, {
    sample <- rnorm(n, mean = mu, sd = sigma)
    mean(sample)
  })
  

  cat("Sample size n =", n, "\n")
  cat("Empirical Mean:", mean(sample_means), "\n")
  cat("Theoretical Mean:", mu, "\n")
  cat("Empirical Variance:", var(sample_means), "\n")
  cat("Theoretical Variance:", sigma^2 / n, "\n\n")
}






# Gamma Distribution 1.

#Take X ∼ Gamma(shape = 2, rate = 1). Simulate and compare the sampling distribution for n = 10 and n = 100.

set.seed(123)
num_samples <- 1000
shape <- 2
rate <- 1
mu <- shape/rate
var_theoretical <- shape/(rate^2)

n_values <- c(10,100)

for(n in n_values){
  sample_means <- replicate(num_samples, {
    sample <- rgamma(n, shape = shape, rate = rate)
    mean(sample)
  })
  
  cat("Sample size n =", n, "\n")
  cat("Empirical Mean:", mean(sample_means), "\n")
  cat("Theoretical Mean:", mu, "\n")
  cat("Empirical Variance:", var(sample_means), "\n")
  cat("Theoretical Variance:", var_theoretical / n, "\n\n")

}


#2. How does the shape of the original distribution influence the sample mean distribution?

par(mfrow = c(2,1))
set.seed(123)
num_samples <- 1000
shape <- 2
rate <- 1

n_values <- c(10,100)

for (n in n_values) {
  sample_means <- replicate(num_samples, {
    mean(rgamma(n, shape = shape, rate = rate))
  })
  
  hist(sample_means, probability = TRUE,
       breaks = 30,
       col = "lightblue",
       main = paste("n =", n),
       xlab = "Sample Mean")
  
  # Overlay normal curve
  mu <- shape / rate
  sigma2 <- shape / rate^2
  curve(dnorm(x, mean = mu, sd = sqrt(sigma2 / n)),
        add = TRUE, col = "red", lwd = 2)
}














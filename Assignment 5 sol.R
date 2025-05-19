# Ques 1 Uniform Distribution
#a) P(X>45)
p_more_45 = 1 - punif(45, min = 0, max = 60)
# OR p_more_45 = punif(45, min = 0, max = 60, lower.tail = FALSE)

cat("P(X > 45):", p_more_45, "\n")

#b) (20<X<30)
p_in_20_30 = punif(30, min=0, max = 60) - punif(20, min=0, max=60)
cat("P(20 < X < 30):", p_in_20_30, "\n")

# Ques 2 : Exponential lambda = 1/3
#a) Value of density at x=4x=4
p_val_density = dexp(4, 1/3)
cat("Value of density function at x = 4:", p_more_45, "\n")

#b) PDF for 0≤x≤50≤x≤5
x_vals <- seq(0, 5)
pdf_vals <- dexp(x_vals, rate = 1/3)


plot(x_vals, pdf_vals, type = "l", 
     col = "blue",
     main = "Exponential PDF (λ = 1/3)", 
     xlab = "Time (hours)",
     ylab = "Density")

#c) p(x <= 3)
p_less_3 = pexp(3, 1/3)
cat("P(X ≤ 3):", p_less_3, "\n")


#d) Plot CDF
x_vals_3 <- seq(0, 5)
cumultative_vals <- pexp(x_vals_3, rate = 1/3)

plot(x_vals_3, cumultative_vals, type = "l", 
     col = "red", lwd = 2,
     main = "Cumulative exponential probabilities for 0 ≤ x ≤ 5", 
     xlab = "Time (hours)", ylab = "Density")

#e) Simulate 1000
n <- 1000
x_sim <- rexp(n, rate = 1/3) # rexp used to generate sample from exponetial distribution. 

plot(density(x_sim), xlab = "Simulated x", ylab = "density", 
     main = "Simulated data from exp. dist. at lambda = 1/3")

hist(x_sim, probability=TRUE, xlab = "Simulated x", ylab = "density", 
     main = "Simulated data from exp. dist. at lambda = 1/3")

# Ques 3 Gamma Distribution 
alpha = 2
beta = 1/4

#a) P(X≥1)
p_at_least_1 = 1 - pgamma(1, shape = alpha, rate = beta)
# OR  p_at_least_1 = pgamma(1, shape = alpha, rate = beta, lower.tail = FALSE)
cat("P(X ≥ 1):", p_at_least_1, "\n")

#b) inverse cdf, P(X≤c)=0.70
c_value <- qgamma(0.70, shape = alpha, rate = beta)
cat("Value of c for P(X ≤ c) ≥ 0.70:", c_value, "\n")

c_value2 = 0
while (pgamma(c_value2, shape = alpha, rate = beta) < 0.70) {
  c_value2 <- c_value2 + 0.000001  
}
cat("Value of c for P(X ≤ c) ≥ 0.70 (Using loop):", c_value2, "\n")





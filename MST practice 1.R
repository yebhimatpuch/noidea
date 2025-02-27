# SET 1
## Ques 1

trials = 10000


p_e = 0 # atleast two heads
p_f = 0 # first flip is head
p_e_and_f = 0

for (i in 1:trials){
  flips = sample(c("H", "T"), 3, replace = TRUE)
  
  if (flips[1] == "H") p_f = p_f + 1
  
  if (sum(flips == "H") >= 2) p_e = p_e + 1
  
  if ((flips[1] == "H") && (sum(flips == "H") >= 2)) p_e_and_f = p_e_and_f + 1
}

e_final = p_e / trials
f_final = p_f / trials
e_and_f_final = p_e_and_f / trials

e_union_f = e_final + f_final - e_and_f_final

cat("P(E) =", e_final, "\nP(F) =", f_final, "\nP(E and F) =", e_and_f_final, "\nP(E or F) =", e_union_f)

# Ques 2
male = 0.6
female = 0.4
remote_given_male = 0.5
remote_given_female = 0.3

remote_work = remote_given_male * male + remote_given_female * female
female_given_remote = (remote_given_female * female) / remote_work
cat("The probability that the chosen worker who prefers remote work is a female is: ", round(female_given_remote * 100, 2))

# Ques 3 i)
fib_seq <- function(x) {
  if (x <= 0) {
    return("Invalid input! Enter a positive integer.")
  } else if (x == 1) {
    return(0)
  } else {
    ans <- numeric(x) 
    ans[1] <- 0
    ans[2] <- 1
    for (i in 3:x) {
      ans[i] <- ans[i - 1] + ans[i - 2]
    }
    return(ans) 
  }
}

fib_10 <- fib_seq(10)
fib_15 <- fib_seq(15)

print(paste("Fibonacci sequence (10 terms):", paste(fib_10, collapse = ", ")))
print(paste("Fibonacci sequence (15 terms):", paste(fib_15, collapse = ", ")))


# Ques 3 ii)
pdf_x <- function(x) {
  ifelse(x > 0 & x < 1, 5 * (1 - x)^4, 0)
}

E_X <- integrate(function(x) x * pdf_x(x), lower = 0, upper = 1)$value

E_X3 <- integrate(function(x) (x^3) * pdf_x(x), lower = 0, upper = 1)$value

E_Y <- E_X3 - 3 * E_X

E_Y2 <- integrate(function(x) ((x^3 - 3*x)^2) * pdf_x(x), lower = 0, upper = 1)$value

Var_Y <- E_Y2 - E_Y^2
SD_Y <- sqrt(Var_Y)

cat("E[Y]:", round(E_Y, 4), "\n")
cat("SD[Y]:", round(SD_Y, 4), "\n")

# Ques 4) 
# Given data
n <- 40  
p <- 0.48  

# (a) X follows a Binomial distribution
# X ~ Bin(n, p)

# (b) 
prob_20 <- dbinom(20, size = n, prob = p)
cat("P(X = 20):", prob_20, "\n")

# Sketching the PMF for 1 ≤ X ≤ 10
x_vals <- 1:10
pmf_vals <- dbinom(x_vals, size = n, prob = p)
plot(x_vals, pmf_vals, type="h", lwd=2, col="blue",
     main="PMF of X (1 ≤ X ≤ 10)", xlab="X", ylab="P(X)")

# (c)
prob_30_or_more <- 1 - pbinom(29, size = n, prob = p)
cat("P(X ≥ 30):", prob_30_or_more, "\n")

# Sketching the CDF for 1 ≤ X ≤ 10
cdf_vals <- pbinom(x_vals, size = n, prob = p)
plot(x_vals, cdf_vals, type="s", lwd=2, col="red",
     main="CDF of X (1 ≤ X ≤ 10)", xlab="X", ylab="P(X ≤ x)")

  
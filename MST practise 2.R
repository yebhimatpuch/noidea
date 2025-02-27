# SET 2
# Ques 1
n_sim = 10000
deck = rep(1:52, n_sim)  

E = (deck %% 13 == 0)  
F = (deck %% 13 >= 11)  

P_E = mean(E)
P_F = mean(F)
P_E_and_F = mean(E & F)
P_E_or_F = P_E + P_F - P_E_and_F

cat("P(E):", P_E, "\n")
cat("P(F):", P_F, "\n")
cat("P(E and F):", P_E_and_F, "\n")
cat("P(E or F):", P_E_or_F, "\n")

# Ques 2
compute_bayes = function(P_A, P_B, P_B_given_A) {
  return (P_B_given_A * P_A) / P_B
}

P_local = 0.80
P_international = 0.20
P_more_lang_given_local = 0.60
P_more_lang_given_intl = 0.90
P_more_lang = P_more_lang_given_local * P_local + P_more_lang_given_intl * P_international

P_international_given_more_lang = compute_bayes(P_international, P_more_lang, P_more_lang_given_intl)

cat("P(International | Speaks more than one language):", P_international_given_more_lang, "\n")

# Ques 3 i)
pell_sequence = function(n) {
  P = numeric(n)
  P[1] = 0
  P[2] = 1
  for (i in 3:n) {
    P[i] = 2 * P[i-1] + P[i-2]
  }
  return(P)
}

n1 = 10
n2 = 15
pell_10 = pell_sequence(n1)
pell_15 = pell_sequence(n2)

cat("First 10 Pell numbers:", pell_10, "\n")
cat("First 15 Pell numbers:", pell_15, "\n")

# Ques 3 ii)
f_x = function(x) 4 * x^3

E_X = integrate(function(x) x * f_x(x), 0, 1)$value
E_X2 = integrate(function(x) x^2 * f_x(x), 0, 1)$value
Var_X = E_X2 - E_X^2

E_Y = integrate(function(x) (2*x^2 - x) * f_x(x), 0, 1)$value
E_Y2 = integrate(function(x) (2*x^2 - x)^2 * f_x(x), 0, 1)$value
Var_Y = E_Y2 - E_Y^2
SD_Y = sqrt(Var_Y)

cat("E[Y]:", E_Y, "\n")
cat("Var[Y]:", Var_Y, "\n")
cat("SD[Y]:", SD_Y, "\n")

# Ques 4
n = 30
p = 0.55

P_18 = dbinom(18, size = n, prob = p)
cat("P(X = 18):", P_18, "\n")

x_vals = 1:10
pmf_vals = dbinom(x_vals, size = n, prob = p)
plot(x_vals, pmf_vals, type="h", lwd=2, col="blue",
     main="PMF of X (1 ≤ X ≤ 10)", xlab="X", ylab="P(X)")

P_at_least_25 = 1 - pbinom(24, size = n, prob = p)
cat("P(X ≥ 25):", P_at_least_25, "\n")

cdf_vals = pbinom(x_vals, size = n, prob = p)
plot(x_vals, cdf_vals, type="s", lwd=2, col="red",
     main="CDF of X (1 ≤ X ≤ 10)", xlab="X", ylab="P(X ≤ x)")

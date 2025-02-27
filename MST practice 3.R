# SET 3
# Ques 1
lambda <- 50000 * 1e-4

cat("X follows Poisson distribution with λ =", lambda, "\n")

prob_X_leq_2 <- ppois(2, lambda)
prob_X_geq_3 <- 1 - prob_X_leq_2

cat("P(X ≥ 3) =", prob_X_geq_3, "\n")

# Ques 2
x_values <- c(-3, -1, 2, 4, 5)  
p_values <- rep(1/5, 5)         

E_X <- sum(x_values * p_values)
cat("E[X]:", E_X, "\n")

E_X2 <- sum((x_values^2) * p_values)
cat("E[X^2]:", E_X2, "\n")

Var_X <- E_X2 - E_X^2
cat("Var[X]:", Var_X, "\n")

E_Y <- 3 * E_X + 2
Var_Y <- 9 * Var_X

cat("E[Y]:", E_Y, "\n")
cat("Var[Y]:", Var_Y, "\n")

# Ques 3
f_x <- function(x) (3/8) * x^2

E_X <- integrate(function(x) x * f_x(x), lower = 0, upper = 2)$value
cat("E[X]:", E_X, "\n")

E_X2 <- integrate(function(x) x^2 * f_x(x), lower = 0, upper = 2)$value
cat("E[X^2]:", E_X2, "\n")

# Ques 4
P_S <- 0.30   
P_L <- 0.20   
P_L_given_S <- 0.50  

P_S_given_L <- (P_L_given_S * P_S) / P_L
cat("P(Smoker | Lung Disease):", P_S_given_L, "\n")



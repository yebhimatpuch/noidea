# install.packages('pracma')

#Q1 i) joint pdf 
library(pracma)
joint_pdf = function(x,y){
    return(2*(2*x + 3*y) / 5)
}
int_value = integral2(joint_pdf, xmin = 0, xmax = 1, 
                      ymin = 0, ymax = 1)
print(int_value$Q)

#Q1 ii) marginal for x
g_x = function(x) {
  integrate(function(y) joint_pdf(x, y), lower = 0, upper = 1)$value
}

cat("g(x) at x = 1: ", g_x(1), "\n")

#Q1 iii) marginal for y
h_y = function(y){
  integrate(function(x) joint_pdf(x,y), lower = 0, upper = 1)$value
}
cat("h(y) at x = 0: ", h_y(0), "\n")

#Q1 iv) expectation
library(pracma)
expected_value_xy_func <- function(x, y) {
  return(x * y * joint_pdf(x, y))
}
expected_value_xy <- integral2(expected_value_xy_func, xmin = 0, xmax = 1, ymin = 0, ymax = 1)$Q

cat("Expected value of g(xy) = xy: ", expected_value_xy, "\n")

#Q2 i) joint pmf  ( discrete )
joint_pmf = function(x, y) {
  return((x + y) / 30)  
}

x_values = c(0,1,2,3) 
y_values = c(0,1,2) 

pmf_matrix = matrix(nrow = length(x_values), ncol = length(y_values)) # it is empty for now

for (i in 1:length(x_values)) {
  for (j in 1:length(y_values)) {
    pmf_matrix[i, j] <- joint_pmf(x_values[i], y_values[j])
  }
}

print(pmf_matrix)

#Q2 ii)  check if mass function or not 
total_prob = sum(pmf_matrix)
cat("Sum of all probabilities =", total_prob, "\n")

#Q2 iii) marginal x
marginal_x = apply(pmf_matrix, 1, sum)
print("Marginal distribution of X:")
print(marginal_x)

#Q2 iv) marginal y
marginal_y = apply(pmf_matrix, 2, sum)
print("Marginal distribution of Y:")
print(marginal_y)

#Q2 v) conditional 
p_x0_y1_num = pmf_matrix[1,2]
p_y1_den = marginal_y[2]

conditional_x0_given_y1 = p_x0_y1_num / p_y1_den
cat("Conditional probability at x = 0 given y = 1: ", conditional_x0_given_y1, "\n")

#Q2 vi) expectations
E_X = sum(x_values * marginal_x)
cat("E(x): ", E_X, "\n")

E_Y = sum(y_values * marginal_y)
cat("E(y): ", E_Y, "\n")

E_XY = sum(outer(x_values, y_values, "*") * pmf_matrix)
cat("E(xy): ", E_XY, "\n")

# Variences
E_X2 <- sum((x_values^2) * marginal_x)
Var_X <- E_X2 - (E_X)^2
cat("Var(x): ", Var_X, "\n")

E_Y2 <- sum((y_values^2) * marginal_y)
Var_Y <- E_Y2 - (E_Y)^2
cat("Var(y): ", Var_Y, "\n")

# Covareince and corelation

Cov_XY = E_XY - E_X * E_Y
cat("Cov(x,y): ", Cov_XY, "\n")

correlation = Cov_XY / (sqrt(Var_X) * sqrt(Var_Y))
cat("Correlation coefficient: ", correlation, "\n")




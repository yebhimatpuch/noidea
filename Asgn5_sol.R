# Question 1
# Consider that X is the time (in minutes) that a person has to wait in order to take a flight.
# If each flight takes off each hour X ~ U(0, 60). Find the probability that
# (a) waiting time is more than 45 minutes, and
# (b) waiting time lies between 20 and 30 minutes.

#punif - cdf
a <- punif(45, min = 0, max = 60, lower.tail = FALSE)
print(a)

b <- punif(30,0,60) - punif(20,0,60)
print(b)


# Question 2
# The time (in hours) required to repair a machine is an exponential distributed random
# variable with parameter λ = 1/2.
# (a) Find the value of density function at x = 3.
# (b) Plot the graph of exponential probability distribution for 0 ≤ x ≤ 5.
# (c) Find the probability that a repair time takes at most 3 hours.
# (d) Plot the graph of cumulative exponential probabilities for 0 ≤ x ≤ 5.
# (e) Simulate 1000 exponential distributed random numbers with λ = 1⁄2 and plot the
# simulated data.

lambda <- 1/2

# a <-  dexp (x,lambda)    dexp - pdf for exponential
a <- dexp(3, lambda)
print(a)


#b 
#x <- seq(0, 5, 0.001) creates a sequence of x values from 0 to 5 with increments of 0.001 used for plotting.

x <- seq(0,5, 0.001)
plot(x, dexp(x, lambda), type='l', lwd = 2, col = 'red')

#c
c <- pexp(3, lambda)  # computes cdf for exponential distribution
print(c)

#d
plot(x, pexp(x, lambda), type = 'l', lwd = 2, col = 'blue')

#e
x <- rexp(1000, lambda)
par(mfrow = c(2,1))  #sets the plotting area to display two plots vertically (2 rows, 1 column
hist(x, col = 'orange')
plot(density(x), col='purple', lwd = 3)


# Question 3  , This ques is related to GAMMA DISTRIBUTION
# The lifetime of certain equipment is described by a random variable X that follows
# Gamma distribution with parameters α = 2 and β = 1/3.
# (a) Find the probability that the lifetime of equipment is at least 1 unit of time.
# (b) What is the value of c, if P(X ≤ c) ≥ 0.70? (Hint: try quantile function qgamma())
# alpha - shape, #beta - scale
# pgamma(X,shape,size)



#a
alpha <-2
beta <- 1/3

a <- pgamma(1, shape = alpha, scale = beta, lower.tail=FALSE)
print(a)

b <- qgamma(0.70, shape = alpha, scale = beta) # finds quantile c such that p(x<=c)=0.70, the qgamma func returns the value of c where the cdf reaches 0.70 
print(b)





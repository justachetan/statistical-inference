# Author: Aditya Chetan
# Roll Number: 2016217

# Question 2


# First I declare a function to calculate the
# -ve log-likelihood of a given sample. The reason
# for this is to make use of the ```optim```
# command. More on this in a bit.
#
# This function ```weibull_ll``` calculates the
# -ve log-likeihood of a given sample given theta.
# Here, ```theta```,  is unknown parameter and
# ```y``` is the given data sample.
weibull_nll<-function(theta, y) {

    # ```theta``` is a vector here which contains both
    # the unknown parameters
    c <- theta[1]
    sigma <- theta[2]


    # ```ll``` will hold the sample log-likelihood
    # Initialising it with 0 here.
    ll <- 0


    for (i in y) {

        ll <- ll + log(c / sigma) + ((c - 1) * log(i / sigma)) - ((i / sigma) ** c)

    }


    # Since ```optim``` minimises by default, multiply ```ll``` with -1
    # so as to get parameters that minimise negative log-likelihood
    # or maximise ```ll``` to get Maximum Likelihood Estimate
    return(-ll)

}

# Defining the sample as given in the question
y <- c(143, 164, 188, 188, 190, 192, 206, 209, 213, 216, 220, 227, 230, 246, 265, 304, 234)

# Using ```optim``` to minimise -ve log-likelihood.
# This function call returns an object. The
# maximum value of parameter theta can be accessed
# using $par.
#
# ```optim``` by default uses Neldor-Mead method
# for optmisation
p <-optim(c(1,2), weibull_nll, y=y)


cat("Value of MLE:\n\tc\t= ", p$par[1], "\n\tsigma\t= ", p$par[2],"\n")

# Ranges to c and sigma to check minimum on
cs <- seq(1, 10, 0.5)
sigmas <- seq(225, 235, 0.5)

# Code used to get data for plotting

z <- c()

for (i in cs) {
    for (j in sigmas) {
        z <- c(z, weibull_nll(c(i, j), y))
        # cat(i, ',', j, ',', weibull_nll(c(i, j), y), '\n') # Used when I wanted a .csv of the points
    }
}

# z[i, j] if the NLL of sample with c = cs[i] and sigma = sigmas[j]
z <- matrix(z, length(cs), length(sigmas))

persp(cs, sigmas, z, col='green', theta = -60, ticktype = 'detailed', xlab='c', ylab='sigma', zlab='NLL', main = "Plot of Negative Log Likelihood with c and sigma", sub ="Plot 1")
persp(cs, sigmas, z, col='green', theta = -90, ticktype = 'detailed', xlab='c', ylab='sigma', zlab='NLL', main = "Plot of Negative Log Likelihood with c and sigma", sub ="Plot 2")

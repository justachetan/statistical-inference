# Author: Aditya Chetan
# Roll Number: 2016217

# Question 1

# Assumption: The unknown distribution is assumed
# to be a Bernoulli distribution with unknown
# parameter, theta.



# First I declare a function to calculate the
# likelihood of a given sample. The reason
# for this is to make use of the ```optim```
# command. More on this in a bit.
#
# This function ```bern_ll``` calculates the
# likeihood of a given sample given theta.
# Here, ```theta``` is unknown parameter and
# ```y``` is the given data sample.
bern_lhd<-function(theta, y) {

    # Assumption:
    #       y[i] = 1, w.p. theta
    #   &   y[i] = 0, w.p. (1 - theta)


    # ```ll``` will hold the sample likelihood.
    # Initialising it with 1 here.
    lhd <- 1


    for (i in y) {
        if (i == 1) {
            # Multiplying in accordance with assumed pdf of Bernoulli
            lhd <- lhd * theta
        }
        else{
            # Multiplying in accordance with assumed pdf of Bernoulli
            lhd <- lhd * (1 - theta)
        }
    }

    # Since ```optim``` minimises by default, multiply ```ll``` with -1
    # so as to get parameters that minimise negative likelihood
    # or maximise ```ll``` to get Maximum Likelihood Estimate
    return(-lhd)

}

# Defining the sample as given in the question
y <- c(0, 1, 1, 0, 0, 1, 1, 0, 0, 1)

# Using ```optim``` to maximise likelihood.
# This function call returns an object. The
# maximum value of parameter theta can be accessed
# using $par.
#
# ```optim``` by default uses Neldor-Mead method
# for optmisation
p <-optim(1, bern_lhd, y=y)



cat("Value of MLE:", p$par, "\n")
cat("Theoretical value of MLE: 0.5\n")



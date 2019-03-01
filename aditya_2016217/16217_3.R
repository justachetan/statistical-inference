# Author: Aditya Chetan
# Roll Number: 2016217

# Question 3


# We start with initialising the taus and beta
theta <- rep(2, 19)

# Next, we store the data provided in the question

x <- c(3540, 3560, 3739, 2784, 2571, 2729, 3952, 993, 1908, 948, 1172, 1047, 3138, 5485, 5554, 2943, 4969, 4828)
y <- c(3, 4, 1, 1, 3, 1, 2, 0, 2, 0, 1, 3, 5, 4, 6, 2, 5, 4)

# Part (a)
cat("Part (a) Complete Data\n-------------------------------------------\n\n")
# For complete data
# Equations used here have been derived in the report
beta_complete = sum(y) / sum(x)
tau_complete = (x + y) / (beta_complete + 1)

cat("Value of MLE for complete data:\n\tbeta\t=\t", beta_complete,'\n')
for (i in seq(1, length(tau_complete))) {
    cat("\ttau",i,"\t=\t", tau_complete[i], '\n')
}


cat("\nPart (a) Incomplete Data (x_1 is missing)\n-------------------------------------------\n\n")


# For incomplete data
# Equations used here for EM have again been derived in the report

# x_1 from the data will be missing here
x_m <- x[2:length(x)]


# Therefore, the incomplete data can be defined as follows:
z <- c(x_m, y)


# Setting the limit of the number of iterations for EM
max_iter <- 10000

# Tolerance of error for EM
tol <- 10^-12

# Difference between scores in two iterations
diff = 1

# Variable to store the parameters during the iterations
theta_em <- theta


# Assumptions for EM: I am considering the definition of convergence
# to be either when ```max_iter``` iterations are complete or when the
# L2 norm of the difference between the parameter vectors goes below
# ```tol```, whichever occurs first. The equations used here have been
# derived in the report
for (i in seq(1, max_iter)) {

    # Store parameters in the previous iteration
    theta_prev <- theta_em

    # If difference between iterations is less than
    # tolerance, then stop
    if (diff < tol) {
        break
    }


    # beta being updated
    theta_em[1] <- sum(y) / sum(theta_em[2:length(theta_em)])

    # tau_1 being updated
    theta_em[2] <- y[1] / theta_em[1]

    # tau_j, j = 2, 3, 4 ... ,n being updated
    theta_em[3: length(theta_em)] <- (x[2: length(x)] + y[2:length(y)]) / (theta_em[1] + 1)


    # Calculate the difference in parameters
    diff <- sqrt(sum((theta_em - theta_prev)))

}

cat("Value of MLE for incomplete data:\n\tbeta\t=\t", theta_em[1],'\n')
for (i in seq(1, length(theta_em) - 1)) {
    cat("\ttau",i,"\t=\t", theta_em[i + 1], '\n')
}

cat("\nEstimated value of x_1\t=\ttau_1\t=\t", theta_em[2], "\n\n")


# Part (b) y_1 is missing

cat("\nPart (b) Incomplete Data (y_1 is missing)\n-------------------------------------------\n\n")

# Resetting the difference
diff <- 1

# Removing y_1 from consideration
y_m <- y[2 : length(y)]

# Incomplete data in this case will be as shown below
z2 <- c(x, y_m)

# Variable to store the parameters during the iterations
theta_em2 <- theta

# EM algorithm proceeds as previously. The derivation
# of the equations for this part is shown in the report
for (i in seq(1, max_iter)) {

    # Store parameters in the previous iteration
    theta_prev <- theta_em2

    # If difference between iterations is less than
    # tolerance, then stop
    if (diff < 10 ** -12) {
        break
    }


    # beta being updated
    theta_em2[1] <- sum(y[2 : length(y)]) / sum(theta_em2[3:length(theta_em2)])

    # tau_1 being updated
    theta_em2[2] <- x[1]

    # tau_j, j = 2, 3, 4 ... ,n being updated
    theta_em2[3: length(theta_em2)] <- (x[2: length(x)] + y[2:length(y)]) / (theta_em2[1] + 1)


    # Calculate the difference in parameters
    diff <- sqrt(sum((theta_em - theta_prev)))

}


cat("Value of MLE for incomplete data:\n\tbeta\t=\t", theta_em2[1],'\n')
for (i in seq(1, length(theta_em2) - 1)) {
    cat("\ttau",i,"\t=\t", theta_em2[i + 1], '\n')
}

cat("\nEstimated value of y_1\t=\tbeta * tau_1\t=\t", theta_em[1] * theta_em2[2],"\n\n")



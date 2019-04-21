cat("Null Hypothesis: Alcohol does not affect mean response time (H_0: mu = mu_0)\n")
cat("Alternate Hypothesis: Alcohol affects mean response time (H_0: mu != mu_0)\n\n")


s <- 0.3   # Setting the known sample standard deviation
mu_0 <- 0.8    # Setting the required/true average
x_bar <- 1.0   # Setting the sample mean

n <- 28        # Setting the number of observations


# Calculating the test-statistic, in this case, the t-statistic
t_test <- (x_bar - mu_0) / (s / sqrt(n))

cat("Value of test statistic:", t_test, "\n")

# P value of this statistic, since it is a two sided test, will be
# P[Z > |t|], where Z is the t distribution and t is value of test statistic
pvalue <- pt(t_test, df=n-1, lower.tail = FALSE) * 2

# Setting the significance level
alpha <- 0.05

cat("P-value:", pvalue, "\n")
cat("Is P-value < Significance Level?", pvalue < alpha, "\n")

if (pvalue < alpha) {
    cat("Null Hypothesis is rejected.\n")
} else
{
    cat("Null Hypothesis is accepted.\n")
}

cat("Null Hypothesis: Standard deviation of weights falls below 0.4 (H_0: sigma < s_0)\n")
cat("Alternate Hypothesis: Standard deviation of weights remains above 0.4 (H_1: sigma >= s_0)\n\n")

# Storing the data
data <- c(5.728, 5.731, 5.722, 5.719, 5.727, 5.724, 5.718, 5.726, 5.723, 5.722)
# Setting the sample size
n <- 10

# Calculating the sample standard deviation
s <- sd(data)

# Setting the significance level
alpha <- 0.05

# Setting the actual standard deviation
s_0 <- 0.4

# We calculate the test statistic here which is the
# chi-square statistic of degree (n - 1)
chi_test <- ((n - 1) * (s**2)) / (s_0 ** 2)

# The p-value will be area under the curve of pdf, past the
# value of the test statistic
p_value <- pchisq(chi_test, df=n-1, lower.tail = FALSE)

cat("P-value:", p_value, "\n")
cat("Is P-value < Significance Level?", p_value < alpha, "\n")

if (p_value > alpha) {
    cat("Null Hypothesis is accepted.\n")
} else {
    cat("Null Hypothesis is rejected.\n")
}

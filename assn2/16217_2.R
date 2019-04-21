n <- 18 # Setting the sample size

# Storing the data
data <- c(237, 242, 232, 242, 248, 230, 244, 243, 254, 262, 234, 220, 225, 236, 232, 218, 228, 240)

# Setting the true value
mu_0 <- 240

# Calculating the sample mean
x_bar <- mean(data)

# Calculating the sample standard deviation
s <- sd(data)

# Calculating the test statistic, in this case, the t-statistic
z_score <- (x_bar - mu_0) / (s / sqrt(n))

pvalue <- 0

# We calculate the p value of the t-distribution using the
# CDF of the t-distribution, with df = n - 1.
pvalue <- pt(z_score, n-1, lower.tail = TRUE)

# The significance level is 0.05
alpha <- 0.05

cat("P-value:", pvalue, "\n")
cat("Is P-value < Significance Level?", pvalue < alpha, "\n")

if (pvalue < alpha) {
    cat("Null Hypothesis is rejected.\n")
} else
{
    cat("Null Hypothesis is accepted.\n")
}
cat("Null Hypothesis: Smoking does not affect blood pressure (H_0: mu_1 = mu_2)\n")
cat("Alternate Hypothesis: Smoking affects smoking (H_0: mu_1 != mu_2)\n\n")

# Storing the data of the smokers
smokers <- c(124, 134, 136, 125, 133, 127, 135, 131, 133, 125, 118)
num_smokers <- 11 # Number of smokers

# Storing the data of the non-smokers
nonsmokers <- c(130, 122, 128, 129, 118, 122, 116, 127, 135, 120, 122, 120, 115, 123)
num_nonsmokers <- 14 # Number of non-smokers

# Calculate the sample standard deviation of the smokers
s1 <- sd(smokers)
# Calculate the sample standard deviation of the non-smokers
s2 <- sd(nonsmokers)

# Calculate the sample mean of the smokers
x1_bar = mean(smokers)
# Calculate the sample mean of the non-smokers
x2_bar = mean(nonsmokers)

# Calculating the test statistic (details in report)
test_stat = (x1_bar - x2_bar) / sqrt(((s1)**2 / num_smokers) + ((s2)**2 / num_nonsmokers))

# Setting the significance level
alpha <- 0.05


# Calculating the exact degree of freedom for the distribution of the test statistic
A <- s1**2 / num_smokers
B <- s2**2 / num_nonsmokers

dof <- (A + B)**2 / ((A**2 / (num_smokers - 1)) + (B**2 / (num_nonsmokers - 1)))

# Calculating the p-value, which will be the area under curve
# where T > |t| where T is the T-distribution and t is the
# value of the test statistic
p_value <- pt(abs(test_stat), df=dof , lower.tail = FALSE) * 2

cat("P-value:", p_value, "\n")
cat("Is P-value < Significance Level?", p_value < alpha, "\n")

if (p_value > alpha) {
    cat("Null Hypothesis is accepted.\n")
} else {
    cat("Null Hypothesis is rejected.\n")
}



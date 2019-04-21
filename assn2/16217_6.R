cat("Null Hypothesis: Additive does not affect mileage (H_0: m_1 = m_2)\n")
cat("Alternate Hypothesis: Additive affects mileage (H_0: m_1 != m_2)\n\n")

# Storing the data
car_with_additive <- c(24.2, 30.4, 32.7, 19.8, 25.0, 24.9, 22.2, 21.5)
car_wo_additive <- c(23.5, 29.6, 32.3, 17.6, 25.3, 25.4, 20.6, 20.7)

# Sample size
n <- 8

# Calculating differences in corresponding data points
d <- car_with_additive - car_wo_additive

# Number of values that have a positive difference
nplus <- length(d[d>0])
# Number of  values that have a negative difference
nminus <- length(d[d<0])

# This will be our test statistic
x <- min(nplus, nminus)

# Setting the significance level
alpha <- 0.05

# This is the function to execute the sign test. We need a two sided
# test with a significance level of 0.05, i.e., conf. level of 1 - 0.05 = 0.95
result <- binom.test(x, n, p = 0.5, alternative = "two.sided", conf.level = 1 - alpha)
print(result)

cat("\n")

p_value <- result$p.value

cat("P-value:", p_value, "\n")
cat("Is P-value < Significance Level?", p_value < alpha, "\n")

if (p_value > alpha) {
    cat("Null Hypothesis is accepted.\n")
} else {
    cat("Null Hypothesis is rejected.\n")
}

# This is call to execute the Wilcoxon Signed test for paired data. Again, we need
# a two-sided test/interval for getting the solution
result_w <- wilcox.test(car_with_additive, car_wo_additive, paired=TRUE, alternative = "two.sided", conf.level = 0.95)

print(result_w)

p_value <- result_w$p.value

cat("P-value:", p_value, "\n")
cat("Is P-value < Significance Level?", p_value < alpha, "\n")

if (p_value > alpha) {
    cat("Null Hypothesis is accepted.\n")
} else {
    cat("Null Hypothesis is rejected.\n")
}

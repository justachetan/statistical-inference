resin_1 <- c(0.046, 0.025, 0.014, 0.017, 0.043)
resin_2 <- c(0.038, 0.035, 0.031, 0.022, 0.012)
resin_3 <- c(0.031, 0.042, 0.020, 0.018, 0.039)

# Storing the entire data
resins <- c(resin_1, resin_2, resin_3)

# Storing the treatment class of each point
class <- c(rep(1, 5), rep(2, 5), rep(3, 5))

# Forming a data frame containing the information of data and it's treatment type
df <- data.frame(cbind(resins, class))

# Convert the treatment column of data frame to a factor/category
df$class <- factor(df$class, labels = c("Resin 1", "Resin 2", "Resin 3"))

# Perform One-way ANOVA on the data
result <- aov(resins~class, data = df)

print(summary(result))


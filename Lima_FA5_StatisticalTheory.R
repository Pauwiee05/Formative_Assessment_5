# PAUL CARLOS T. LIMA I
# FORMATIVE ASSESSMENT #5


# EXERCISE 8.18
# Given DATA
pData <- c(9, 12, 15)
prob <- rep(1/3, 3)

# Generate all possible samples of size 2 with replacement
sampleSpace <- expand.grid(sample1 = pData, sample2 = pData)

# Calculate the mean of "EACH" sample
sampleSpace$mean <- rowMeans(sampleSpace[, c("sample1", "sample2")])

# Calculate xbar (mean of sample means)
xbar <- mean(sampleSpace$mean)

# Calculate P(xbar) (probability distribution of sample means)
pxbar <- table(sampleSpace$mean) / length(sampleSpace$mean)

# Calculate the xbar, p(xbar), xbar * p(xbar), and xbar^2 * p(xbar)
x_bar <- c(9, 10.5, 12, 13.5, 15)
px_bar <- c(0.1111111, 0.2222222, 0.3333333, 0.2222222, 0.1111111)

xbar_pxbar <- x_bar * px_bar
xbar2_pxbar <- x_bar^2 * px_bar

## DISPLAY OF RESULTS ##

display_result <- cbind(sampleSpace, stringsAsFactors = FALSE)
print(display_result)

cat("Below Is the xbar[TOP PART] and p(xbar)[BOTTOM PART] \n")
print(pxbar)

cat("xbar * p(xbar): ", xbar_pxbar, "12 \n")
cat("xbar * p(xbar): ", xbar2_pxbar, "147 \n")






# EXERCISE 8.21

# Calculation of the MEAN 
mean_of_Population <- (3+7+11+15)/4
cat("Population Mean:", mean_of_Population, "\n")

# Calculation of the population standard deviation
x <- ((3-mean_of_Population)**2 + (7-mean_of_Population)**2 + (11-mean_of_Population)**2 + (15-mean_of_Population)**2)/4  
sd_of_Population <- sqrt(x)
cat("Population Standard Deviation:", sd_of_Population, "\n")

# Calculation of the MEAN of the Sampling Distribution of Means
cat("Mean of the Sampling Distribution of Means:", mean_of_Population, "\n")

# Calculation of the STANDARD DEVIATION of the Sampling Distribution of Means
sd_of_sdm <- sd_of_Population/sqrt(2) 
cat("Standard Deviation of the Sampling Distribution of Means:", sd_of_sdm, "\n")





# EXERCISE 8.34

# (a) Probability of less than 40% boys
prob_less_boys <- pbinom(80,  200, 0.5)
cat("Probability of less than 40% boys: ", prob_less_boys, "\n")

# (b) Probability of between 43% and 57% girls
prob_bet_girls <- pbinom(114, 200, 0.5) - pbinom(86 - 1, 200, 0.5)
cat("Probability of between 43% and 57% girls: ", prob_bet_girls, "\n")

# (c) Probability of more than 54% boys
prob_more_boys <- 1 - pbinom(108 - 1, 200, 0.5)
cat("Probability of between 43% and 57% girls: ", prob_more_boys, "\n")





#EXERCISE 8.49

# Given data
credited_hours <- c(6, 9, 12, 15, 18)
probakities <- c(0.1, 0.2, 0.4, 0.2, 0.1)

# Calculate mean (μ)
Vmean <- sum(credited_hours * probakities)

# Calculate variance (σ^2)
Varianze <- sum((credited_hours - Vmean)^2 * probakities)

# Display mean and variance
cat("Mean (μ):", Vmean, "\n")
cat("Variance (σ^2):", Varianze, "\n\n")

# Generate all possible samples of size 2 with replacement
sampleSpace2 <- expand.grid(credited_hours, credited_hours)

# Calculate means for each sample
sampleSpace_mean <- rowMeans(sampleSpace2)

# Calculate probabilities for each sample
sampleSpace_probakitties <- apply(sampleSpace2, 1, function(sample) prod(probakities[credited_hours %in% sample]))

# Display the header
cat("A     B       C     D\n")
cat("First Second  Mean  Probability\n")

# Display the results
for (i in 1:nrow(sampleSpace2)) {
  cat(sampleSpace2[i, 1], "  ", sampleSpace2[i, 2], "   ", round(sampleSpace_mean[i], 1), "   ", round(sampleSpace_probakitties[i], 4), "\n")
}





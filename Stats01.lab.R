
# Laboratory --------------------------------------------------------------

# Q1
# Load necessary package

# Generate the vector z
set.seed(123)  # for reproducibility
z <- exp(rnorm(n = 1000, mean = 0, sd = 0.1))

# Calculate arithmetic mean
arithmetic_mean <- mean(z)

# Calculate geometric mean
geometric_mean <- exp(mean(log(z)))

# Calculate median
median_value <- median(z)

# Print results
print(arithmetic_mean)
print(geometric_mean)
print(median_value)

#Q2 Histogram

# Load necessary libraries
library(tibble)
library(ggplot2)

set.seed(123)
z <- exp(rnorm(n = 1000, mean = 0, sd = 0.1))

# Convert to tibble
z_tibble <- tibble(value = z)

# Plot histogram
ggplot(z_tibble, aes(x = value)) + geom_histogram(binwidth = 0.02, fill = "salmon", color = "black") +
  labs(title = "Histogram of z", x = "Value", y = "Frequency") +
  theme_minimal()

library(patchwork)
#Q3 Vertical lines

# Compute statistics
arithmetic_mean <- mean(z)
geometric_mean <- exp(mean(log(z)))
median_value <- median(z)

# Convert to tibble
z_tibble <- tibble(value = z)

g1<-ggplot(z_tibble, aes(x = value)) +
  geom_histogram(linewidth = 0.02, fill = "salmon", color = "black") +
  geom_vline(aes(xintercept = arithmetic_mean), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = geometric_mean), color = "green", linetype = "dotted", size = 1) +
  geom_vline(aes(xintercept = median_value), color = "purple", linetype = "solid", size = 1) +
  labs(title = "Histogram of z with Mean, Geometric Mean, and Median",
       x = "Value", y = "Frequency") +
  theme_minimal()

#Q4 

# Create a tibble for comparison
comparison <- tibble(
  Measure = c("Arithmetic Mean", "Geometric Mean", "Median"),
  Value = c(arithmetic_mean, geometric_mean, median_value)
)
print(comparison)

#Q5

# Create reversed vector z_rev
z_rev <- -z + max(z) + 0.1

# Compute central tendency measures for z_rev
arithmetic_mean_rev <- mean(z_rev)
geometric_mean_rev <- exp(mean(log(z_rev)))
median_rev <- median(z_rev)

# Convert to tibble and plot
z_rev_tibble <- tibble(value = z_rev)

g2 <- ggplot(z_rev_tibble, aes(x = value)) +
  geom_histogram(linewidth = 0.02, fill = "salmon", color = "black") +
  geom_vline(aes(xintercept = arithmetic_mean_rev), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = geometric_mean_rev), color = "green", linetype = "dotted", size = 1) +
  geom_vline(aes(xintercept = median_rev), color = "purple", linetype = "solid", size = 1) +
  labs(title = "Histogram of z_rev",
       x = "Value", y = "Frequency") +
  theme_minimal()

g1/g2

# Variation measures ------------------------------------------------------

w <- rnorm(100, mean = 10, sd = 1)

# Convert to milligrams
m <- w * 1000


# SD and MAD --------------------------------------------------------------

# Calculate SD and MAD for w
sd_w <- sd(w)
mad_w <- mad(w)

# Calculate SD and MAD for m
sd_m <- sd(m)
mad_m <- mad(m)

print(sd_w)
print(sd_m)
print(mad_w)
print(mad_m)


# CV and MAD/Median -------------------------------------------------------


# Calculate CV and MAD/Median for w
cv_w <- sd(w) / mean(w)
mad_median_ratio_w <- mad(w) / median(w)

# Calculate CV and MAD/Median for m
cv_m <- sd(m) / mean(m)
mad_median_ratio_m <- mad(m) / median(m)

print(cv_w)
print(cv_m)
print(mad_median_ratio_w)
print(mad_median_ratio_m)



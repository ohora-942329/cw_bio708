# Ctrl (Command) + Shift + N is a hotkey for creating a new script file
# Ctrl + I is to fix indent
# Probability distribution

library(tidyverse)

# load csv data on R
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

# get a sense of data
df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width
                 center = 0.5) + # bin's center specification
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean


# normal distribution -----------------------------------------------------

# vector of x values
# seq() generate min to max values with specified numbers of elements or interval
# the following produce 100 elements
x <- seq(min(df_h0$height), max(df_h0$height), length = 100)

mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

tibble(y = pd, 
       x = x) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  labs(y = "Probability density",
       x = "Plant height")

## probability x < 10
p10 <- pnorm(10, mean = mu, sd = sigma)

## probability x < 20
p20 <- pnorm(20, mean = mu, sd = sigma)

## 10 < x < 20
p20_p10 <- p20 - p10

## make it 1 cm bins
x_min <- floor(min(df_h0$height)) # floor takes the integer part of the value
x_max <- ceiling(max(df_h0$height)) # ceiling takes the next closest integer
bin <- seq(x_min, x_max, by = 1) # each bin has 1cm

p <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - 
    pnorm(bin[i], mean = mu, sd = sigma)
}

# data frame for probability
# bin: last element [-length(bin)] was removed to match length
# expected frequency in each bin is "prob times sample size"
# "+ 0.5" was added to represent a midpoint in each bin
df_prob <- tibble(p, 
                  bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))

## overlay
df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width; must match the bin width used for probability
                 center = 0.5) +
  geom_point(data = df_prob,
             mapping = aes(y = freq,
                           x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            mapping = aes(y = freq,
                          x = bin),
            color = "salmon")


# poisson distribution ----------------------------------------------------

## data for plant counts
df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))
print(df_count)

## histogram
df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5,
                 center = 0)

# vector of x values
# create a vector of 0 to 10 with an interval one
# must be integer of > 0
x <- seq(0, 10, by = 1)

lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)

# figure
tibble(y = pm,
       x = x) %>% # data frame
  ggplot(aes(x = x,
             y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label

df_prob <- tibble(x = x,
                  y = pm) %>% 
  mutate(freq = y * nrow(df_count))

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq))



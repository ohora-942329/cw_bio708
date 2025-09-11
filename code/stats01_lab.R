
library(tidyverse)
library(patchwork)

# 7.3.1 Q1
z <- exp(rnorm(n = 1000, mean = 0, sd = 0.1))

# arithmetic mean
## using a built-in function
mu_z <- mean(z)

## manual
sum(z) / length(z)

# geometric
## taking the mean in log scale
mug_z <- exp(mean(log(z)))

## use prod() function
prod(z)^(1 / length(z))

# median
## built-in function
med_z <- median(z)

## manual (even number, so get two mid-point values)
z <- sort(z)
id1 <- length(z) / 2
id2 <- (length(z) + 1) / 2
0.5 * sum(c(z[id1], z[id2])) # slightly differ from median(z), so the function may employ something different

# 7.3.1 Q2, Q3, Q4
df_z <- tibble(x = z)

g1 <- df_z %>% 
  ggplot(aes(x = x)) +
  geom_histogram() +
  geom_vline(xintercept = mu_z) +
  geom_vline(xintercept = mug_z,
             color = "salmon") +
  geom_vline(xintercept = med_z,
             color = "skyblue")
  
# 7.3.1 Q5
## new vector
z_rev <- -z + max(z) + 0.1

## get mean values
mu_z_rev <- mean(z_rev)
mug_z_rev <- exp(mean(log(z_rev)))
med_z_rev <- median(z_rev)

## re-define df_z
df_z <- df_z %>% 
  mutate(y = z_rev)

## visualization
g2 <- df_z %>% 
  ggplot(aes(x = y)) +
  geom_histogram() +
  geom_vline(xintercept = mu_z_rev) +
  geom_vline(xintercept = mug_z_rev,
             color = "salmon") +
  geom_vline(xintercept = med_z_rev,
             color = "skyblue")

# this syntax works for ggplot objects after loading `library(patchwork)`
g1 / g2

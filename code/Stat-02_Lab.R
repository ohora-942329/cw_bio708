#sampling Lab 
library(tidyverse)
library(patchwork)

# full dataset -------------------------------------------------
df_h0 <-read_csv(here::here("data_raw/data_plant_height.csv"))
mu <- mean(df_h0$height)
var <- sum((df_h0$height - mean(df_h0$height))^2) / nrow(df_h0) 
# true mean and variance  -------------------------------------------------
#question 8.3 question 1
#use var() to estimate unbiased variance  

#for subset 50
mu50_i <- var50_i <- NULL # create empty objects
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 50) # random samples of 50 individuals
  
  # save mean for sample set i
  mu50_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var50_i[i] <- var(df_i$height) 
 
}

#for subset 100
mu100_i <- var100_i <- NULL
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 100) # random samples of 100 individuals
  
  # save mean for sample set 
  mu100_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var100_i[i] <- var(df_i$height)
}


# draw histograms for ----
df_est <- tibble(mu50 = mu50_i,
                 var50 = var50_i,
                 mu100 = mu100_i,
                 var100 = var100_i)

# histogram for mu
g50_mu <- df_est %>% 
  ggplot(aes(x = mu50)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for mu
g100_mu <- df_est %>% 
  ggplot(aes(x = mu100)) +
  geom_histogram() +
  geom_vline(xintercept = mu)
g100_mu/g50_mu

# histogram for var
g50_var <- df_est %>% 
  ggplot(aes(x = var50)) +
  geom_histogram() +
  geom_vline(xintercept = var )
plot(g50_var)
# histogram for Var
g100_var <- df_est %>% 
  ggplot(aes(x = var100)) +
  geom_histogram() +
  geom_vline(xintercept = var)
plot(g100_var)

g100_var/g50_var

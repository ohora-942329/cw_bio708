#sampling 
library(tidyverse)

# 10 individuals sampling -------------------------------------------------
# first dataset
h1 <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10, # a vector from 1 to 10 by 1
                height = h1, # height
                unit = "cm") # unit
# nrow() returns the number of rows
# while piping, "." refers to the dataframe inherited 
# i.e., nrow(.) counts the number of rows in df_h1

df_h1 <- df_h1 %>% 
  mutate(mu_h1 = mean(h1),
         var_h1 = sum((h1 - mu_h1)^2) / nrow(.))

print(df_h1)

#Set2
h2 <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h2,
                unit = "cm") %>% 
  mutate(mu_h2 = mean(h2),
         var_h2 = sum((h2 - mu_h2)^2) / nrow(.))

print(df_h2)
#parameter 
#it is some thing that is assumed but not directly measured like mean and variance
# load csv data on R
#install.packages("here")

df_h0 <-read_csv(here::here("data_raw/data_plant_height.csv"))


# show the first 10 rows
print(df_h0)

#true mean-average for 1000 individuals 

mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

print(mu)

print (sigma2)

## random sampling 
df_i <- df_h0 %>% 
  sample_n(size = 10) # size specifies the number of rows to be selected randomly

print(df_i)

# for reproducibility

set.seed(3)

mu_i <- var_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
}


#install.packages("patchwork") # install only once
library(patchwork)

df_sample <- tibble(mu_hat = mu_i, var_hat = var_i)

# histogram for mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for variance
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

# layout vertically
# possible only if "patchwork" is loaded
g_mu / g_var
## histogram for the variance 
g_var<- df_sample %>% 
  ggplot(aes(x=var_i))+
  geom_histogram()
geom_vline(xintercept = sigma2)
# layout vertically
# possible only if "patchwork" is loaded
g_mu / g_var

# for reproducibility
set.seed(3)

# redo simulations ----
mu_i <- var_i <- var_ub_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
  var_ub_i[i] <- var(df_i$height)
}
# draw histograms ----
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i,
                    var_ub_hat = var_ub_i)

# histogram for mu
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for variance
# scale_x_continuous() adjusts scale in x-axis
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

# histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

g_mu / g_var / g_var_ub

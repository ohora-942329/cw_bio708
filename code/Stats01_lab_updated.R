library(tidyverse)
library(patchwork)

# central tendency  -------------------------------------------------------

#Create a new vector z with length  
#1000 as exp(rnorm(n = 1000, mean = 0, sd = 0.1)), and calculate the arithmetic mean, geometric mean, and median.

z<-exp(rnorm(n = 1000, mean = 0, sd = 0.1))

#Q1
# arithmetic mean
## using a built-in function

mu_z <- mean(z)

## manual
sum(z) / length(z)

# Geometric mean
## taking the mean in log scale
mug_z <- exp(mean(log(z)))
## use prod() function
prod(z)^(1 / length(z))

# median
## built-in function
med_z<- median(z)
## manual (even number, so get two mid-point values)
z <- sort(z)
id1 <- length(z) / 2
id2 <- (length(z) + 1) / 2
0.5 * sum(c(z[id1], z[id2]))

#Q2
#Draw a histogram of z using functions tibble(), ggplot(), and geom_histogram().

df_z<-tibble(x=z)
g1 <- df_z %>% 
  ggplot(aes(x = x)) +
  geom_histogram() 
  
#Q3
#Draw vertical lines of arithmetic mean, geometric mean, and 
#median on the histogram with different colors using a function geom_vline().
g1 <- df_z %>% 
  ggplot(aes(x = x)) +
  geom_histogram() +
  geom_vline(xintercept = mu_z) +
  geom_vline(xintercept = mug_z,
             color = "red") +
  geom_vline(xintercept = med_z,
             color = "green")

#Q5; Create a new vector z_rev as -z + max(z) + 0.5, and repeat step 1 ??? 4.
 
z_rev<-z + max(z) + 0.1

## get mean values
mu_z_rev <- mean(z_rev)
mug_z_rev <- exp(mean(log(z_rev)))
med_z_rev <- median(z_rev)

## re-define df_z
df_z <- df_z %>% 
  mutate(y = z_rev)

##visualization 
g2<-df_z %>% 
  ggplot(aes(x=y))+
  geom_histogram()+
geom_vline(xintercept = mu_z_rev)+
  geom_vline(xintercept = mug_z_rev, color = "green")+
  geom_vline(xintercept = med_z_rev, color = "purple")

# this syntax works for ggplot objects after loading `library(patchwork)`
g1 / g2

  
  # Variation ---------------------------------------------------------------
w <- rnorm(100, mean = 10, sd = 1) # unit: g
head(w) # show first 10 elements in w

# Q1
## gram to milligram
m <- 1000 * w # unit: milligram

# Q2
## standard deviaion
s2_w <- sum((w - mean(w))^2) / length(w)
s_w <- sqrt(s2_w)

s2_m <- sum((m - mean(m))^2) / length(m)
s_m <- sqrt(s2_m)

## MAD
mad_w <- median(abs(w - median(w)))
mad_m <- median(abs(m - median(m)))

# Q3
## CV
cv_w <- s_w / mean(w) # numerator has a unit of [g], denom [g]
cv_m <- s_m / mean(m) # numerator has a unit of [mg], denom [mg]

## MAD / median
madr_w <- mad_w / median(w)
madr_m <- mad_m / median(m)




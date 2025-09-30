#ctrl (command) +shift + N is a hotkey for creating a new file
#ctrl + I is fix indent
# probability lab
library(tidyverse)
library(patchwork)
# load csv data on R

# Normal distribution -----------------------------------------------------
v<- rnorm(n=50, mean = 100, sd = 5)

#create a vector of bin

bin <-seq(floor(min(v)), ceiling (max (v)), by = 1) 

p <- NULL 
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mean(v), sd = sd(v)) - pnorm(bin[i], mean = mean(v), sd = sd(v))
}

df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5,
                  prob= p) %>% 
  mutate(freq = length(v)*prob)
#figure
df_v<- tibble(v=v)
df_v %>% 
  ggplot(aes(x = v)) + 
  geom_histogram(binwidth = 1, 
                 center = 0.5) +
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "blue") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "blue")

# poisson distribution ----------------------------------------------------
#for 1000 
x<-rpois(n=1000, lambda = 10)
df_count<- tibble(count= x)
lambda_hat<- mean(df_count$count)

bin<- seq(0, 
          max(x) +5,
          by=1)

##probability of observation 
p<-dpois(x, lambda = mean(x))
 ## data frame
df_prob<- tibble(x= x, y= p) %>% 
                  mutate(freq = y*nrow(df_count))

##figure 

df_count %>% 
  ggplot(aes(x=count))+
  geom_histogram(binwidth = 1, 
                 center = 0.5) +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq),
             color = "blue") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = x),
            color = "blue")


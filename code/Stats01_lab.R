library(tidyverse)
#Create a new vector z with length  
#1000 as exp(rnorm(n = 1000, mean = 0, sd = 0.1)), and calculate the arithmetic mean, geometric mean, and median.

z<-exp(rnorm(n = 1000, mean = 0, sd = 0.1))

#Q1

mean(z)
n_z <- length(z)
sum_z <- sum(z)
# Geometric mean
mu_z <- sum_z / n_z
exp(mean(log(z)))
print(mu_z)
med_z<- median(z)

#Q2
#Draw a histogram of z using functions tibble(), ggplot(), and geom_histogram().

df_z<-tibble(x=z)
z %>% 
  tibble() %>% 
  ggplot(aes(x=z))+
  geom_histogram()
#Q3
#Draw vertical lines of arithmetic mean, geometric mean, and 
#median on the histogram with different colors using a function geom_vline().
z %>% 
  tibble() %>% 
  ggplot(aes(x=z))+
  geom_histogram()+
geom_vline(xintercept = mu_z,
           color= "salmon")
  
#Q5
#new vector 
z_rev<-z + max(z) + 0.1
#get mean value
mu_z_rev<-mean(z_rev)
mug_z_rev<-exp(mean(log(z_rev)))
med_z_rev<-median(z_rev)
##re-define df_z
df_z<-df_z %>% 
  mutate(y=z_rev)
##vistualization 
g2<-df_z %>% 
  ggplot(aes(x=y))+
  geom_histogram()+

# Variation ---------------------------------------------------------------
w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w
#gram to miligram
m<-1000
#q2
s2_w<- sum(w-mean(w))^2/length(w)
s_w<-sqrt(s2_w)
s2_m<-sum(m-mean(m))^2/length(m)
s_m<-sqrt(s2_m)
##MAD
mad_w<-median(abs(w-medaian(w)))
mad_m<-median(abs(m-median(m)))
#Q3 CV
cv_w<-s_w/mean(w)
cv_m<-s_m/mean(m)
#madr_w<-mad_w/median()




  

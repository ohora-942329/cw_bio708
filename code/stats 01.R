library(tidyverse)

# Central tendency -------------------------------------------------------

# hotkey for a section ctrl 
# Arithmetic mean
#calculate the arithmetic mean of v-x using length() 
 v_x<-rnorm(10)
 v_x
 mu_xsum(v_x)/length (v_x)
 # geometric mean
 #use prod( ), length(),^
 v_y<- runif(10, min= 10, max = 20)
 v_y
 prod(v_y)^ (1/length(v_y))
 exp(mean(log(v_y)))
 #Median
 v_z<- runif(9, min= 10, max = 20)
v_z<- sort(v_z)
v_z
index<- (length(v_z) + 1)/2
v_z[index]
median(v_z)

# Variance measure --------------------------------------------------------

# Variance 
# use sum(), mean() and length() to define the measure

v_a <- rnorm(100)
s2<- sum((v_a - mean(v_a))^2)/length(v_a)
s <- sqrt(s2)

#interquantile range
a_l<- quantile(v_a, probs = 0.25)
a_h<- quantile(v_a, probs = 0.75)
(iqr<- abs(a_h - a_l))
#MAD
median((abs(v_a - median(v_a))))
# coefficient of variation 
# use v_b, s and mean() of v_b to define CV
v_b <- runif(100, min = 10, max= 20)
s2<- sum((v_b - mean(v_b))^2)/length(v_b)
s <- sqrt(s2)
(cv<- s/mean(v_b))

#MAD/median
mad<- median(abs(v_b -median(v_b)))
med<- median(v_b)
mad2med<-mad/med


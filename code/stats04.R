pacman::p_load(tidyverse, 
               pactchwork,
               here)
## read data
df_f1<-read_csv(here("data_raw/data_fish_length.csv"))
print(df_f1)
# unique returns unique values as a vector
unique(df_f1$lake)
# distinct returns unique values as a tibble
distinct(df_f1, lake)
##visualization

  # group mean and sd

  df_f1_mu <- df_f1 %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            sd_l = sd(length)) # summarize with sd()

# plot
# geom_jitter() plot data points with scatter
# geom_segment() draw lines
# geom_point() draw points
  
df_f1 %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_jitter(width = 0.1, 
              height = 0, 
              alpha = 0.25) + 
  geom_segment(data = df_f1_mu, 
               aes(x = lake,
                   xend = lake,
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l)) +
  geom_point(data = df_f1_mu, 
             aes(x = lake,
                 y = mu_l),
             size = 3) +
  labs(x = "Lake", # x label
       y = "Fish body length (cm)") # y label
## perform t-test
# subset lake a
x <- df_f1 %>%
  filter(lake == "a") %>%  
  pull(length)
print(x)
# subset lake b
y <- df_f1 %>%
  filter(lake == "b") %>%
  pull(length)
print(y)

# details in t-test -------------------------------------------------------

mu_x<-mean(x)
mu_y<-mean(y)
mu_x-mu_y

# get some key statistics from each groups (group mean, variance, and sample size)
df_t <- df_f1 %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean(length),
            var_l = var(length), 
            n = n()) 

print(df_t)
t.test(x, y, var.equal = TRUE)

# pull values as a vector

v_mu <- pull(df_t, mu_l)
v_var <- pull(df_t, var_l)
v_n <- pull(df_t, n)

var_a <- ((v_n[1] - 1)/(sum(v_n) - 2)) * v_var[1] 
print(var_a)
var_b<- ((v_n[2] - 1)/(sum(v_n) - 2)) * v_var[2]
print(var_b)
var_p<-var_a+var_b
print(var_p)

## get t-statistics 

t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))

print(t_value)

## Null distribution 
# produce 500 values from -5 to 5 with equal interval

x <- seq(-5, 5, length = 500)
print(x)
# probability density of t-statistics with df = sum(v_n) - 2
y <- dt(x, df = sum(v_n) - 2)

# draw figure

tibble(x, y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = t_value,
             color = "green") + 
  geom_vline(xintercept = abs(t_value),
             color = "green") +
  labs(y = "Probability density",
       x = "t-statistic")
p_lower<-pt(q=t_value,df=98)
print(p_lower)
p_higher<- 1-pt(q=abs(t_value), df=98)
print(p_higher)
p_value<-p_lower + p_higher
print(p_value)

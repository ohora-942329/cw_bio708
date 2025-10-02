#ctrl (command) +shift + N is a hotkey for creating a new file
#ctrl + I is fix indent
# probability lab
library(tidyverse)
library(patchwork)
# 10.5.1. influence of sample size 
# Create vectors with rnorm(), sample size 10, mean 10, sd 5
xs <- rnorm(n = 10, mean = 10, sd = 5)
print(xs)
ys <- rnorm(n = 10, mean = 12, sd = 5)
print(ys)
xl <- rnorm(n = 100, mean = 10, sd = 5)
print(xl)
yl <- rnorm(n = 100, mean = 12, sd = 5)
print(yl)

# Welch's t-test (var.equal = TRUE forces pooled variance t-test)
t_xs_ys <- t.test(xs, ys, var.equal = TRUE)
print(t_xs_ys)
t_xl_yl <- t.test(xl, yl, var.equal = TRUE)
print(t_xl_yl)

#10.5.2 difference and uncertainty 

a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)
##create a tibble (use either rep() or pivot_longer()
df_value <- tibble(value= c(a1, a2, b1, b1),
  group = c(rep("a1", length(a1)),
            rep("a2", length(a2)),
            rep("b1", length(b1)),
            rep("b2", length(b2))
  )
)

#create figures
df_mu<- df_value %>% 
  group_by(group) %>% 
  summarize(mu= mean(value),
            sd= sd(value))

df_value %>% 
  filter(group %in% c("a1", "a2")) %>% 
  ggplot(aes(x = group,
             y = value)) +
  geom_jitter(width= 0.1,
              alpha=0.5) +
  geom_segment(data = df_mu %>% 
                 filter(group %in% c("a1", "a2")),
               aes(x = group,
                   xend = group,
                   y = mu - sd,
                   yend = mu + sd)) +
  geom_point(data = df_mu %>% 
               filter(group %in% c("a1", "a2")),
             aes(x= group,
                 y= mu))

t.test(a1, a2)
t.test(b1, b2)

#install.packages("tidyverse")
library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

# Referesher --------------------------------------------------------------


# Exercise ----------------------------------------------------------------

#filter iris-sub to those with Sepal.Length greater than 5
#assign to ds_g5
df_g5<-filter(iris_sub,
              Sepal.Length > 5)

#exercise 2
#select columns of Sepal.Length and Petal.width from iris_sub
#assign to df_sp
(df_sp<-iris_sub %>%
    select(c(Sepal.Length, Petal.Width)))


#execrsise 3 
#arrange rows by Petal.Width in iris_sub
#assign to df_arrange

(df_arrange <- iris_sub %>%
  arrange(Petal.Width))

#execrsise 4
#do excersise 1-3 at once with pipes 
#assign to df_master

df_master <-iris_sub %>%
  filter(Sepal.Length> 5) %>% 
  select(c(Sepal.Length, Petal.Width)) %>% 
  arrange(Petal.Width)

# ggplot ------------------------------------------------------------------

(g_example <- ggplot(data = iris,
       mapping = aes(x = Sepal.Length,
                    y = Sepal.Width)) +
  geom_point())

# with pipe
 (g_example<- iris %>%
  ggplot(mapping = aes(x = Sepal.Length,
                       y = Sepal.Width)) +
  geom_point())
# color
(g_col <- iris %>%
    ggplot(mapping = aes(x = Sepal.Length,
                         y = Sepal.Width,
                         color = Species)) +
    geom_point())
#
(g_scol<- iris %>%
  ggplot(mapping = aes(x= Sepal.Length,
                       y= Sepal.Width)) +
       geom_point(color = 'salmon'))
## Line plot
# sample data
df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)

# basic plot
df0 <- tibble(x= rep(1:50, 3),
              y= x*2)
df0 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line()
# histogram 
# basic plot; bins = 30 by default
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram()
#histogram  colored by species 
iris %>% 
  ggplot(aes(x = Sepal.Length,
             color= Species)) +
  geom_histogram()
#histogram filled by species 
iris %>% 
  ggplot(aes(x = Sepal.Length,
             fill = Species)) +
  geom_histogram()
# boxplot 
iris %>%
  ggplot(aes(y= Sepal.Length,
         x= Species)) +
  geom_boxplot()
#Boxplot filled by species

iris %>%
  ggplot(aes(y= Sepal.Length,
             x= Species,
             fill= Species)) +
  geom_boxplot()
# use multiple layers
#example 1
iris %>%
  ggplot(aes(y= Sepal.Length,
             x= Species,
             fill= Species)) +
  geom_boxplot()+
  geom_point()
# example 2

iris %>%
  ggplot(aes(y= Sepal.Length,
             x= Species,
             fill= Species)) +
  geom_boxplot()+ geom_jitter(alpha= 0.25)



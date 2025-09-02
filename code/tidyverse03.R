#install.packages("tidyverse")
library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

# refresher ---------------------------------------------------------------

# exercise 1
# filter iris_sub to those with Sepal.Length greater than 5
# assign to `df_g5`

(df_g5 <- filter(iris_sub,
                 Sepal.Length > 5))

(df_g5 <- iris_sub %>% 
    filter(Sepal.Length > 5))

# exercise 2
# select columns of Sepal.Length and Petal.Width from iris_sub
# assign to `df_sp`

(df_sp <- iris_sub %>% 
    select(c(Sepal.Length, Petal.Width)))

# exercise 3
# arrange rows by Petal.Width in iris_sub
# assign to `df_arrange`

(df_arrange <- iris_sub %>% 
    arrange(Petal.Width))

# exercise 4
# do exercise 1-3 at once with pipes
# assign to `df_master`

df_master <- iris_sub %>% 
  filter(Sepal.Length > 5) %>% 
  select(c(Sepal.Length, Petal.Width)) %>% 
  arrange(Petal.Width)

# extra
# calculate mean Petal.Width for each Species separately
# use group_by() and summarize() functions

(df_means <- iris_sub %>% 
    group_by(Species) %>% 
    summarize(mean_pw = mean(Petal.Width)))

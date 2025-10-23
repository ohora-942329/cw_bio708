# Ctrl (Command) + Shift + N is a hotkey for creating a new script file
# Ctrl + I is to fix indent
# regression lab

pacman::p_load(tidyverse,
               patchwork,
               here)



# exercise 1 --------------------------------------------------------------

## develop regression models for each species separately
## response: Sepal.Length or Sepal.Width!
## explanatory: Petal.Width

## manual approach
df_ver <- iris %>% 
  filter(Species == "versicolor")

df_set <- iris %>% 
  filter(Species == "setosa")

df_vir <- iris %>% 
  filter(Species == "virginica")

m_ver <- lm(Sepal.Width ~ Petal.Width,
            df_ver)

m_set <- lm(Sepal.Width ~ Petal.Width,
            df_set)

m_vir <- lm(Sepal.Width ~ Petal.Width,
            df_vir)

## for loop example
v_sp <- unique(iris$Species)
list_m <- list(NULL)

for (i in 1:length(v_sp)) {
  df_i <- iris %>% 
    filter(Species == v_sp[i])
  
  list_m[[i]] <- lm(Sepal.Width ~ Petal.Width,
                    data = df_i)
}


# exercise 2 --------------------------------------------------------------

## select one of the species in `iris`, and compare between:
## model 1: Petal.Width only
## model 2: Petal.Width and Petal.Length

m_ver_2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
              df_ver)

## compare the following
## regression estimate of `Petal.Width`
## R^2 values (which model is better?)

summary(m_ver) # R2 = 0.44
summary(m_ver_2) # R2 = 0.444

# exercise 3 --------------------------------------------------------------

## if you are done 1 & 2...
## try this one - create a new random variable x
v_x <- rnorm(nrow(iris), mean = 0, sd = 1)
iris <- iris %>% 
  mutate(x = v_x)

## `x` is a random variable, so no relevance to iris data
## include this `x` in one of the models you developed in exercise 1
## then investigate whether x improves R^2 or not.

df_ver <- iris %>% 
  filter(Species == "versicolor")

m_ver <- lm(Sepal.Width ~ Petal.Width,
            df_ver)

m_ver_w_x <- lm(Sepal.Width ~ Petal.Width + x,
                df_ver)

summary(m_ver) # 0.44
summary(m_ver_w_x) # 0.441

plot(Sepal.Width ~ x, df_ver)


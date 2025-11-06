#ctrl (command) +shift + N is a hotkey for creating a new file
#ctrl + I is fix indent
# #GLM Laboratory
pacman::p_load(tidyverse, 
               patchwork)
## fish data
url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- readr::read_csv(url)
head(df_fish)


# 14.4.1 GLM exercise -----------------------------------------------------

##Q1. Identify which predictor variable(s) significantly influence the response variable
mod_fish <- glm(n_sp ~ distance + cat_area + hull_area,
                family = "poisson",
                data = df_fish)
summary(mod_fish)

## car dataset
## Which variables significantly influence the probability that a car has a manual transmission?
mod_am <- glm(am ~ mpg + hp + wt,
                 family = "binomial",
                 data = mtcars)
summary(mod_am)

plot (am~ wt,
      mtcars)

##Compare with Gaussian GLM, it will get different results 
mod_gauss <- glm(am ~ mpg + hp + wt,
                 family = gaussian(),
                 data = mtcars)
summary(mod_gauss)

# 14.4.2 Effect size ------------------------------------------------------
# create standardized variable 

df_fish <- df_fish %>%
  mutate(
    std_dist = scale(distance)[, 1],
    std_cat  = scale(cat_area)[, 1],
    std_hull = scale(hull_area)[, 1]
  )

mod_fish_std <- glm(n_sp ~ std_dist + std_cat + std_hull,
                    family = "poisson",
                    data = df_fish)
summary(mod_fish_std)

## compare coefs
coef(mod_fish)
coef(mod_fish_std)

# 14.4.3 Offset term ------------------------------------------------------

##lode the data

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_offset.csv"
df_offset <- readr::read_csv(url)
head(df_offset)

##Explore relationships with plots

library(ggplot2)

# Count vs Nitrate

g1<- ggplot(df_offset, aes(x = nitrate, y = count)) +
  geom_point() +
  theme_bw()

# Count vs Area

g2<- ggplot(df_offset, aes(x = area, y = count)) +
  geom_point() +
  theme_bw()

# Count/Area vs Nitrate

g3<- ggplot(df_offset, aes(x = nitrate, y = count / area)) +
  geom_point() +
  theme_bw()

g1 +g2 +g3

 ## Gaussian trial 

df_offset <- df_offset %>% 
  mutate(density = count / area)

glm(density ~ nitrate,
    data = df_offset,
    family = "poisson")

##(a) Model without offset term

mod_no_offset <- glm(count ~ nitrate,
                     family = "poisson",
                     data = df_offset)
summary(mod_no_offset)

##(b) Model with offset term

mod_with_offset <- glm(count ~ nitrate + offset(log(area)),
                       family = "poisson",
                       data = df_offset)
summary(mod_with_offset)

# 14.4.4 Overdispersion ---------------------------------------------------

##Load the dataset

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_tadpole.csv"
df_tadpole <- readr::read_csv(url)

head(df_tadpole)

##Plot to visualize the relationships

##library(ggplot2)

# (a) Tadpole vs Aquatic vegetation

a<- ggplot(df_tadpole, aes(x = aqveg, y = tadpole)) +
  geom_point() +
  theme_bw() +
  labs(x = "Aquatic vegetation cover", y = "Tadpole count")

# (b) Tadpole vs Permanence

b<- ggplot(df_tadpole, aes(x = permanence, y = tadpole)) +
  geom_point() +
  theme_bw() +
  labs(x = "Pond permanence (days)", y = "Tadpole count")
a + b 

##GLM distribution 
## 1. If overdispersion is present (variance > mean),
##Fit the Poisson GLM

mod_tadpole <- glm(tadpole ~ aqveg + permanence,
                   family = "poisson",
                   data = df_tadpole)
summary(mod_tadpole)

## negative binomial regression 

library(MASS)

mod_tadpole_nb <- glm.nb(tadpole ~ aqveg + permanence, data = df_tadpole)
summary(mod_tadpole_nb)


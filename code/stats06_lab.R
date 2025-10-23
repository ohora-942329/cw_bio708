#ctrl (command) +shift + N is a hotkey for creating a new file
#ctrl + I is fix indent
# #Regression lab 
pacman::p_load(tidyverse, 
               pactchwork,
               here)

head(iris)

# Exercise 1 -------------------------------------------------------------
# Split into three separate data frames and develop regression model for each species separately 
df_set<-iris %>% 
  filter(Species == "setosa")
df_ver<-iris %>% 
  filter(Species == "versicolor")
df_vir<-iris %>% 
  filter(Species == "virginica")
# regression model 

m_set<- lm(Sepal.Width ~ Petal.Width, 
           df_set)
m_ver <- lm(Sepal.Width ~ Petal.Width,
            df_ver)
m_vir <- lm(Sepal.Width ~ Petal.Width, 
            df_vir)

summary(m_set)
summary(m_ver)
summary(m_vir)

# Exercise 2 -------------------------------------------------------------

## select one of the species and in "iris" and compare between:
## Model 1: petal.width only 
## model2: petal.width and petal.length 
m_set_2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length, 
              df_set)
m_ver_2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length, 
              df_ver)
m_vir_2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length, 
              df_vir)
## compare the following 
## regression estimate of petal.width 
##R^2 value (which model is better?)
summary(m_ver)# R^2= 0.4409
summary(m_ver_2)#R^2= 0.444

# exercise 3; create new random variable x ---------------------------------
x<-rnorm(nrow(iris), mean=0, sd=1)
iris<-iris %>% 
  mutate(x=x)
## 'x' is random variable, so no 
df_ver<- iris %>% 
  filter(Species=="versicolor")
m_ver<-lm(Sepal.Width~Petal.Width,
          df_ver)
m_ver_w_x<-lm(Sepal.Width~Petal.Width+x,
          df_ver)
summary(m_ver)
summary(m_ver_w_x)
plot(Sepal.Width~x,df_ver)

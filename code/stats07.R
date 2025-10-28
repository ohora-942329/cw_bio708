#ctrl (command) +shift + N is a hotkey for creating a new file
#ctrl + I is fix indent
# #Linear Model 
pacman::p_load(tidyverse, 
               pactchwork,
               here)

# t-test vs lm ------------------------------------------------------------

df_fl <- read_csv(here("data_raw/data_fish_length.csv"))
m<-lm(length~lake,
      data = df_fl)
summary(m)
#calculate gurp mean for length
v_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)
#mean of lake "a"
v_mu[1]
#mean of lake "b"
v_mu[2]
# mu_b - mu_a: should be identical to slope
v_mu[2] - v_mu[1]

#mean length for lake b
sum(coef(m))
#look into result
summary(m)
#compare with t-test result
a<-df_fl %>% 
filter (lake=="a") %>% 
  pull(length)

b<-df_fl %>% 
  filter (lake=="b") %>% 
  pull(length)
  
t.test(x =a,
        y =b,
       var.equal=TRUE)

# ANOVA vs lm -------------------------------------------------------------

df_anova <- read_csv(here("data_raw/data_fish_length_anova.csv"))
m1<-lm(length~ lake,
       data= df_anova)
#get group means
v_mu_anova<- df_anova %>% 
  group_by(lake) %>% 
  summarise(mu_l= mean(length)) %>% 
  pull(mu_l)
# this corresponds to "intercepts"
v_mu_anova[1]
#this corresponds to "
v_mu_anova [2]-v_mu_anova[1]
# this corresponds to "
v_mu_anova[3]-v_mu_anova[1]
#compare with aov()
m_aov <- aov(length ~ lake,
             data = df_anova)
print(m_aov)

# ANCOVA ------------------------------------------------------------------
#ancova example

m2<- lm(Sepal.Length~Sepal.Width + Species,
       data = iris)

#visualization
m_iris<-lm(Petal.Length~Petal.Width + Species,
           data = iris)

df_pred<- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                         max(iris$Petal.Width),
                         length= 100),
       times =3),
       Species = rep(unique(iris$Species),
                               each = 100))
#get predicted values 
y_pred <- predict(m_iris,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

print(df_pred)

#get visual out put
iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred)) 

# interaction -------------------------------------------------------------
## how to include interaction 
m_int<-lm(Petal.Length~ Petal.Width*Species,
   data = iris)
summary(m_int)
#identical model of different expression 




#ctrl (command) +shift + N is a hotkey for creating a new file
#ctrl + I is fix indent
##Generalized Linear Model (GLM)
pacman::p_load(tidyverse, 
               pactchw,here)

# Count data --------------------------------------------------------------

df_count <- read_csv(here("data_raw/data_garden_count.csv"))
print(df_count)

nrow(df_count)

m_normal <- lm(count ~ nitrate,
               df_count)

summary(m_normal)

alpha <- coef(m_normal)[1] # intercept
beta <- coef(m_normal)[2] # slope

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta)

## random number from poisson distribution 

(y<-rpois(n=10, lambda = 2))
## apply GLM function 

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")# what probability distribution to be used 
summary(m_pois)
df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_abline(intercept = coef(m_pois)[1],
              slope = coef(m_pois)[2])

# Poisson regression -------------------------------------------------------

df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                                max(df_count$nitrate),
                                length = 100))
print(df_pred)
y_pred<- predict(m_pois,
         newdata= df_pred) %>%  exp()

df_pred <- df_pred %>% 
  mutate(df_pred,
         y_pred)

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y_pred),
            linetype = "dashed") +
  geom_line(data = df_pred,
            aes(y = y_pred),
            color = "salmon")  
##compare the the data 
summary(m_normal)
summary(m_pois)

# Proportional data  ------------------------------------------------------

df_mussel <- read_csv(here("data_raw/data_mussel.csv"))
print(df_mussel)

df_mussel <- df_mussel %>% 
  mutate(prop_fert = n_fertilized / n_examined)
##plot
df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  labs(y = "Proportion of eggs fertilized",
       x = "Mussel density")
##cbind() is needed for binomial 
cbind(df_mussel$n_fertilized, df_mussel$n_examined)
##binomial model
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")
summary(m_binom)

##how logit function works 

df_test<-tibble(logit_x = seq(-10, 10, length=100),
                x = exp(logit_x) / (1 + exp(logit_x)))

df_test %>% 
  ggplot(aes(x = logit_x,
             y = x)) +
  geom_point()+
  geom_line()
                
                

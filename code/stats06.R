#ctrl (command) +shift + N is a hotkey for creating a new file
#ctrl + I is fix indent
# Regression 
pacman::p_load(tidyverse, 
               pactchwork,
               here)

# Read data ---------------------------------------------------------------

df_algae <- read_csv(here::here("data_raw/data_algae.csv"))
print(df_algae)

## visualize 

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point()

# Regression analysis -----------------------------------------------------

m <- lm(biomass ~ conductivity,
        data = df_algae)

summary(m)

alpha <- coef(m)[1]
print (alpha)
beta <- coef(m)[2]
beta
df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) # draw the line

# get t-value -------------------------------------------------------------

## extract coefficients
theta <- coef(m)

## extract standard errors

se <- sqrt(diag(vcov(m)))

beta/se[2]

# t-value
t_value <- theta / se

print(t_value)

# for intercept
p_alpha <- (1 - pt(t_value[1], df = 48)) + pt(-t_value[1], df = 48) #df is sample size -number of parameter (slop and intercept)
print (p_alpha)
# for slope
p_beta <- (1 - pt(t_value[2], df = 48)) + pt(-t_value[2], df = 48)

print(p_alpha)

# Coefficient of determination ----------------------------------------------

eps <- resid(m)
print(eps)

df_algae <- df_algae %>% 
  mutate(eps = eps)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) + 
  geom_segment(aes(x = conductivity, 
                   xend = conductivity, # to draw vertical line, both x and xend have same value x
                   y = biomass, # start-coordinate y
                   yend = biomass - eps), # end-coordinate y
               linetype = "dashed")

ss <- sum(resid(m)^2)
ss
ss0 <- sum((df_algae$biomass - mean(df_algae$biomass))^2)
ss0
r_sq<-1-(ss/ss0)
r_sq
## compare with lm output
summary (m)

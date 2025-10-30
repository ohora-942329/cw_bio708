# Ctrl (Command) + Shift + N is a hotkey for creating a new script file
# Ctrl + I is to fix indent
# linear model lab

pacman::p_load(tidyverse,
               patchwork,
               here)

# normality assumption ----------------------------------------------------

## develop a linear model
m_tooth <- lm(len ~ supp * dose,
              data = ToothGrowth)

# # or
# m_tooth <- lm(len ~ supp + dose + supp:dose,
#               data = ToothGrowth)

## test normality assumption
eps <- resid(m_tooth)
shapiro.test(eps)

# m <- lm(len ~ supp + dose,
#         data = ToothGrowth)
# shapiro.test(resid(m))

# model interpretation ----------------------------------------------------

df_pred <- ToothGrowth %>%
  group_by(supp) %>%
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))

y_pred <- predict(m_tooth,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y = y_pred)

ToothGrowth %>% 
  ggplot(aes(x = dose,
             y = len,
             color = supp)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y))

# multicollinearity -------------------------------------------------------

## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

## true regression coefficients
b <- c(0.05, 1.00)

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))

## create y vs x figures
g_x1 <- df_y %>% 
  ggplot(aes(x = x1,
             y = y)) +
  geom_point()

g_x2 <- df_y %>% 
  ggplot(aes(x = x2,
             y = y)) +
  geom_point()

g_x1 + g_x2

## create lm() model with x1 and x2 as predictors
## inspect the results
m <- lm(y ~ x1 + x2,
        data = df_y)

summary(m)

## look at the relationship between x1 and x2
df_y %>% 
  ggplot(aes(x = x1,
             y = x2)) +
  geom_point()

with(df_y,
     cor(x1, x2))



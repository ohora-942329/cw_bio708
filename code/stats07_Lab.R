#ctrl (command) +shift + N is a hotkey for creating a new file
#ctrl + I is fix indent
##linear model Laboratory
pacman::p_load(tidyverse, 
               pactchw,here)

# 13.4.1 Normality Assumption ---------------------------------------------

data("ToothGrowth")

#1. Build the linear model
m_tooth <- lm(len ~ supp * dose, data = ToothGrowth)
summary(m_tooth)

#Which should we test for normality?
#residuals 

eps<-resid(m_tooth) #if p-value > 0.05 → residuals are approximately normal
#p-value < 0.05 → residuals are not normal

shapiro.test(residuals(m_tooth))

# 13.4.2 Model Interpretation ---------------------------------------------

#Create a new prediction grid
df_pred <- ToothGrowth %>%
  group_by(supp) %>%
  reframe(dose = seq(min(dose), max(dose), length = 100))

# Add model predictions
df_pred <- df_pred %>%
  mutate(pred_len = predict(m_tooth, newdata = df_pred))

#Plot observed and predicted values
library(ggplot2)

ggplot(ToothGrowth, aes(x = dose, y = len, color = supp)) +
  geom_point() +  # observed values
  geom_line(data = df_pred, aes(y = pred_len), size = 1) +  # predicted trend
  labs(
    x = "Dose (mg/day)",
    y = "Tooth Length"
  ) +
  theme_minimal()


# 13.4.3. Multicollinearity -----------------------------------------------

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
print(df_y)

# y vs x1
ggplot(df_y, aes(x = x1, y = y)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Response (y) vs Predictor (x1)",
       x = "x1", y = "y")
# y vs x2
ggplot(df_y, aes(x = x2, y = y)) +
  geom_point() +
  theme_minimal() +
  labs(x = "x2", y = "y")
#x1 and x2 are strongly positively correlated (ρ = 0.9)
#y depends on both x1 and x2

m_y <- lm(y ~ x1 + x2, data = df_y)
summary(m_y)

# Scatter plot of x1 and x2
ggplot(df_y, aes(x = x1, y = x2)) +
  geom_point() +
  theme_minimal() +
  labs(x = "x1", y = "x2")
with(df_y,
     cor(x1,x2))

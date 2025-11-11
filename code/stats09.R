#ctrl (command) +shift + N is a hotkey for creating a new file
#ctrl + I is fix indent
##Model comparison
pacman::p_load(tidyverse, 
               pactchw,here)

# Least squares -----------------------------------------------------------

set.seed(1)

# hypothetical sample size
n <- 100

# true intercept and slope
b <- c(0.1, 0.5)

# simulated predictor 
x1 <- rnorm(n = n, mean = 0, sd = 1)

# create a design matrix

X <- model.matrix(~x1)


# get simulated y
(x<-model.matrix(~x1))
# y = X %*% b equals y = b[1] + b[2] * x
# recall linear algebra

y_hat <- X %*% b

plot(y_hat~x1)

# add normal errors
y <- rnorm(n = n, mean = y_hat, sd = 0.5)

# plot
df0 <- tibble(y = y, x1 = x1)

df0 %>% 
  ggplot(aes(y = y,
             x = x1)) + 
  geom_point()

## fit a model to the simulated data

lm(y~ x1,
   data = df0)


# Likelihood  --------------------------------------------------------------
## probability of 
dpois(3, lambda = 3.5)
## try other different number
dpois(1, lambda = 3.5)
dpois(4, lambda = 3.5)
dpois(10, lambda = 3.5)
## try different lambda values 
lambda<-seq(0, 10, by= 0.1)
pr <- dpois(3, lambda = lambda)
df_pois <- tibble(y = 3,
                  lambda = lambda,
                  pr = pr)
df_pois %>% 
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_point() +
  geom_line() +
  labs(x = "lambda",
       y = "Pr(k = 3)")
df_pois %>% 
  arrange(desc(pr))
#probability of observing 3, 2, 5 

pr <- dpois(c(3, 2, 5), lambda = 3)

print(pr)
#probability of observing 3, 2, 5 simultaneously
prod(pr)
#lambda 3,2, 5
y<-c(3, 2,5)
lambda<- seq(0,10, by= 0.01)

pr <- sapply(X = lambda,
             FUN = function(z) prod(dpois(y, lambda = z)))

df_pois <- tibble(lambda = lambda,
                  pr = pr)

  # plot
  df_pois %>% 
    ggplot(aes(x = lambda,
               y = pr)) +
    geom_line() +
    labs(y = "Likelihood")
  
  df_pois %>% 
    arrange(desc(pr))
  print()
  
  mean(c(3, 2, 5))
  
  
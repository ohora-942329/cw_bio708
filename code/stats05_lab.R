# Ctrl (Command) + Shift + N is a hotkey for creating a new script file
# Ctrl + I is to fix indent
# ANOVA

pacman::p_load(tidyverse,
               patchwork,
               here)


# anova practice with PlantGrowth -----------------------------------------

## drawing a figure
df_pg <- as_tibble(PlantGrowth)

df_pg %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_violin(draw_quantiles = 0.5,
              alpha = 0.8) +
  geom_jitter(width = 0.1)

## perform anova
m <- aov(weight ~ group,
         data = df_pg)

summary(m)

# power analysis ----------------------------------------------------------

## pwr package
## n group = 3
## f = 0.5
## power = 0.8
pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)

## change k, f, power, one at a time
## see how these changes affect the number of samples you may need
## - increase in k (number of groups)
pwr::pwr.anova.test(k = 5,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)

## - decrease in f
pwr::pwr.anova.test(k = 5,
                    f = 0.25,
                    sig.level = 0.05,
                    power = 0.8)

## - increase in power
pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.9)

## - leave power blank
pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.5,
                    sig.level = 0.05)

## - try different levels of k, n, f
pwr::pwr.anova.test(k = 5,
                    n = 5,
                    f = 0.5,
                    sig.level = 0.05)

pwr::pwr.anova.test(k = 3,
                    n = 100,
                    f = 0.5,
                    sig.level = 0.05)

pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.25,
                    sig.level = 0.05)

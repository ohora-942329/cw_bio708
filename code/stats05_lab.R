#ctrl (command) +shift + N is a hotkey for creating a new file
#ctrl + I is fix indent
# #ANOVA 
pacman::p_load(tidyverse, 
               pactchwork,
               here)

# ANOVA Laboratory --------------------------------------------------------

              
# Load and view the database

data("PlantGrowth")
head(PlantGrowth)
summary(PlantGrowth)
str(PlantGrowth)

# Fit ANOVA model
anova_lab <- aov(weight ~ group, data = PlantGrowth)

# View ANOVA table
summary(anova_lab)
#Create Visualizations

ggplot(PlantGrowth, aes(x = group, y = weight, fill = group)) +
  labs(title = "Plant Weight by Group",
       x = "Group",
       y = "Weight") +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2,
              fill = "steelblue") + 
  geom_jitter(width = 0.1) + 
  theme_bw()

# power analysis  ---------------------------------------------------------


#pwr package
install.packages("pwr")  
library(pwr)
pwr::pwr.anova.test(k=3,
                    f= 0.5,
                    sig.level = 0.05,
                    power= 0.8)
## change k, f, power one at a time
# increase in k
pwr::pwr.anova.test(k=5,
                    f= 0.5,
                    sig.level = 0.05,
                    power= 0.8)

# decrease in f
pwr::pwr.anova.test(k=3,
                    f= 0.25,
                    sig.level = 0.05,
                    power= 0.8)
# increase in power 
pwr::pwr.anova.test(k=3,
                    f= 0.5,
                    sig.level = 0.05,
                    power= 0.9)
# increase in power  blank
pwr::pwr.anova.test(k=3,
                    f= 0.5,
                    n=5,
                    sig.level = 0.05)
# try different level of k,n, f in power  blank
pwr::pwr.anova.test(k=3,
                    f= 0.5,
                    n=100,
                    sig.level = 0.05)
pwr::pwr.anova.test(k=3,
                    f= 0.25,
                    n=5,
                    sig.level = 0.05)

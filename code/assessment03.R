# Submitting this assessment by the due date is worth 50 points.
# Each question is worth 5 points.
# Call suitable packages as needed.

library(tidyverse)

# CO2 dataset -------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# The CO2 dataset in R records how grass plants from two origins, Quebec and
# Mississippi, respond to varying CO₂ concentrations and temperature treatments.

head(CO2)

# Each plant is identified by the `Plant` factor, with `Type` indicating its origin
# and `Treatment` showing whether it was chilled or nonchilled. `conc` gives the
# ambient CO₂ concentration (mL/L), and `uptake` measures the rate of CO₂
# assimilation (µmol/m²/sec). 

## DATA DESCRIPTION END ###################################################

# Q1 ----------------------------------------------------------------------

# CO2 dataframe is a base dataframe. Convert this to a class `tibble`
# then assign to `df_co2`

library(tibble)
df_co2 <- as_tibble(CO2)
class(df_co2)

# Q2 ----------------------------------------------------------------------

# Convert column names to lowercase and reassign to `df_co2`

df_co2 <- janitor::clean_names(df_co2)
names(df_co2)

# Q3 ----------------------------------------------------------------------

# Create scatter plots of CO₂ uptake versus ambient CO₂ concentration using `df_co2`.
# - The x-axis should represent ambient CO₂ concentration
# - The y-axis should represent CO₂ assimilation rate
# - Color the points by treatment.
# - Create separate panels for each plant type (Quebec vs Mississippi) and combine the plots.
# Call additional packages as needed

# Plot for Quebec
ptype1 <- df_co2 %>% 
  filter(type == "Quebec") %>% 
  ggplot(aes(x = conc,
             y = uptake,
             color = treatment)) +
  geom_point()
# Plot for Mississippi
ptype2 <- df_co2 %>% 
  filter(type == "Mississippi") %>% 
  ggplot(aes(x = conc,
             y = uptake,
             color = treatment)) +
  geom_point()

# Combined plot 
library(patchwork)

ptype1/ ptype2

# Q4 ----------------------------------------------------------------------

# The df_co2 dataset contains the following variables:
# - CO₂ assimilation rate
# - ambient CO₂ concentration
# - treatment: chilled vs nonchilled
# - type: plant origin (Quebec vs Mississippi)
# 
# Develop suitable statistical models to examine:
#   
# - The main effect of ambient CO₂ concentration (conc)
# - The main effect of treatment
# - The interaction between concentration and treatment
# 
# Fit these models separately for each plant origin (type).
# Fit these models separately for each plant origin.

mod_quebec <- df_co2 %>%
  filter(type == "Quebec") %>%
  lm(uptake ~ conc * treatment, data = .)

summary(mod_quebec)

mod_mississippi <- df_co2 %>%
  filter(type == "Mississippi") %>%
  lm(uptake ~ conc * treatment, data = .)

summary(mod_mississippi)

# Q5 -------------------------------------------------------------------


# Based on the models fitted in Q4 for Quebec and Mississippi plants,
# describe how CO2 assimilation rate responded to ambient CO2
# concentration under different treatments (chilled vs non-chilled) 
# for each plant origin. Highlight the differences between Quebec and 
# Mississippi plants, and use the model results to support your answers.

#ENTER YOUR ANSWER HERE as COMMENT: 
#(no coding required for this question)

#For both Quebec and Mississippi plants, CO2 assimilation rate (uptake) increased with rising ambient CO2 concentration. 
#This reflects a general positive physiological response to higher CO2 availability.
#However, the strength of this response and the influence of treatment (chilled vs non chilled) 
#differed between plant origins.
# In the Quebec model, both concentration (conc) and treatment had clear effects. 
#Non chilled Quebec plants showed a stronger increase in CO2 uptake with increasing CO2 concentration,
#indicated by a steeper slope.
#Chilled plants had lower overall uptake, and their response curve increased more slowly. 
#chilling reduced both the baseline assimilation rate and the sensitivity to increasing CO2.
# For Mississippi plants, the main effect of concentration was also positive, but the treatment effect and
#the conc:treatment interaction were weaker than those observed in Quebec plants. 
#Non-chilled Mississippi plants typically had 
# higher uptake overall, but chilled plants did not show as large a reduction 
# in either baseline uptake or slope as the Quebec chilled plants did. This 
# suggests that Mississippi plants were less sensitive to chilling, and their 
# CO2-response curves remained more similar across treatments.

#Differences Between Origins
#Quebec plants demonstrated a stronger treatment effect: 
#chilling had a more pronounced negative effect on both the level of CO2 assimilation and 
#the rate at which uptake increased with rising CO2 concentration. Mississippi 
# plants showed a more stable response, with less change in slope and baseline uptake across treatments. 


# BCI data ----------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# BCI dataset:
# run the following code to get data.
if(!require(vegan)) install.packages("vegan")
library(vegan)
data("BCI")
data("BCI.env")

# BCI dataset:
# The BCI dataset contains tree species abundance data from 50 1-hectare plots 
# on Barro Colorado Island (Panama). Each row represents a plot, and each 
# column represents a tree species. Entries are counts of individuals of each 
# species in that plot. This dataset is often used to study species richness, 
# community composition, and diversity patterns in tropical forests.

print(BCI)

# The following code transforms the BCI dataset from wide to long format. 
# Originally, each plot was a row and each species a column. 
# After the transformation, each row represents a single species in a 
# single plot, with columns indicating the plot (plot), species, and 
# the corresponding count.

cnm <- colnames(BCI)

df_bci <- BCI %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  pivot_longer(cols = cnm[1]:cnm[length(cnm)], 
               names_to = "species", 
               values_to = "count")
head(df_bci)

# BCI.env dataset:
# This dataset contains environmental variables for the 50 plots in the BCI dataset.
# Key columns include:
# - UTM.EW, UTM.NS: spatial coordinates of each plot
# - Precipitation: mean annual rainfall (mm)
# - Elevation: plot elevation (m)
# - Age.cat: categorical forest age class
# - Geology: underlying geological formation type
# - Habitat: dominant habitat type in the plot
# - Stream: indicates presence of streamside (riparian) habitat
# - EnvHet: environmental heterogeneity (Simpson diversity of habitat subcells)
# These variables help explain variation in species composition and abundance 
# across plots and allow exploration of species–environment relationships.

print(BCI.env)

# The following code adds a new "plot" column.

df_env <- BCI.env %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  relocate(plot)

head(df_env)


## DATA DESCRIPTION END ####################################################

# Q6 ----------------------------------------------------------------------


# Convert column names of `df_bci_env` to lowercase and reassign to `df_bci_env`

df_env <- janitor::clean_names(df_env)

print(df_env)

# Q7 ----------------------------------------------------------------------

# In `df_env`, some environmental variables have no variation between plots
# (i.e., the same value for all plots). Identify these columns and remove them
# from the dataframe. Assign the resulting dataframe to `df_env_sub`.

no_var_cols  <- sapply(df_env, n_distinct) # This shows which columns have zero variance
df_env_sub <- df_env %>% 
  select(all_of(names(no_var_cols )[which(no_var_cols > 1)]))

no_var_cols

# Q8 ----------------------------------------------------------------------

# Calculate summary statistics for each plot using `df_bci`.
# For each plot, compute:
# - n_sum: total count of all individuals across species
# - n1: count of the most dominant species (maximum count among species)
# - p: proportion of the most abundant species (n1 / n_sum)
# Assign the resulting dataframe to `df_n`.

df_n <- df_bci %>% 
  group_by(plot) %>% 
  summarize(n_sum = sum(count),
            n1 = max(count),
            p = n1 / n_sum)
df_n
# Q9
# Combine the summary data (`df_n`) with the environmental variables
# (`df_env_sub`) for each plot. Assign the resulting dataframe to `df_m`.

df_m <- df_n %>% 
  left_join(df_env_sub,
            by = "plot")
df_m

# Q10
# Develop a statistical model to explain variation in the proportion of the dominant
# species in each plot. Use `EnvHet`, `Stream`, and `Habitat` as predictors.
# Fit a suitable statistical model to the data.
# Use model selection based on predictability (i.e., out-of-sample prediction) 
# rather than the goodness of fit, and report which variables are included in 
# the best predictive model as a comment.
df_mod <- glm(
  cbind(n1, n_sum - n1) ~ env_het + stream + habitat,
  data = df_m,
  family = "binomial"
)
library(MuMIn)
options(na.action = "na.fail")
(ms <- dredge(df_mod, rank = "AIC"))

best_mod <- get.models(ms, 1)[[1]]
summary(best_mod)
## the best predictive model can be the one with the lowest AIC value, 517.3 and 
##the highest model weight (0.716, meaning 71.6% probability of being the best model in the candidate set) Stream and Habitat 
##habitat + env_het without stream effect. Habitat + stream is also on the second 
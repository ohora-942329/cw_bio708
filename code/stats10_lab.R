#ctrl (command) +shift + N is a hotkey for creating a new file
#ctrl + I is fix indent
# #Model comparison Laboratory
pacman::p_load(tidyverse, janitor, palmerpenguins,
               pactchwork,here)
#install.packages("janitor")
# Perform only once
# install.packages("palmerpenguins")

# 16.3.1 Format Penguin Data ----------------------------------------------

# Load required packages
library(palmerpenguins)
library(dplyr)
library(tidyr)
library(janitor)
library(MuMIn)

# Start with the raw dataset
data("penguins_raw")

penguins_clean <- penguins_raw %>%
  # 1. Clean column names:
  #    - lowercase
  #    - replace spaces with underscores
  #    - remove parentheses
  clean_names() %>%
  
  # 2. Convert clutch_completion Yes/No to 1/0
  mutate(clutch_completion = ifelse(clutch_completion == "Yes", 1, 0)) %>%
  
  # 3. Standardize species names: adelie, chinstrap, gentoo
  mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo",
    TRUE ~ species  # fallback in case other values appear
  )) %>%
  
  # 4. Remove rows with missing values in key measurement columns
  drop_na(
    culmen_length_mm,
    culmen_depth_mm,
    flipper_length_mm,
    body_mass_g,
    sex
  )

# Preview cleaned data
head(penguins_clean)



# 16.3.2 Analyze Penguin Data ---------------------------------------------

# required for dredge
options(na.action = "na.fail")

# logistic regression with all predictors
m <- glm(
  clutch_completion ~ species + culmen_length_mm + culmen_depth_mm +
    flipper_length_mm + body_mass_g + sex,
  data = penguins_clean,
  family = "binomial"
)
m_set <- dredge(m, rank = "AIC")
subset(m_set, delta < 2)



  
  
  

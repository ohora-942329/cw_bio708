# Ctrl (Command) + Shift + N is a hotkey for creating a new script file
# Ctrl + I is to fix indent
# data cleaning tips in R

pacman::p_load(tidyverse,
               patchwork,
               janitor,
               stringdist,
               here)

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_messy.csv"
df_messy <- read_csv(url)


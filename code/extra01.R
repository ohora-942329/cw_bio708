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

# check data entries before you analyze data ------------------------------

## check class of each column
## - sapply & class

## check unique elements in each column
## - sapply & unique

## check possible type
## - stringdistmatrix()

# text cleaning in R ------------------------------------------------------

## remove white space
## str_squish()

## align text case
## str_to_lower() & str_to_upper()

## replace text
## str_replace() & str_replace_all()

## remove text
## str_remove() & str_remove_all()

## detect text
## str_detect()

# column manipulation based on column type --------------------------------
## mutate(), across(), where()

# example code ------------------------------------------------------------

# chr_clean <- function(x) {
#   x %>%
#     str_squish() %>% 
#     str_to_lower() %>% 
#     str_replace_all("\\.|\\s", "_") %>% 
#     str_remove_all("^_|_$")
# }
# 
# df_messy %>% 
#   mutate(collector = chr_clean(collector),
#          species = chr_clean(species),
#          length_mm = str_squish(length_mm) %>% 
#            str_replace(",", "\\.") %>% 
#            str_extract("\\d{1,}") %>% 
#            as.numeric(),
#          sample_date = parse_date_time(sample_date,
#                                        tz = "EST",
#                                        order = c("Y/m/d",
#                                                  "B d Y",
#                                                  "d B Y")),
#          recaptured = str_to_lower(recaptured) %>% 
#            str_sub(start = 1L,
#                    end = 1L)
#   )

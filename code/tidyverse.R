#install.packages("tidyverse")
library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)
#Subset rows; The filter() function (dplyr package) is used for subsetting rows of a data frame (or tibble) based on conditions.

#Single match ==

filter(iris_sub, Species == "virginica")
#Multiple match %in%
  
filter(iris_sub, Species %in% c("virginica", "versicolor"))

arrange(iris_sub, Sepal.Length)
#Except !=
filter(iris_sub, Species != "virginica")
#Except multiple !=
  
  filter(iris_sub, !(Species %in% c("virginica", "versicolor")))
 
# Greater than >
    
  filter(iris_sub, Sepal.Length > 5)  
#Greater than and equal to >=
    
    filter(iris_sub, Sepal.Length >= 5)  
#Less than <
      
filter(iris_sub, Sepal.Length < 5)    

#Less than and equal to <=
  
  filter(iris_sub, Sepal.Length <= 5)
 
## Multiple conditions (AND) & (or ,)
  
# Sepal.Length is less than 5 AND Species equals "setosa"
  filter(iris_sub,
         Sepal.Length < 5 & Species == "setosa")  
# same; "," works like "&"
  filter(iris_sub,
         Sepal.Length < 5, Species == "setosa")  
# Either Sepal.Length is less than 5 OR Species equals "setosa"
  filter(iris_sub,
         Sepal.Length < 5 | Species == "setosa")  
 
##Arrange: Increasing/ascending order
  
  arrange(iris_sub, Sepal.Length)

##Decreasing/descending order
  
  arrange(iris_sub, desc(Sepal.Length)) 
  
  #Select one column
  
  select(iris_sub, Sepal.Length)
#Select multiple columns
  
  select(iris_sub, c(Sepal.Length, Sepal.Width))  

  #Remove one column
  
  select(iris_sub, -Sepal.Length)  

  #Remove multiple columns
  
  select(iris_sub, -c(Sepal.Length, Sepal.Width))  
 
  # Select/Remove with starts_with()
  
  # select columns starting with "Sepal"
  select(iris_sub, starts_with("Sepal"))  
  
  # remove columns starting with "Sepal"
  select(iris_sub, -starts_with("Sepal"))  
  
  # select columns ending with "Sepal"
  select(iris_sub, ends_with("Width"))  
  
  # remove columns ending with "Sepal"
  select(iris_sub, -ends_with("Width"))  
  
 ## Add a new column
  
  # nrow() returns the number of rows of the dataframe
  (x_max <- nrow(iris_sub))
  
  # create a vector from 1 to x_max
  x <- 1:x_max
  
  # add as a new column
  # named `x` as `row_id` when added
  mutate(iris_sub, row_id = x)  
  
 ## Modify an existing column
  
  # twice `Sepal.Length` and add as a new column
  
  mutate(iris_sub, sl_two_times = 2 * Sepal.Length)
  
  #Excersis 
  
  library(dplyr)
  
  iris_pw <- iris_sub %>%
    select(Petal.Width, Species)
  
 #Pipeing:  Pipe %>%
  ## 
  df_vir <- filter(iris_sub, Species == "virginica")
  df_vir_sl <- select(df_vir, Sepal.Length)
  
  print(df_vir_sl)
  
df_tw<-iris_sub %>% 
  select(Sepal.Length) %>% 
mutate(twice = 2* Sepal.Length)

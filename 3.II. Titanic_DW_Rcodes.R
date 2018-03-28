# 0: Load the data in RStudio

library(readr)
titanic_original <- read_delim("~/R projects/Titanic data/3.II.Titanic_DW_titanic_original.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)
View(titanic_original)

library(tidyr)
library(dplyr)

# 1: Port of embarkation -> Fix the missing values

# Using Base R method 

titanic_original$embarked[is.na(titanic_original$embarked)] <- "S"            # <- USING INDEXING

# titanic_original[is.na(titanic_original$embarked), "embarked"] <- "S"             alternative ways to write the same code
# titanic_original[["embarked"]][is.na(titanic_original[["embarked"]])] <- "S"      alternative ways to write the same code


titanic_original <- within(titanic_original, embarked <- ifelse(is.na(embarked), "S", embarked))   #  <- USING IFELSE


# Using Dplyr

titanic_original <- titanic_original %>% 
  mutate(embarked = replace(embarked, is.na(embarked), "S"))


# 2: Age ( While there are many ways to fill these missing values, using the mean or median of the rest of the values is quite common in such cases.)

titanic_original <- titanic_original %>% 
  mutate(age  = replace(age, is.na(age), mean (as.numeric(age), na.rm = TRUE))) 


# Is there something better to use instead of the Mean ? If so, which value ?


# 3: Lifeboat -> Create dummy variable for missing values

titanic_original <- titanic_original %>% 
  mutate( boat = replace(boat,is.na(boat), "None"))

# 4: Cabin

titanic_original <- titanic_original %>% 
  mutate( has_cabin_number = ifelse(!is.na(cabin), 1, 0))

# 5: Import clean dataset on Github

write.csv(titanic_original, file = "3.II.Titanic_DW_titanic_clean")






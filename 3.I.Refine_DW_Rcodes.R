library(dplyr)
library(tidyr)

# 0: Load the data in RStudio

library(readr)
refine_original <- read_delim("~/R projects/Ex.1 dataset/3.I.Refine_DW_refine_original.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)
View(refine_original)


# 1: Clean up brand names


# a.Solving the problem with base R version


refine_original$company <- tolower(refine_original$company)
refine_original$company <- sub(pattern = ".*ps", replacement = "philips",refine_original$company)
refine_original$company <- sub(pattern = "ak.*", replacement = "akzo",refine_original$company)
refine_original$company <- gsub(pattern = "van.*", replacement = "van houten",refine_original$company)
refine_original$company <- gsub(pattern = "un.*", replacement = "unilever",refine_original$company)

# b.Using dplyr 

refine_original <- refine_original %>% 
  mutate(company = tolower(company)) %>%
  mutate(company = sub(pattern = ".*ps","philips",company), 
         company = sub(pattern = "ak.*","akzo",company), 
         company = sub(pattern = "van.*","van houten",company), 
         company = sub(pattern = "un.*","unilever",company))


# 2:Separate product code and product number into separate columns i.e. add two new columns called product_code and product_number, containing the product code and number respectively

refine_original <- refine_original %>% separate(`Product code / number`, c("product_code","product_number"), sep = "-")

# 3: Add product categories column, that helps the reader to associate the product_code column to its meaning.

refine_original<- refine_original %>%
  mutate(product_category = case_when(
    product_code == "p" ~ "Smartphone",
    product_code == "v" ~ "Tv",
    product_code == "x" ~ "Laptop",
    product_code == "q" ~ "Tablet"))

# 4: Add full address for geocoding 

refine_original <- refine_original %>% 
  unite("full_address", address, city,country, sep = ",", remove = FALSE) 


# 5: Create dummy variables for company and product category 

refine_original <- refine_original %>% mutate(company_philips = ifelse(company %in% "philips", yes = 1, no = 0),
                                              company_akzo = ifelse(company %in% "akzo", yes = 1, no = 0),
                                              company_van_houten = ifelse(company %in% "van houten", yes = 1, no = 0),
                                              company_unilever = ifelse(company %in% "unilever", yes = 1, no = 0),
                                              
                                              product_smartphone = ifelse(product_category %in% "Smartphone", yes = 1, no = 0),
                                              product_tv = ifelse(product_category %in% "Tv", yes = 1, no = 0),
                                              product_laptop = ifelse(product_category %in% "Laptop", yes = 1, no = 0),
                                              product_tablet = ifelse(product_category %in% "Tablet", yes = 1, no = 0)) 

# 6: Submit the project on Github

write.csv(refine_original, file = "3.I.Refine_DW_refine_clean.csv")






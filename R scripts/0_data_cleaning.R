# Unsupervised Machine Learning Tutorial ----
# Cleaning & Loading Data ----

# Load necessary packages ----
# Before running this code, ensure you have these packages installed:
# 'tidyverse' for data manipulation, 'here' for managing file paths,
# 'haven' for reading SPSS files, 'readxl' for reading Excel files,
# and 'naniar' for analyzing missing data.
library(tidyverse)
library(here) 
library(haven)
library(readxl)
library(naniar)

# Load data ----
# Using the 'here' package to set the file path from the project root directory.
# If you are not using 'here', specify your own file path directly.
iat_codebook <- readxl::read_xlsx(here("Data/Race_IAT_public_2023_codebook.xlsx"))
iat <- haven::read_sav(here("Data/RaceIAT.public.2023.sav"))

# Downsample the dataset ----
# Set a seed for reproducibility to ensure consistent results
set.seed(1234)

# Calculate missingness for each variable ----
# 'naniar' package helps create a summary of missing data in each variable
missingness_summary <- iat %>% 
  miss_var_summary() # Summarizes the percentage of missing data for each variable

# Filter variables with missingness above 40%, except MCPR variables ----
# Identify variables where more than 40% of the data is missing, excluding those starting with 'mcpr'
vars_to_remove <- missingness_summary %>% 
  filter(pct_miss > 40 & !str_starts(variable, "mcpr")) %>% # Exclude MCPR variables from removal
  pull(variable) # Extract the names of these variables for removal

# Remove variables with more than 40% missingness from the dataset ----
# Create a new dataset excluding the identified variables with excessive missingness
iat_clean <- iat %>% 
  select(-one_of(vars_to_remove)) # Remove the identified variables from the dataset

# Omit participants who do not have responses on the MCPR variables ----
# Filter out participants with missing responses in any MCPR variable
iat_clean <- iat_clean %>% 
  filter(if_all(starts_with("mcpr"), ~ !is.na(.))) # Keep only participants with complete MCPR responses

# Specify columns to exclude from the missingness check ----
# These columns are allowed to have missing values without excluding participants
exclude_columns <- c("raceombmulti", 
                     "raceomb_003sub_asian", 
                     "raceomb_003sub_black", 
                     "raceomb_003sub_hispanic", 
                     "raceomb_003sub_pacific", 
                     "raceomb_003sub_middleeast", 
                     "raceomb_003sub_white")

# Omit participants with any missing values, except in the specified columns ----
# Use 'filter' to keep only rows (participants) where all specified conditions are met
iat_clean <- iat_clean %>%
  filter(if_all(-all_of(exclude_columns), ~ !is.na(.)))
# 'if_all(-all_of(exclude_columns), ...)' applies the condition to all columns except those specified
# '!is.na(.)': checks if the value is not missing (NA)
# The row is retained if all non-excluded columns have no missing values
save(iat_clean, file = here("data/iat.rda")) # Saving the cleaned IAT dataset to an RDA
# Final Message ----
# At this point, the dataset 'iat_clean' has been prepared for analysis in Unsupervised Machine Learning (UML).
# We have removed variables with excessive missing data, kept essential variables (MCPR), 
# and excluded participants with incomplete responses where necessary.



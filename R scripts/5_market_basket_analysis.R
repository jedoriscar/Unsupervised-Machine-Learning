# Unsupervised Machine Learning Tutorial ----
# Market Basket Analysis / Association Rules ----
# Load necessary packages ----
# These packages are required for various tasks:
library(tidyverse) # For data manipulation
library(here) # For managing file paths
library(arules) # For performing association rule mining
library(arulesViz) # For visualizing association rules
library(dplyr) # For data manipulation
library(haven) # Load the haven package

options(scipen = 99999) # Prevent scientific notation in plots for clarity

# Load data ----
# Using the 'here' package to load the datasets. This helps in managing file paths from the project root directory.
# If you are not using 'here', specify your own file paths directly.
iat_codebook <- readxl::read_xlsx(here("Data/Race_IAT_public_2023_codebook.xlsx"))
load(here("Data/iat.rda")) # Load the cleaned dataset

# Select specified variables for Market Basket Analysis ----
# We create a subset of the dataset containing only the specified variables.
variables_to_keep <- c("D_biep.White_Good_all", "att7", "Tblack_0to10", 
                       "Twhite_0to10", "D_biep.White_Good_36", 
                       "D_biep.White_Good_47", "edu",
                       "occuSelf",
                       "politicalid7", 
                       "CountyNo",
                       "STATE", 
                       "religionid", "broughtwebsite", "mcpr1", 
                       "mcpr2", "mcpr3", "mcpr4", 
                       "mcpr5", "mcpr6", "mcpr7", 
                       "mcpr8", "mcpr9", "mcpr10", 
                       "mcpr11", "mcpr12", "mcpr13", 
                       "mcpr14", "mcpr15", "mcpr16", "mcpr17")

iat_market_basket_data <- iat_clean %>%
  select(all_of(variables_to_keep)) # Select only specified variables

# Convert data to transactions ----
# Market Basket Analysis (MBA) requires the dataset to be formatted as "transactions."
# Transactions are sets of items that co-occur, analogous to a shopping basket where each "basket" contains items purchased together.
# Each row in the dataset represents a transaction, and each column represents an item or variable.

# Step 1: Ensure all variables are in factor format ----
# MBA operates on categorical data. If variables are not already factors, we need to convert them.
# The first step converts haven_labelled columns (e.g., SPSS-style labels) to character.
# The second step converts all columns to factors for compatibility with MBA.
iat_market_basket_data <- iat_market_basket_data %>%
  mutate(across(where(haven::is.labelled), as.character)) %>% # Convert haven_labelled to character
  mutate(across(everything(), as.factor)) # Convert all variables to factors

# Step 2: Convert the dataset to transactions ----
# The 'transactions' format is required by MBA functions. Each row becomes a transaction (e.g., a participant),
# and each column represents an item or variable. 
transactions <- as(iat_market_basket_data, "transactions")

# Step 3: Summarize the transactions ----
# Provides a summary of the dataset in "transactions" format.
# The summary includes:
#   - The number of transactions (rows in the dataset).
#   - The number of items (columns in the dataset).
#   - Information on sparsity (percentage of empty entries).
#   - Frequency of the most common items or combinations.
summary(transactions)

# Key Outputs Explained:
# 1. **`transactions` Object**:
#    - Encodes the dataset for MBA, where rows are individual transactions, and columns are items or variables.
#    - This format allows algorithms to identify patterns of co-occurrence.
# 2. **Summary of Transactions**:
#    - Highlights the size of the dataset and the sparsity (useful for determining dataset suitability for MBA).
#    - Lists the most frequent items, providing initial insights into the data's structure.
# 3. **Why Factor Conversion is Necessary**:
#    - MBA algorithms require categorical data.
#    - Converting numeric or labeled data to factors ensures the correct input format.

# Notes for Practitioners:
# - Ensure meaningful binning or categorization of numeric variables before converting to factors.
# - Transaction format is particularly useful for identifying co-occurrence patterns (e.g., "rules") in the data.
# - For more details on the variables and their levels, inspect `iat_market_basket_data` before conversion.

# Perform Market Basket Analysis ----
# Use the Apriori algorithm to identify association rules based on thresholds for support and confidence.
rules <- apriori(transactions, parameter = list(supp = 0.10, conf = 0.8))
# 'support' (supp): Minimum fraction of transactions containing the rule (e.g., 10% of transactions).
# 'confidence' (conf): Minimum likelihood that the consequent (RHS) occurs given the antecedent (LHS), here 80%.

# Summary of rules ----
# Provides an overview of the rules, including their count and statistical properties like support and confidence.
summary(rules)

# Inspect the top 20 rules ----
# View the 20 rules with the highest confidence.
inspect(sort(rules, by = "confidence")[1:20])
# Sorting by confidence ranks rules by the strength of their predictiveness.

# Visualize the association rules with parallel coordinates plot ----
# Parallel coordinates plot shows relationships between variables in the rules.
plot(rules, method = "paracoord", control = list(reorder = TRUE, alpha = 0.8))
# Variables (LHS and RHS) appear as vertical axes, and connecting lines represent rules.
# Transparency (alpha = 0.8) reduces clutter for better visualization.

# Using arulesViz to create visualizations of the association rules.
plot(rules, method = "graph", control = list(type = "items"))
plot(rules, method = "scatterplot", measure = c("support", "confidence"), shading = "lift")

# Scatter Plot (Matrix Plot)
plot(rules, measure = c("support", "confidence"), shading = "lift")

# Grouped Matrix Plot
plot(rules, method = "grouped")
# Save the association rules ----
# Save the rules for future use.
save(rules, file = here("data/association_rules.rda"))

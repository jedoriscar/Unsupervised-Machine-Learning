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
# Market Basket Analysis requires data to be in transaction format.
# Convert the selected variables to factors if they are not already.
# Convert haven_labelled columns to character first, then convert all columns to factor
iat_market_basket_data <- iat_market_basket_data %>%
  mutate(across(where(haven::is.labelled), as.character)) %>%
  mutate(across(everything(), as.factor))


# Convert the dataset to transactions
transactions <- as(iat_market_basket_data, "transactions")

# Summary of transactions ----
# This provides a summary of the transactions.Transactions are levels that co-occur
summary(transactions)

# Perform Market Basket Analysis ----
# Using the apriori algorithm to find association rules.
rules <- apriori(transactions, parameter = list(supp = 0.10, conf = 0.8))

# Summary of rules ----
# This provides a summary of the association rules found.
summary(rules)

# Inspect the top 10 rules ----
# Display the top 10 rules sorted by confidence.
inspect(sort(rules, by = "confidence")[1:20])

# Visualize the association rules ----
# Using arulesViz to create visualizations of the association rules.
plot(rules, method = "graph", control = list(type = "items"))
plot(rules, method = "scatterplot", measure = c("support", "confidence"), shading = "lift")

# Scatter Plot (Matrix Plot)
plot(rules, measure = c("support", "confidence"), shading = "lift")

# Grouped Matrix Plot
plot(rules, method = "grouped")

# Parallel Coordinates Plot
plot(rules, method = "paracoord", control = list(reorder = TRUE))

plot(rules, method = "paracoord", control = list(reorder = TRUE, alpha = 0.8))
# Save the association rules ----
# Save the rules for future use.
save(rules, file = here("data/association_rules.rda"))

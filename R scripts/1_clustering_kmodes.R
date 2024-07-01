# Unsupervised Machine Learning Tutorial ----
# K-Modes Clustering ----

# Load necessary packages ----
# Ensure these packages are installed:
# 'tidyverse' for data manipulation, 'here' for file paths,
# 'klaR' for K-modes clustering, and 'vcd' for visualizing categorical data.
library(tidyverse)
library(here)
library(klaR)
library(vcd)
options(scipen = 99999) # Prevent scientific notation in plots for clarity

# Load data ----
# Load datasets using 'here' to set file paths from the project root directory.
# If not using 'here', specify the file paths directly.
iat_codebook <- readxl::read_xlsx(here("Data/Race_IAT_public_2023_codebook.xlsx"))
load(here("Data/iat.rda")) # Load the cleaned dataset

# Filter only character variables from the dataset ----
# K-Modes clustering is suitable for categorical (character) data.
variables_to_exclude <- c("session_status", "study_name", "occuSelf", "previous_session_schema")

iat_categorical <- iat_clean %>% 
  select(where(is.character)) %>% # Select only character columns
  select(-all_of(variables_to_exclude)) # Exclude specified variables

# K-Modes Clustering ----
# Compute K-modes clustering with a chosen number of clusters (e.g., K = 3)
# Suitable for categorical data.
set.seed(1234) # Set seed for reproducibility in cluster assignment
km.res <- kmodes(iat_categorical, 3) # Perform K-modes clustering with 3 clusters

# Print the clustering results
print(km.res)

# Define a function to calculate the mode
get_mode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}

# Calculate the mode for each cluster in the categorical dataset
cluster_mode <- aggregate(iat_categorical, by = list(cluster = km.res$cluster), get_mode)

# Visualize the K-modes clustering results ----
# Use a mosaic plot to visualize the relationship between clusters and a categorical variable
# 'broughtwebsite' should be replaced with a relevant variable from the dataset
iat_categorical$cluster <- as.factor(km.res$cluster)

# Create a mosaic plot for one of the categorical variables by cluster
mosaic(~ cluster + broughtwebsite, data = iat_categorical,
       main = "Mosaic Plot of Clusters by Categorical Variable",
       shade = TRUE, legend = TRUE)

# Note:
# This tutorial demonstrates K-modes clustering, suitable for categorical data.
# We filtered character variables, performed clustering, calculated the mode for each cluster,
# and visualized the results using a mosaic plot to show the relationship between clusters and a categorical variable.

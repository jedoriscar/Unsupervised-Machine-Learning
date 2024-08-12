# Unsupervised Machine Learning Tutorial ----
# Principal Component Analysis (PCA) ----

# Load necessary packages ----
# These packages are required for various tasks:
library(tidyverse) # For data manipulation
library(here) # For managing file paths
library(stats) # For performing statistical operations, including PCA
library(ggplot2) # For creating plots
library(factoextra) # For visualizing PCA results
library(dplyr) # For data manipulation
options(scipen = 99999) # Prevent scientific notation in plots for clarity

# Load data ----
# Using the 'here' package to load the datasets. This helps in managing file paths from the project root directory.
# If you are not using 'here', specify your own file paths directly.
iat_codebook <- readxl::read_xlsx(here("Data/Race_IAT_public_2023_codebook.xlsx"))
load(here("Data/iat.rda")) # Load the cleaned dataset

# Select specified variables for PCA ----
# We create a subset of the dataset containing only the specified variables.
variables_to_keep <- c("D_biep.White_Good_all", "att7", "Tblack_0to10", 
                       "Twhite_0to10", "D_biep.White_Good_36", 
                       "D_biep.White_Good_47", "edu", "edu_14", 
                       "occuSelf", "occupation_self_002", 
                       "occuSelfDetail", "politicalid_7", 
                       "politicalid7", "MSANo", 
                       "CountyNo", "MSAName", 
                       "STATE", "religion2014", 
                       "religionid", "broughtwebsite", "mcpr1", 
                       "mcpr2", "mcpr3", "mcpr4", 
                       "mcpr5", "mcpr6", "mcpr7", 
                       "mcpr8", "mcpr9", "mcpr10", 
                       "mcpr11", "mcpr12", "mcpr13", 
                       "mcpr14", "mcpr15", "mcpr16", "mcpr17")

iat_pca_data <- iat_clean %>%
  select(all_of(variables_to_keep)) # Select only specified variables

# Keep only numeric variables for PCA ----
# PCA requires numeric variables, so we filter the dataset to include only numeric columns.
iat_pca_data_numeric <- iat_pca_data %>%
  select(where(is.numeric))

# Handle missing data ----
# PCA requires complete data, so we will remove rows with any missing values.
iat_pca_data_numeric <- na.omit(iat_pca_data_numeric)

# Standardize the data ----
# Scaling the data to have mean 0 and standard deviation 1, which is important for PCA.
iat_pca_data_scaled <- scale(iat_pca_data_numeric)

# Perform Principal Component Analysis (PCA) ----
# Using the prcomp function from the stats package to perform PCA on the scaled data.
pca_result <- prcomp(iat_pca_data_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA results ----
# This provides the standard deviations of the principal components, and their rotation (or loadings).
summary(pca_result)

# Print the loadings (principal component vectors) ----
# Loadings represent the correlation between the original variables and the principal components.
print(pca_result$rotation)

# Visualize the variance explained by each principal component ----
# Create a scree plot to visualize the proportion of variance explained by each component.
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50)) +
  labs(title = "Scree Plot",
       x = "Principal Component",
       y = "Percentage of Variance Explained")

# Visualize the PCA results ----
# Create a biplot to visualize the PCA results.
fviz_pca_biplot(pca_result, repel = TRUE, 
                col.var = "contrib", # Color variables by contributions to the PC
                col.ind = "cos2", # Color individuals by the quality of representation
                palette = "jco", 
                ggtheme = theme_minimal())

# Create a dataframe that includes the principal component scores ----
iat_pca_scores <- as.data.frame(pca_result$x)

# Append the principal component scores to the original dataset ----
iat_pca_data_with_scores <- cbind(iat_clean, iat_pca_scores)

# Save the dataset with PCA scores ----
save(iat_pca_data_with_scores, file = here("data/iat_pca_data_with_scores.rda"))

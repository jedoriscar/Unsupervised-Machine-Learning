# Unsupervised Machine Learning Tutorial ----
# K-Modes Clustering ----

# Load necessary packages ----
# These packages are required for various tasks:
library(tidyverse) # For data manipulation
library(here) # For managing file paths
library(klaR) # For performing K-modes clustering
library(vcd) # For visualizing categorical data
library(dplyr) # For data manipulation
library(dbscan) # For density-based clustering
library(ggplot2) # For creating plots
options(scipen = 99999) # Prevent scientific notation in plots for clarity

# Load data ----
# Using the 'here' package to load the datasets. This helps in managing file paths from the project root directory.
# If you are not using 'here', specify your own file paths directly.
iat_codebook <- readxl::read_xlsx(here("Data/Race_IAT_public_2023_codebook.xlsx"))
load(here("Data/iat.rda")) # Load the cleaned dataset

# Filter only numeric variables from the dataset ----
# K-means clustering requires numeric variables. We filter the dataset to include only the specified numeric variables.
variables_to_keep <- c("politicalid_7",
                       "D_biep.White_Good_all", "Tblack_0to10")

iat_numeric <- iat_clean %>%
  select(where(is.numeric)) %>% # Select only numeric columns
  select(where(~ var(.) > 1e-10)) %>% # Exclude columns with near-zero variance
  select(all_of(variables_to_keep)) # Include only specified variables

# Standardize the numeric variables ----
# Scaling the data to have mean 0 and standard deviation 1, which is important for clustering.
iat_numeric_scaled <- scale(iat_numeric)

# Verify the standardization ----
# Check the summary of the scaled data to ensure it has been standardized correctly.
summary(iat_numeric_scaled)

# Visualizing k-Nearest Neighbors (kNN) to see how close data points are to one another ----
# kNN distance plot helps in choosing the appropriate eps value for DBSCAN.
kNNdistplot(iat_numeric_scaled, k = 5) # Adjust k based on MinPts
abline(h = .55, col = "red", lty = 2) # Add a horizontal line at height 3, adjust based on the plot

# Density Based Clustering ----
# Compute Density-Based clustering with chosen epsilon (eps) and minimum points (MinPts)
set.seed(1234) # Set seed for reproducibility in cluster assignment
db.res <- dbscan(iat_numeric_scaled, eps = 0.61, minPts = 5) 

# Print the clustering results ----
# Display the results of the DBSCAN clustering
print(db.res)

# Display a table of the clustering results against one of the variables ----
# Calculate the mean of each variable by cluster ----
# This helps in understanding the characteristics of each cluster
cluster_mean <- aggregate(iat_numeric_scaled, by = list(cluster = db.res$cluster), mean)

# Append the clusters to the original dataset ----
# This adds a new column to the dataset indicating the cluster assignment for each participant
iat_clusters <- cbind(iat_clean, cluster = db.res$cluster)

# Explanation of DBSCAN parameters ----
# In DBSCAN, clusters are determined based on the parameters eps and MinPts:
# eps (Epsilon): Defines the radius of the neighborhood around a point. Points within this distance are considered neighbors.
# MinPts: The minimum number of points required to form a dense region. If a point has at least MinPts within its eps neighborhood, it becomes a core point, leading to cluster formation.
# Clusters form around core points, and noise points are labeled as outliers. The number of clusters isn't predetermined; it's based on data density and the chosen parameters.

# Create a dataframe that includes the data and the cluster labels ----
iat_numeric_scaled_df <- as.data.frame(iat_numeric_scaled)
iat_numeric_scaled_df$cluster <- factor(db.res$cluster)

# Filter out noise points (cluster 0) ----
# Removing noise points to focus on the actual clusters
iat_numeric_scaled_df_filtered <- iat_numeric_scaled_df %>%
  filter(cluster != 0)

# Plot the clustering results without noise points ----
# Use 'factoextra' to create a cluster plot
fviz_cluster(list(data = iat_numeric_scaled_df_filtered[, -ncol(iat_numeric_scaled_df_filtered)], 
                  cluster = iat_numeric_scaled_df_filtered$cluster),
             geom = "point", # Points represent data observations
             ellipse.type = "convex", # Convex hulls around clusters
             ggtheme = theme_minimal(), # Minimal theme for clarity
             palette = "jco") # Color palette for clusters

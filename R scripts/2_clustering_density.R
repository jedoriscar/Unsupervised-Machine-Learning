# Unsupervised Machine Learning Tutorial ----
# Density Based Spatial Noise Clustering (DBSCAN) ----

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

# Visualizing k-Nearest Neighbors (kNN) Distances ----
# The kNN distance plot is a diagnostic tool used in DBSCAN to determine the optimal value for 'eps' (the neighborhood radius).
# The 'k' parameter corresponds to the number of neighbors (MinPts), which influences how clusters are defined.

# Generate the kNN distance plot for the scaled data
kNNdistplot(iat_numeric_scaled, k = 5) # Set k to 5, aligning with the MinPts parameter in DBSCAN

# Add a horizontal line to highlight the 'elbow point' in the kNN plot
abline(h = 0.55, col = "red", lty = 2) # Set at the chosen 'eps' value (based on the plot inspection)
# The horizontal line indicates the threshold distance for clustering (eps). 
# Points below this line are considered part of a dense cluster, while points above may be treated as noise.

# Density-Based Clustering with DBSCAN ----
# Perform DBSCAN clustering using the scaled data
# Parameters:
# 'eps' specifies the neighborhood radius for clustering.
# 'minPts' is the minimum number of points required to form a dense region (cluster).
set.seed(1234) # Set seed for reproducibility in cluster assignment
db.res <- dbscan(iat_numeric_scaled, eps = 0.61, minPts = 5) # Perform DBSCAN with specified parameters

# Print the DBSCAN clustering results ----
# The DBSCAN output includes the following components:
# 'cluster': Cluster assignments for each data point. Points assigned to 0 are considered noise.
# 'eps': The neighborhood radius used for clustering.
# 'minPts': The minimum number of points required to form a cluster.
# 'isseed': A logical vector indicating whether each point is a core point (TRUE) or not (FALSE).
print(db.res) # Display the clustering results

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
# Converting the scaled dataset into a dataframe for easy manipulation
iat_numeric_scaled_df <- as.data.frame(iat_numeric_scaled) # Convert to dataframe
iat_numeric_scaled_df$cluster <- factor(db.res$cluster) # Add cluster labels from DBSCAN

# Filter out noise points (cluster 0) ----
# DBSCAN assigns points not belonging to any cluster as "noise" (cluster 0). 
# Here, we remove noise points to focus on the main clusters.
iat_numeric_scaled_df_filtered <- iat_numeric_scaled_df %>%
  filter(cluster != 0) # Exclude noise points (cluster 0)

# Plot the clustering results without noise points ----
# Use 'factoextra' to visualize the clustering results, highlighting actual clusters without noise.
fviz_cluster(list(
  data = iat_numeric_scaled_df_filtered[, -ncol(iat_numeric_scaled_df_filtered)], # Exclude the cluster column for plotting
  cluster = iat_numeric_scaled_df_filtered$cluster), # Use the cluster assignments
  geom = "point", # Each data observation is shown as a point
  ellipse.type = "convex", # Draw convex hulls around each cluster
  ggtheme = theme_minimal(), # Apply a clean, minimal theme
  palette = "jco" # Use the 'jco' color palette for clusters
)

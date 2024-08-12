# Unsupervised Machine Learning Tutorial ----
# K-Means Clustering ----

# Load necessary packages ----
# These packages are required for various tasks:
library(tidyverse) # For data manipulation
library(here) # For managing file paths
library(stats) # For performing statistical operations
library(readr) # For reading CSV files
library(factoextra) # For visualizing clustering results
library(dplyr) # For data manipulation
library(reshape2) # For reshaping data
library(corrplot) # For visualizing correlation matrices
library(Hmisc) # For creating correlation matrices with p-values
library(haven) # For handling labeled data from SPSS, SAS, and Stata
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

# Determining Optimal Clusters ----
# Elbow Method: Determines the optimal number of clusters by plotting WSS vs. number of clusters (K)
# The "elbow" point where WSS starts to decrease slowly indicates the optimal K.

# Create a function to calculate total within-cluster sum of squares (WSS) for different K values
wss <- function(data, max_k) {
  sapply(1:max_k, function(k) {
    kmeans(data, centers = k, nstart = 25)$tot.withinss # Calculate WSS for each K
  })
}

# Calculate WSS for K values from 1 to 10
wss_values <- wss(iat_numeric_scaled, max_k = 10)

# Plot the Elbow Method
# This plot helps us determine the optimal number of clusters by visualizing WSS for each K
fviz_nbclust(iat_numeric_scaled, kmeans, method = "wss") + 
  geom_vline(xintercept = which.min(diff(wss_values)), linetype = 2) + # Add a line at the elbow point
  labs(title = "Elbow Method for Optimal K") # Title the plot

# K-Means Clustering ----
# Perform k-means clustering with a chosen number of clusters (e.g., K = 5)
# K-means uses Euclidean distance to calculate the distance between points and cluster centers.
set.seed(1234) # Set seed for reproducibility in cluster assignment
km.res <- kmeans(iat_numeric_scaled, 5, nstart = 25) # Perform k-means with 5 clusters, 25 random starts

# This returns a list of components from the k-means algorithm:
# 'cluster': Cluster assignments for each point
# 'centers': Cluster centroids (means)
# 'totss': Total sum of squares (TSS) in the data
# 'withinss': Within-cluster sum of squares for each cluster
# 'tot.withinss': Total within-cluster sum of squares
# 'betweenss': Between-cluster sum of squares
# 'size': Number of observations in each cluster

# Print the results to see the summary of the clustering
print(km.res)

# Calculate the mean of each variable by cluster ----
# This helps in understanding the characteristics of each cluster
cluster_mean <- aggregate(iat_numeric_scaled, by = list(cluster = km.res$cluster), mean)

# Append the clusters to the original dataset ----
# This adds a new column to the dataset indicating the cluster assignment for each participant
iat_k_clusters <- cbind(iat_clean, cluster = km.res$cluster)

# Save the clustered k-means dataset ----
save(iat_k_clusters, file = here("data/iat_k_clusters.rda")) # Save the dataset with cluster assignments

# Visualize the K-means clustering results ----
# Use 'factoextra' to create a cluster plot
fviz_cluster(km.res, data = iat_numeric_scaled, 
             geom = "point", # Points represent data observations
             ellipse.type = "convex", # Convex hulls around clusters
             ggtheme = theme_minimal(), # Minimal theme for clarity
             palette = "jco") # Color palette for clusters

# Unsupervised Machine Learning Tutorial ----
# K-Means Clustering ----

# Load necessary packages ----
# Ensure these packages are installed:
# 'tidyverse' for data manipulation, 'here' for file paths,
# 'readr' for reading CSV files, 'factoextra' for cluster visualizations,
# and 'stats' for performing k-means clustering.
library(tidyverse)
library(here)
library(stats)
library(readr)
library(factoextra)
options(scipen = 99999) # Prevent scientific notation in plots for clarity

# Load data ----
# Load datasets using 'here' to set file paths from the project root directory.
# If not using 'here', specify the file paths directly.
iat_codebook <- readxl::read_xlsx(here("Data/Race_IAT_public_2023_codebook.xlsx"))
load(here("Data/iat.rda")) # Load the cleaned dataset

# Filter only numeric variables from the dataset ----
# K-means clustering requires numeric variables. Filter the dataset to include only specified numeric variables.
variables_to_keep <- c("edu", "politicalid_7", 
                       "religion2014", "religionid", "mcpr1", "mcpr2", 
                       "mcpr3", "mcpr4", "mcpr5", "mcpr6", "mcpr7", 
                       "mcpr8", "mcpr9", "mcpr10", "mcpr11", "mcpr12", 
                       "mcpr13", "mcpr14", "mcpr15", "mcpr16", "mcpr17", 
                       "D_biep.White_Good_all", "Tblack_0to10", "Twhite_0to10")

iat_numeric <- iat_clean %>% 
  select(where(is.numeric)) %>% # Select only numeric columns
  select(where(~ var(.) > 1e-10)) %>% # Exclude columns with near-zero variance
  select(all_of(variables_to_keep)) # Include only specified variables

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
wss_values <- wss(iat_numeric, max_k = 10)

# Plot the Elbow Method
fviz_nbclust(iat_numeric, kmeans, method = "wss") + # Visualize WSS for each K
  geom_vline(xintercept = which.min(diff(wss_values)), linetype = 2) + # Add a line at the elbow point
  labs(title = "Elbow Method for Optimal K") # Title the plot

# K-Means Clustering ----
# Compute k-means clustering with a chosen number of clusters (e.g., K = 2)
# Euclidean distance is used to calculate the distance between points and cluster centers.
set.seed(1234) # Set seed for reproducibility in cluster assignment
km.res <- kmeans(iat_numeric, 2, nstart = 25) # Perform k-means with 2 clusters, 25 random starts

# This returns a list of components from the k-means algorithm:
# 'cluster': Cluster assignments for each point
# 'centers': Cluster centroids (means)
# 'totss': Total sum of squares (TSS) in the data
# 'withinss': Within-cluster sum of squares for each cluster
# 'tot.withinss': Total within-cluster sum of squares
# 'betweenss': Between-cluster sum of squares
# 'size': Number of observations in each cluster

# Print the results
print(km.res)

# Mean of each variable by cluster
cluster_mean <- aggregate(iat_numeric, by = list(cluster = km.res$cluster), mean)

# Appending the clusters to the original Dataset so that each participant is assigned to a cluster
iat_clusters <- cbind(iat_numeric, cluster = km.res$cluster)

# Visualize the K-means clustering results ----
# Use 'factoextra' to create a cluster plot
fviz_cluster(km.res, data = iat_numeric, 
             geom = "point", # Points represent data observations
             ellipse.type = "convex", # Convex hulls around clusters
             ggtheme = theme_minimal(), # Minimal theme for clarity
             palette = "jco") # Color palette for clusters

# The plot shows data points colored by cluster membership,
# with cluster centroids and convex hulls for visual clarity.

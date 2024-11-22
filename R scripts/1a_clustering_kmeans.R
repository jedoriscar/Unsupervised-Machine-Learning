# Unsupervised Machine Learning Tutorial ----
# K-Means Clustering ----

# Load necessary packages ----
# These packages are required to perform and visualize K-means clustering as well as manipulate data:
library(tidyverse)   # Comprehensive set of tools for data manipulation and visualization
library(here)        # Helps manage file paths relative to the project root directory
library(stats)       # Contains core statistical functions, including K-means
library(readr)       # For reading in CSV files
library(factoextra)  # Provides tools for visualizing clustering results
library(dplyr)       # A component of tidyverse, simplifies data manipulation
library(reshape2)    # Helps reshape data, useful in pre-processing steps
library(corrplot)    # For creating correlation matrix visualizations
library(Hmisc)       # Generates correlation matrices with associated p-values
library(haven)       # Enables working with datasets from SPSS, SAS, or Stata

# Set options to improve readability of output ----
options(scipen = 99999)  # Turns off scientific notation in output, ensuring results are easier to interpret

# Load data ----
# Here we load the necessary datasets for clustering. Using the 'here' package ensures consistent file paths,
# regardless of where the script is run. If you're not using 'here', replace `here()` with the specific file paths.

iat_codebook <- readxl::read_xlsx(here("Data/Race_IAT_public_2023_codebook.xlsx"))
# Codebook provides definitions of variables in the dataset, which is helpful for interpreting results.

load(here("Data/iat.rda")) # This is the cleaned dataset we will use for clustering.

# Filter only numeric variables from the dataset ----
# K-means clustering requires numeric data because the algorithm relies on distance calculations.
# In this step, we filter the dataset to retain only numeric variables. Non-numeric variables
# (e.g., categorical or text data) cannot be directly used for clustering.

variables_to_keep <- c("politicalid_7",           # Political ideology (e.g., liberal to conservative)
                       "D_biep.White_Good_all",   # Implicit attitudes (e.g., pro-White/anti-Black bias)
                       "Tblack_0to10")           # Neighborhood racial demographics (e.g., percentage Black)

iat_numeric <- iat_clean %>%
  select(where(is.numeric)) %>%            # Select only numeric variables from the dataset
  select(where(~ var(.) > 1e-10)) %>%      # Exclude variables with near-zero variance, as they provide no useful information
  select(all_of(variables_to_keep))        # Retain only the specified variables of interest for clustering

# Why we filter for numeric variables:
# K-means clustering relies on distance metrics (e.g., Euclidean distance), which are only meaningful for numeric data.
# Including non-numeric variables would result in errors or invalid calculations.

# Standardize the numeric variables ----
# Standardization ensures that all variables contribute equally to the clustering process by giving them
# the same mean (0) and standard deviation (1). Without standardization, variables with larger scales
# (e.g., percentages vs. bias scores) could dominate the distance calculation.

iat_numeric_scaled <- scale(iat_numeric)  # Center (mean = 0) and scale (standard deviation = 1) the data

# Why standardization is critical for clustering:
# - Clustering algorithms calculate the "distance" between data points. Variables with larger ranges can
#   disproportionately influence these distances, leading to biased clusters.
# - For example, if one variable ranges from 0 to 100 and another from 0 to 1, the first variable would dominate the results.
# - By standardizing, we ensure that all variables contribute equally to the clustering process.

# At this point, your data is ready to be input into the K-means clustering algorithm.
# The next steps would involve deciding the number of clusters (K) and running the algorithm.

# Determining Optimal Clusters ----
# Elbow Method: Determines the optimal number of clusters by plotting WSS vs. number of clusters (K)
# The "elbow" point where WSS starts to decrease slowly indicates the optimal K.

# Create a function to calculate total within-cluster sum of squares (WSS) for different K values
# This function iterates over a range of cluster numbers (1 to max_k) to calculate WSS for each K.
# WSS (Within-Cluster Sum of Squares) quantifies the compactness of the clusters; lower WSS values indicate tighter clusters.
# The function uses the `kmeans()` function, which:
# - Takes the scaled dataset (`data`) as input.
# - Uses `centers = k` to specify the number of clusters.
# - Uses `nstart = 25` to initialize k-means clustering with 25 random starting points to improve the stability of results.
wss <- function(data, max_k) {
  sapply(1:max_k, function(k) {
    kmeans(data, centers = k, nstart = 25)$tot.withinss # Extract total WSS for each K
  })
}

# Calculate WSS for K values from 1 to 10
# Here, we pass the scaled data (`iat_numeric_scaled`) to the WSS function and specify `max_k = 10`.
# This creates a vector (`wss_values`) of WSS values for each K from 1 to 10.
wss_values <- wss(iat_numeric_scaled, max_k = 10)

# Plot the Elbow Method
# The Elbow Method is a visual technique to identify the optimal number of clusters (K).
# By plotting WSS against K, we look for a "bend" or "elbow" in the curve, which indicates diminishing returns
# in reducing WSS as K increases. The elbow point is often chosen as the optimal K.
fviz_nbclust(iat_numeric_scaled, kmeans, method = "wss") + # Generate WSS plot using `factoextra`
  geom_vline(xintercept = which.min(diff(wss_values)), linetype = 2) + # Add a dashed vertical line at the elbow point
  labs(title = "Elbow Method for Optimal K") # Add a title to the plot

# K-Means Clustering ----
# Perform k-means clustering with a chosen number of clusters (e.g., K = 5)
# K-means is a clustering algorithm that groups data points into K clusters, minimizing the total within-cluster variance.
# The algorithm uses Euclidean distance by default to measure the distance between data points and their cluster centroids.

set.seed(1234) # Set seed for reproducibility
# Setting a random seed ensures that the cluster assignments are consistent each time the code is run,
# as k-means clustering involves a random initialization of centroids.

km.res <- kmeans(iat_numeric_scaled, 5, nstart = 25) 
# Perform k-means clustering on the scaled data ('iat_numeric_scaled').
# - `centers = 5`: Specifies the number of clusters (K) to partition the data into.
# - `nstart = 25`: Runs the algorithm 25 times with different random centroid initializations
#   and selects the clustering solution with the lowest within-cluster sum of squares.

# Output of `kmeans()`:
# The result is an object containing key components of the clustering process:
# - `cluster`: A vector indicating the cluster assignment for each data point (e.g., participant).
# - `centers`: The centroids (mean coordinates) of the clusters in the data space.
# - `totss`: Total sum of squares (TSS), measuring the total variance in the dataset.
# - `withinss`: A vector of within-cluster sum of squares for each cluster, measuring compactness.
# - `tot.withinss`: Total within-cluster sum of squares across all clusters, used for evaluating clustering fit.
# - `betweenss`: Between-cluster sum of squares, capturing the separation between clusters.
# - `size`: Number of data points assigned to each cluster, indicating cluster sizes.

# Print the results
print(km.res) 
# Displays a summary of the clustering results, including cluster sizes, cluster centers, and within-cluster sum of squares.

# Calculate the mean of each variable by cluster ----
# This helps in understanding the characteristics of each cluster
cluster_mean <- aggregate(iat_numeric_scaled, by = list(cluster = km.res$cluster), mean)

# Append the clusters to the original dataset ----
# This adds a new column to the dataset indicating the cluster assignment for each participant
iat_k_clusters <- cbind(iat_clean, cluster = km.res$cluster)

# Save the clustered k-means dataset ----
save(iat_k_clusters, file = here("data/iat_k_clusters.rda")) # Save the dataset with cluster assignments

# Visualize the K-means clustering results ----
# The 'factoextra' package provides an easy way to create cluster visualizations.

fviz_cluster(km.res,               # The k-means results object, containing cluster assignments
             data = iat_numeric_scaled, # The scaled dataset used for clustering
             geom = "point",            # Each data point is represented as a dot on the plot
             ellipse.type = "convex",   # Draw convex hulls around the clusters to show their boundaries
             ggtheme = theme_minimal(), # Apply a minimalistic theme for improved readability
             palette = "jco")           # Use the 'jco' palette for distinct and visually appealing cluster colors

# Key Notes:
# - `km.res`: This object contains the k-means clustering results, including cluster assignments for each data point.
# - `data`: The scaled dataset ensures that the clustering distances are not biased by variable scale differences.
# - `geom = "point"`: Visualizes individual data points, helping us identify cluster membership.
# - `ellipse.type = "convex"`: Convex hulls encapsulate the points in each cluster, showing their spatial grouping.
# - `ggtheme = theme_minimal()`: Provides a clean, publication-ready visual style.
# - `palette = "jco"`: Ensures clusters are color-coded for easy differentiation.

# Load necessary packages ----
# Unsupervised Machine Learning Tutorial ----
# Hierarchical Clustering ----

# Load necessary packages ----
# Ensure these packages are installed:
# 'tidyverse' for data manipulation, 'here' for file paths,
# 'factoextra' for visualizing the hierarchical clustering results.
library(tidyverse) # For data manipulation and visualization
library(here) # For managing file paths
library(factoextra) # For visualizing clustering results

# Load data ----
# Load datasets using 'here' to set file paths from the project root directory.
# If not using 'here', specify the file paths directly.
iat_codebook <- readxl::read_xlsx(here("Data/Race_IAT_public_2023_codebook.xlsx"))
load(here("Data/iat.rda")) # Load the cleaned dataset

# Filter the dataset to include only the specified variables ----
# Selecting variables of interest for hierarchical clustering
variables_to_keep <- c("politicalid_7", "D_biep.White_Good_all", "Tblack_0to10")

iat_hc_data <- iat_clean %>% 
  select(all_of(variables_to_keep)) %>% 
  na.omit() # Remove rows with missing values to avoid issues with clustering

# Standardize the data ----
# Hierarchical clustering can be sensitive to the scale of the data, so we standardize the variables.
iat_hc_data_scaled <- scale(iat_hc_data)

# Compute the distance matrix ----
# The distance matrix measures the dissimilarity between pairs of observations.
dist_matrix <- dist(iat_hc_data_scaled, method = "euclidean")

# Perform hierarchical clustering ----
# Using the Ward's method for hierarchical clustering
hc_res <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram ----
# The dendrogram shows the hierarchical relationships between clusters.
fviz_dend(hc_res, rect = TRUE, show_labels = FALSE, 
          main = "Dendrogram of Hierarchical Clustering")

# Cut the tree into clusters ----
# Specify the number of clusters (e.g., k = 3) to cut the dendrogram.
num_clusters <- 3
clusters <- cutree(hc_res, k = num_clusters)

# Append the cluster assignments to the original dataset ----
# Adding cluster labels to the dataset for further analysis
iat_hc_clusters <- iat_clean %>%
  select(all_of(variables_to_keep)) %>% 
  na.omit() %>% 
  mutate(cluster = clusters)

# Save the clustered dataset ----
# Saving the dataset with cluster assignments for future use
save(iat_hc_clusters, file = here("data/iat_hc_clusters.rda"))

# Visualize the clusters ----
# Plot the hierarchical clustering results with clusters colored
fviz_cluster(list(data = iat_hc_data_scaled, cluster = clusters),
             geom = "point", ellipse.type = "convex", 
             ggtheme = theme_minimal(), palette = "jco")

# Notes:
# - 'scale()' standardizes the data, ensuring that each variable contributes equally to the distance calculations.
# - 'dist()' computes the Euclidean distance between pairs of observations.
# - 'hclust()' performs hierarchical clustering using the Ward's method, which minimizes the variance within each cluster.
# - 'cutree()' cuts the dendrogram to form a specified number of clusters.
# - 'fviz_dend()' and 'fviz_cluster()' are used to visualize the dendrogram and the resulting clusters.

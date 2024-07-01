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

# K-Modes Clustering ----
# Compute K-modes clustering with a chosen number of clusters (e.g., K = 3)
# Suitable for categorical data.
set.seed(1234) # Set seed for reproducibility in cluster assignment
db.res <- dbscan(iat_numeric, eps = 0.45, MinPts = 5) 

# Print the clustering results
print(db.res)

table(db.res$cluster, iat_numeric$Tblack_0to10) 


# In DBSCAN, clusters are determined based on the parameters eps and MinPts:
#   
#   eps (Epsilon): Defines the radius of the neighborhood around a point. Points within this distance are considered neighbors.
# 
# MinPts: The minimum number of points required to form a dense region. If a point has at least MinPts within its eps neighborhood, it becomes a core point, leading to cluster formation.
# 
# Clusters form around core points, and noise points are labeled as outliers. The number of clusters isn't predetermined; it's based on data density and the chosen parameters.
plot(db.res, iat_numeric, main = "DBScan") 


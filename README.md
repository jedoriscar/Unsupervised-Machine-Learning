# Unsupervised Machine Learning Tutorial

This repository contains R scripts and data for a tutorial on Unsupervised Machine Learning (UML) methods in social cognition research. The tutorial is designed to supplement the accompanying article, guiding you through various UML techniques applied to a dataset involving Implicit Association Test (IAT) data.

## Introduction

Unsupervised Machine Learning (UML) is a powerful set of techniques used to uncover hidden patterns and structures in data without predefined labels or outcomes. Unlike supervised methods, UML does not rely on a target variable but instead focuses on understanding the inherent structure of the dataset. In social cognition research, UML can reveal insights into complex psychological phenomena by identifying clusters of similar participants, reducing dimensionality for easier interpretation, and exploring associations between different variables.

This tutorial provides practical examples of how to apply UML methods to a dataset of IAT scores, exploring implicit biases and their relationship with other demographic and psychological variables. By following along with the R scripts provided, you can learn how to implement UML techniques such as clustering, dimensionality reduction, and market basket analysis.

## Repository Structure

The repository is organized into the following folders and files:

### Data

This folder contains the datasets used in the tutorial.

- **iat_k_clusters.rda**: Contains k-means clustering results on IAT data.
- **iat_pca_data_with_scores.rda**: Contains PCA results with component scores.
- **iat.rda**: The primary dataset used for analysis, including IAT scores and related variables.
- **Race_IAT_public_2023_codebook.xlsx**: A codebook detailing the variables and their descriptions in the IAT dataset.

### R Scripts

Each script corresponds to a specific UML technique, providing step-by-step instructions and code for reproducing the analyses.

- **0_data_cleaning.R**: Prepares and cleans the IAT dataset, ensuring data quality and consistency before analysis.
- **1a_clustering_kmeans.R**: Applies k-means clustering to identify groups of participants with similar implicit and explicit biases.
- **1b_clustering_kmodes.R**: Implements k-modes clustering, an alternative method suitable for categorical data, to find clusters in the dataset.
- **2_clustering_density.R**: Uses density-based clustering (e.g., DBSCAN) to discover clusters based on data point density, identifying natural groupings of participants.
- **3_clustering_hierarchical.R**: Performs hierarchical clustering, providing a dendrogram to visualize the relationships between clusters.
- **4_dimensionality_reduction_PCA.R**: Conducts Principal Component Analysis (PCA) to reduce dimensionality and identify the most informative components of the dataset.
- **5_market_basket_analysis.R**: Executes Market Basket Analysis to uncover frequent co-occurrences of explicit attitudes and IAT scores, generating association rules.

### Project Files

- **Unsupervised-Machine-Learning.Rproj**: RStudio project file for easy access to scripts and data.
- **README.md**: This file, providing an overview and guide to the repository.

## Getting Started

To get started with the tutorial, follow these steps:

1. **Clone the repository**:
   ```bash
   git clone https://github.com/your-username/unsupervised-machine-learning-tutorial.git
   cd unsupervised-machine-learning-tutorial
   ```

2. **Open the R project**: Double-click the `Unsupervised-Machine-Learning.Rproj` file to open the project in RStudio.

3. **Install necessary packages**: Ensure that the required R packages are installed. You can run the following command in R:
   ```r
   install.packages(c("tidyverse", "cluster", "factoextra", "arules", "arulesViz"))
   ```

4. **Run the scripts**: Execute the R scripts in the order listed above to reproduce the analyses and explore the UML methods discussed in the tutorial.

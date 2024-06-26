# Load necessary libraries
library(ggplot2)
library(ggfortify)
library(readr)
library(dplyr)
library(tidyr)
library(factoextra)


# PCA Analysis

Customers <- read.csv("~/Documents/19. GITHUB/Portfolio/CustomerClustering/Customers.csv")

Customers$Gender = as.factor(Customers$Gender)
Customers$Profession = as.factor(Customers$Profession)

# Categorize Spending Score into 'Low', 'Medium', 'High'
Customers$Spending.Score.Category <- ifelse(Customers$Spending.Score..1.100. <= 20, 'Low',
                                            ifelse(Customers$Spending.Score..1.100. > 20 & Customers$Spending.Score..1.100. <= 50, 'Medium',
                                                   'High'))
# One-hot encode categorical variables
Customers <- Customers %>%
  mutate_if(is.factor, as.numeric)

str(Customers)

# Perform PCA
pca_result <- prcomp(Customers, scale. = TRUE)

# Extract the loadings
loadings <- as.data.frame(pca_result$rotation)

# Extract the scores
scores <- as.data.frame(pca_result$x)

# Enhanced biplot using factoextra and coloring by Spending Score
fviz_pca_biplot(pca_result,
                geom.ind = "point", # Show points only (but not "text")
                col.ind = Customers$Spending.Score.Category, # Color by Spending Score
                palette = "jco", # Color palette
                addEllipses = TRUE, # Concentration ellipses
                label = "var", # Label variables
                col.var = "black", # Variables color
                repel = TRUE # Avoid text overlapping
) +
  labs(title = "PCA Biplot",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Spending Score") +
  theme_minimal()

hist(Customers$Spending.Score..1.100.)

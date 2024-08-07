# Load necessary libraries
library(ggplot2)
library(ggfortify)
library(readr)
library(dplyr)
library(tidyr)
library(factoextra)

# Load the dataset
Customers <- read.csv("~/Documents/19. GITHUB/Portfolio/CustomerClustering/Customers.csv")


# Convert Gender and Profession columns to factors
Customers$Gender <- as.factor(Customers$Gender)
Customers$Profession <- as.factor(Customers$Profession)

# Categorize Spending Score into 'Low', 'Medium', 'High'
Customers$Spending.Score.Category <- ifelse(Customers$Spending.Score..1.100. <= 20, 'Low',
                                            ifelse(Customers$Spending.Score..1.100. > 20 & Customers$Spending.Score..1.100. <= 50, 'Medium',
                                                   'High'))

# One-hot encode categorical variables
Customers <- Customers %>%
  mutate_if(is.factor, as.numeric)

# Print the structure of the dataset to see the data types of all columns
str(Customers)

# Perform PCA, excluding the non-numeric and non-required columns
pca_data <- Customers %>% select(-Spending.Score..1.100., -Spending.Score.Category, -CustomerID)
pca_result <- prcomp(pca_data, scale. = TRUE)

# Extract the loadings
loadings <- as.data.frame(pca_result$rotation)

# Extract the scores
scores <- as.data.frame(pca_result$x)

# Enhanced biplot using factoextra and coloring by Spending Score Category
fviz_pca_biplot(pca_result,
                geom.ind = "point", # Show points only (but not "text")
                col.ind = Customers$Spending.Score.Category, # Color by Spending Score Category
                palette = "jco", # Color palette
                addEllipses = TRUE, # Concentration ellipses
                label = "var", # Label variables
                col.var = "black", # Variables color
                repel = TRUE # Avoid text overlapping
) +
  labs(title = "PCA Biplot",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Spending Score Category") +
  theme_minimal()

# Scree plot to visualize the explained variance
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50)) +
  labs(title = "Scree Plot",
       x = "Principal Components",
       y = "Percentage of Variance Explained") +
  theme_minimal()

# Histogram of Spending Score
ggplot(Customers, aes(x = Spending.Score..1.100.)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Spending Score",
       x = "Spending Score (1-100)",
       y = "Frequency") +
  theme_minimal()



fviz_pca_biplot(pca_result, axes = c(1,2), cex = 4,labelsize = 3,geom = "point",pointsize = 2,
                habillage = Customers$Spending.Score.Category)

fviz_contrib(pca_result, axes = 1, choice = "var")
fviz_contribib(pca_result, axes = 2, choice = "var")
fviz_contrib(pca_result, axes = 3, choice = "var")

fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 40))

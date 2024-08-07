# Load necessary libraries
library(ggplot2)
library(ggfortify)
library(readr)
library(dplyr)
library(tidyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the dataset
data <- read_csv("Customers.csv")
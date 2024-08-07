# ---------------------- #
# --- ANALYSIS SETUP --- #
# ---------------------- #
library(party) 
library(caret)
library(randomForest) 
library(e1071) 

df <- read.csv("P:/6. DSMA/7. Seminar Data Science and Marketing Analytics/Practical Presenation 1/heart.csv")

# -- Making all chr features into factor variables
for (col_name in names(df)) {
  if (is.character(df[[col_name]])) {
    df[[col_name]] <- as.factor(df[[col_name]])
  }
}

df$HeartDisease = factor(df$HeartDisease, levels = c(1,0), labels = c("Heart Disease","Normal"))

# Edit Cholosterol levels
df$Cholesterol = ifelse(df$Cholesterol == 0, NA, df$Cholesterol)

# Remove NAs
df = na.omit(df)

# ------------------------------- #
# ------ CROSS VALIDATION ------- #
# ------------------------------- #

set.seed(123) # For reproducibility



# Define the ranges for each parameter
maxdepth_range <- 1:20
mincriterion_range <- seq(0.90, 1, by = 0.01)
minsplit_range <- 1:30

# Prepare to store results
results <- data.frame(maxdepth = integer(10000),
                      mincriterion = numeric(10000),
                      minsplit = integer(10000),
                      accuracy = numeric(10000))

# Split the data into training and testing sets
index <- createDataPartition(df$HeartDisease, p = 0.8, list = FALSE)
train_data <- df[index, ]
test_data <- df[-index, ]

for (i in 1:1000) {
  # Sample parameters
  maxdepth <- sample(maxdepth_range, 1)
  mincriterion <- sample(mincriterion_range, 1)
  minsplit <- sample(minsplit_range, 1)
  
  # Build the tree
  control_parameters <- ctree_control(maxdepth = maxdepth,
                                      mincriterion = mincriterion,
                                      minsplit = minsplit)
  conditional_tree <- ctree(HeartDisease ~ ., data = train_data, controls = control_parameters)
  
  # Predict on test data
  predictions <- predict(conditional_tree, test_data)
  
  # Calculate accuracy
  accuracy <- sum(predictions == test_data$HeartDisease) / nrow(test_data)
  
  # Store the results
  results[i, ] <- c(maxdepth, mincriterion, minsplit, accuracy)
}

# Find the best model
best_model <- results[which.max(results$accuracy), ]
print(best_model)

# Train the model (Naive Model) # Cross Validated Parameters equipped
control_parameters <- ctree_control(maxdepth = 8,
                                    mincriterion = 0.96,
                                    minsplit = 2)
conditional_tree <- ctree(HeartDisease ~ ., data = df, controls = control_parameters)

# Plot the conditional inference tree
plot(conditional_tree,
     main = "Conditional Inference Tree for Heart Disease",
     inner_panel = node_inner(conditional_tree, 
                              pval = TRUE, 
                              id = FALSE, 
                              abbreviate = FALSE,
                              fill = c("grey", "red")),  # Fill color for terminal nodes
     edge.lwd = 500)  # Line width for edges



# -------------------------------- #
# ---- RUNNING ALL OTHER MODELS -- #
# -------------------------------- #


# Splitting data into training and test sets
set.seed(123)  # For reproducibility
splitIndex <- createDataPartition(df$HeartDisease, p = 0.75, list = FALSE)
trainData <- df[splitIndex, ]
testData <- df[-splitIndex, ]

# Logistic Regression
logitModel <- glm(HeartDisease ~ ., data = trainData, family = 'binomial')
logitPredictions <- predict(logitModel, testData, type = "response")
logitPredictions <- ifelse(logitPredictions > 0.5, "Heart Disease", "Normal")
logitAccuracy <- mean(logitPredictions == testData$HeartDisease)

# Random Forest
rfModel <- randomForest(HeartDisease ~ ., data = trainData)
rfPredictions <- predict(rfModel, testData)
rfAccuracy <- mean(rfPredictions == testData$HeartDisease)

# Support Vector Machine
svmModel <- svm(HeartDisease ~ ., data = trainData)
svmPredictions <- predict(svmModel, testData)
svmAccuracy <- mean(svmPredictions == testData$HeartDisease)

# Conditional Inference Trees
control_parameters <- ctree_control(maxdepth = 8,
                                    mincriterion = 0.96,
                                    minsplit = 2)
conditional_tree <- ctree(HeartDisease ~ ., data = trainData, controls = control_parameters)
tree_predicitons = predict(conditional_tree, testData, type = "response")
tree_accuracy = mean(tree_predicitons == testData$HeartDisease)

# Making Comparable table with accuracies
comp_table = data.frame(
  Model = c("Logit", "RF", "SVM", "Con. Inf. Tree"),
  Accuracy = c(logitAccuracy, rfAccuracy, svmAccuracy, tree_accuracy)
)


######### 
#Starting Local Interpretation 
library(randomForest)
library(xgboost)
library(SHAPforxgboost)
library(shapr)

str(df)
df$HeartDisease <- as.numeric(df$HeartDisease) - 1


data_matrix <- model.matrix(HeartDisease ~ ., data = df)
label <- df$HeartDisease

# Convert to xgb.DMatrix
dtrain <- xgb.DMatrix(data = data_matrix, label = label)

xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "binary:logistic")

##
shap_long <- shap.prep(xgb_model = xgb_model, X_train = data_matrix)
shap.plot.summary(shap_long)


library(lime) # Local Interpretable Model-Agnostic Explanations

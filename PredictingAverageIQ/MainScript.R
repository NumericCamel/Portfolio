# Final Presentation

# ----------------------------- #
# ---- STEP 1: INITIATING ----- #
# ----------------------------- #

library(randomForest)
library(caret)
library(performance)
library(Metrics)
library(ggplot2)
library(factoextra)
library(SHAPforxgboost)
library(xgboost)
library(knitr)
library(shapviz)
library(ggcorrplot)
library(doParallel)
library(iml)
library(pdp)
library(missForest)
library(rpart)
library(rpart.plot)
library(mlr)
library(mlrMBO)
library(DiceKriging)

country.data <- read.csv("P:/6. DSMA/7. Seminar Data Science and Marketing Analytics/Final Document/Code + Data/world-data-2023.csv")
country.iq <- read.csv("P:/6. DSMA/7. Seminar Data Science and Marketing Analytics/Final Document/Code + Data/avgIQpercountry.csv", header=FALSE)

full.df <- merge(country.data, country.iq, by = "Country")

# Setup function below for easy repetition 
comma = function(column) {
  column = gsub(",", "", column)
  column = as.numeric(column)
  return(column)
}

percentt = function(column) {
  column = gsub("%", "", column)
  column = as.numeric(column)
  return(column)
}

usd = function(column) {
  column = gsub("$", "", column)
  column = as.numeric(column)
  return(column)
}

replace_blank_with_na <- function(dataset) {
  dataset <- as.data.frame(dataset)
  dataset[dataset == ""] <- NA
  return(dataset)
}

calculate_r_squared <- function(actual, predicted) {
  # Calculate the total sum of squares
  total_sum_of_squares <- sum((actual - mean(actual))^2)
  # Calculate the residual sum of squares
  residual_sum_of_squares <- sum((actual - predicted)^2)
  # Calculate R-squared
  r_squared <- 1 - (residual_sum_of_squares / total_sum_of_squares)
  return(r_squared)
}

calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

# ------------------------------- #
# -------- Data Cleaning -------- #
# ------------------------------- #
full.df.1 = full.df[,-c(1,3,8,9,13,21,25,34,35,36,38,44)] # Remove unneeded variables

# Below section is mainly for data format
full.df.1$Density..P.Km2. = gsub(",", "", full.df.1$Density..P.Km2.)
full.df.1$Density..P.Km2. = as.numeric(full.df.1$Density..P.Km2.)

full.df.1$Agricultural.Land.... = gsub("%", "", full.df.1$Agricultural.Land....)
full.df.1$Agricultural.Land.... = as.numeric(full.df.1$Agricultural.Land....)

full.df.1$Land.Area.Km2. = gsub(",", "", full.df.1$Land.Area.Km2.)
full.df.1$Land.Area.Km2. = as.numeric(full.df.1$Land.Area.Km2.)

full.df.1$Armed.Forces.size = gsub(",", "", full.df.1$Armed.Forces.size)
full.df.1$Armed.Forces.size = as.numeric(full.df.1$Armed.Forces.size)

full.df.1$Co2.Emissions = gsub(",", "", full.df.1$Co2.Emissions)
full.df.1$Co2.Emissions = as.numeric(full.df.1$Co2.Emissions)

full.df.1$CPI = as.numeric(full.df.1$CPI)

full.df.1$CPI.Change.... = gsub("%", "", full.df.1$CPI.Change....)
full.df.1$CPI.Change.... = as.numeric(full.df.1$CPI.Change....)

full.df.1$Forested.Area.... = percentt(full.df.1$Forested.Area....)

full.df.1$Gasoline.Price = gsub("\\$", "", full.df.1$Gasoline.Price)
full.df.1$Gasoline.Price = as.numeric(full.df.1$Gasoline.Price)

full.df.1$GDP = gsub("\\$", "", full.df.1$GDP)
full.df.1$GDP = gsub(",", "", full.df.1$GDP)
full.df.1$GDP = as.numeric(full.df.1$GDP)

full.df.1$Gross.primary.education.enrollment.... = percentt(full.df.1$Gross.primary.education.enrollment....)

full.df.1$Gross.tertiary.education.enrollment.... = percentt(full.df.1$Gross.tertiary.education.enrollment....)

full.df.1$Minimum.wage = gsub("\\$", "", full.df.1$Minimum.wage)
full.df.1$Minimum.wage = as.numeric(full.df.1$Minimum.wage)

full.df.1$Out.of.pocket.health.expenditure = percentt(full.df.1$Out.of.pocket.health.expenditure)

full.df.1$Population = gsub(",", "", full.df.1$Population)
full.df.1$Population = as.numeric(full.df.1$Population)

full.df.1$Population..Labor.force.participation.... = percentt(full.df.1$Population..Labor.force.participation....)

full.df.1$Tax.revenue....  = percentt(full.df.1$Tax.revenue.... )

full.df.1$Total.tax.rate  = percentt(full.df.1$Total.tax.rate )

full.df.1$Unemployment.rate  = percentt(full.df.1$Unemployment.rate )

full.df.1$Urban_population = gsub(",", "", full.df.1$Urban_population)
full.df.1$Urban_population = as.numeric(full.df.1$Urban_population)

x <- replace_blank_with_na(full.df.1)
x = x[rowSums(is.na(x)) <= 4, ] # Remove rows that have more than 4 NAs in total

x <- missForest(x)$ximp # Using missForest to impute NAs

# ------------------------------- #
# --------- Exploration --------- #
# ------------------------------- #
for (i in 1:ncol(x)) {
  hist(x[,i], main =colnames(x)[i], xlab = "Distribution")
}

# Making a 3x2 histogram 
par(mfrow = c(3,3))

hist(full.df$Literacy.Rate, col = "turquoise", main = "Distribution of Literacy Rate", xlab = "Literacy Rate", ylab = "Frequency", 
     las = 1, font.main = 2, font.lab = 2) #
hist(full.df$Birth.Rate, col = "turquoise", main = "Distribution of Birth Rate", xlab = "Birth Rate", ylab = "Frequency", 
     las = 1, font.main = 2, font.lab = 2) #
hist(full.df$Life.expectancy, col = "turquoise", main = "Distribution of Life Expectancy", xlab = "Life Expectancy", ylab = "Frequency", 
     las = 1, font.main = 2, font.lab = 2) #
hist(full.df$Maternal.mortality.ratio, col = "turquoise", main = "Distribution of Maternal Mortality Ratio", xlab = "Maternal Mortality Ratio", ylab = "Frequency", 
     las = 1, font.main = 2, font.lab = 2) #
hist(full.df$Average.IQ, col = "maroon", main = "Distribution of Average IQ", xlab = "Average IQ", ylab = "Frequency", 
     las = 1, font.main = 2, font.lab = 2) #
hist(full.df.1$CPI, col = "turquoise", main = "Distribution of CPI", xlab = "CPI", ylab = "Frequency", 
     las = 1, font.main = 2, font.lab = 2) #
hist(full.df$Physicians.per.thousand, col = "turquoise", main = "Distribution of Physicians per Thousand", xlab = "Physicians per Thousand", ylab = "Frequency", 
     las = 1, font.main = 2, font.lab = 2) #
hist(full.df$Mean.years.of.schooling...2021, col = "turquoise", main = "Distribution of Avg. Years Schooling", xlab = "Avg. Years Schooling", ylab = "Frequency", 
     las = 1, font.main = 2, font.lab = 2) #
hist(full.df$GNI...2021, col = "turquoise", main = "Distribution of GNI", xlab = "GNI", ylab = "Frequency", 
     las = 1, font.main = 2, font.lab = 2) #


# Literacy vs. Avg. IQ
ggplot(data = full.df, aes(x = full.df$Literacy.Rate, y = full.df$Average.IQ, color = full.df$Continent)) + 
  geom_point(size = 3) +
  labs(
    title = "Literacy Rate vs. Avg. IQ",
    x = "Literacy Rate",
    y = "Average IQ"
  ) +
  theme_minimal()

print(mean(x$Mean.years.of.schooling...2021))
print(mean(x$Out.of.pocket.health.expenditure))
print(max(x$Out.of.pocket.health.expenditure))
print(mean(subset(full.df$Maternal.mortality.ratio, full.df$Continent == "Africa")))
mean(subset(full.df$Mean.years.of.schooling...2021, full.df$Continent == "Europe" & !is.na(full.df$Mean.years.of.schooling...2021)), na.rm = TRUE)

# Out of pocket health expenditure vs analysis
full.df.na = full.df[c(17,26,37,38)]
full.df.na <- replace_blank_with_na(full.df.na)
full.df.na$Out.of.pocket.health.expenditure = percentt(full.df.na$Out.of.pocket.health.expenditure)
full.df.na$GDP = gsub("\\$", "", full.df.na$GDP)
full.df.na$GDP = gsub(",", "", full.df.na$GDP)
full.df.na$GDP = as.numeric(full.df.na$GDP)

full.df.na$GDP = log(full.df.na$GDP)

ggplot(data = full.df.na, aes(x = Average.IQ, y = Out.of.pocket.health.expenditure, color = Continent)) + 
  geom_point(size = 3) +
  labs(
    title = "Literacy Rate vs. Avg. IQ",
    x = "IQ",
    y = "Out of pocket health expenditure"
  ) +
  theme_minimal()



grouped_data_lr <- aggregate(`Out.of.pocket.health.expenditure` ~ Continent, data = full.df.na, FUN = mean, na.rm = TRUE)

hist(full.df.na$Out.of.pocket.health.expenditure)

full.df.na = subset(full.df.na, full.df.na == !is.na(full.df.na))

grouped_data_lr <- aggregate(`Out.of.pocket.health.expenditure` ~ Continent, data = full.df.na, FUN = mean, na.rm = TRUE)
print(grouped_data_lr)

# Correlation Matrix
correlation_matrix <- cor(full.df.1)
ggcorrplot(correlation_matrix, method = "circle")

# ---------------------------------------------------------------------------------------------------- #
# ---------------------------------------------- Analysis -------------------------------------------- #
# ---------------------------------------------------------------------------------------------------- #
lm = lm(Average.IQ ~., data = x)
summary(lm)

rf = randomForest(Average.IQ ~ ., data = train_data, ntree = 10, maxnodes = 10)

rf_predic = predict(rf, test_data)
rf_rmse = rmse(test_data$Average.IQ, rf_predic)
print(rf_rmse)

# -------------------------------- # ----------------------------------------------------------------- #
# --------------------------------------------- Xgb.Boost Model -------------------------------------- #
# --------- # ---------------------------------------------------------------------------------------- #

# Split the data into training and testing sets
index <- createDataPartition(x$Average.IQ, p = 0.8, list = FALSE)
train_data <- x[index, ]
test_data <- x[-index, ]

# Control for cross-validation
control <- trainControl(method = "cv", 
                        number = 10,
                        verboseIter = TRUE)

# Define a grid
xgbGrid <- expand.grid(nrounds = c(50, 100, 150), 
                       max_depth = c(3, 6, 9, 12, 15),
                       eta = c(0.1, 0.2, 0.3),
                       gamma = c(0, 0.01, 0.1),
                       colsample_bytree = c(0.5, 1),
                       min_child_weight = c(1),
                       subsample = c(0.75, 1))

# Train the model using caret 
registerDoParallel(cores = 12) # Multi-Core Setup

set.seed(123)
xgbModel <- caret::train(Average.IQ ~ .,  # Main traning of the model via cv
                         data = train_data, 
                         method = "xgbTree",
                         trControl = control,
                         tuneGrid = xgbGrid,
                         metric = "RMSE")


stopImplicitCluster() #return to single core compute

# Print the best model
print(xgbModel$bestTune)


# Variable Importance - xgbBoost
data_matrix <- as.matrix(train_data[, -which(names(train_data) == "Average.IQ")])
importance_matrix <- xgb.importance(feature_names = colnames(data_matrix), model = xgbModel$finalModel)
xgb.plot.importance(importance_matrix)

to_print = importance_matrix[,c(1,5)]
print(to_print)

# model.matrix with same columns as the one used in caret finalModel
train_data.mat <- model.matrix(Average.IQ ~ . -1, data = train_data)

# Make SHAP values object
SHAP <- shapviz(xgbModel$finalModel, train_data.mat, interactions = TRUE)

# Showing plots
sv_importance(SHAP, show_numbers = TRUE)
sv_importance(SHAP, show_numbers = TRUE, kind = "bee")

sv_force(SHAP, row_id = 129)
sv_waterfall(SHAP, row_id = 30)

## Find that instance
filtered_df <- full.df %>% 
  filter(Literacy.Rate == 0.99) %>% 
  filter(Infant.mortality == 1.9)

filtered_df

train_data[6,]

library(dplyr)
### Interaction plots
sv_interaction(SHAP, max_display = 3)

## Making SHAP for trainig data
# First make model.matrix with same columns as the one used in caret finalModel
test_data.mat <- model.matrix(Average.IQ ~ . -1, data = test_data)

# Make SHAP values object
SHAP_t <- shapviz(xgbModel$finalModel, test_data.mat, interactions = TRUE)

# Showing plots
sv_importance(SHAP_t, show_numbers = TRUE)
sv_importance(SHAP_t, show_numbers = TRUE, kind = "bee")

sv_force(SHAP_t, row_id = 3)

# Outliers from the model in terms of misprediction
par(mfrow =c(2,1))
sv_waterfall(SHAP_t, row_id = 32) + ggtitle("Yemen Prediction") + labs(subtitle = "Actual IQ: 62.9 | Predicted IQ: 80.9")
sv_waterfall(SHAP_t, row_id = 1) + ggtitle("Antigua and Barbuda") + labs(subtitle = "Actual IQ: 70.5 | Predicted IQ: 86.6")# Why did the model have such a big difference in actual vs real prediction



# Generate predictions for the test data
xgbTestPredictions <- predict(xgbModel, newdata = test_data)
y_acc = test_data$Average.IQ
data_res = data.frame(xgbTestPredictions, y_acc)

rmse = rmse(y_acc, xgbTestPredictions)
print(rmse)

r2 = calculate_r_squared(y_acc, xgbTestPredictions)
print(r2)

# Scatter plot of actual vs predicted values
ggplot(data_res, aes(x = y_acc, y = xgbTestPredictions)) + 
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Actual Values", y = "Predicted Values", title = "Actual vs. Predicted Values on Test Set")


# Get together the predictions vs. the model and the country itself
gen1 = as.data.frame(cbind(data_res, test_data))
land = full.df[,c(1,5)]
land$Land.Area.Km2. = comma(land$Land.Area.Km2.)

xgb_exp = merge(gen1, land, by = "Land.Area.Km2.")
xgb_exp$Residual = xgb_exp$y_acc - xgb_exp$xgbTestPredictions

# PCA - Extra code just for fun (not included in main report)
pca_df = x[,-27]
q = x

# Classify IQ
q$IQ_Class <- ifelse(q$Average.IQ >= 100, "High Intelligence",
                            ifelse(q$Average.IQ >= 90, "Above Average",
                                   ifelse(q$Average.IQ >= 80, "Average", 
                                          ifelse(q$Average.IQ >= 70, "Below Average",
                                          "Low Intelligence"))))

pca_result = prcomp(pca_df, scale. = TRUE)
fviz_pca_biplot(pca_result, axes = c(1,2), cex = 6,labelsize = 3,geom = "point",pointsize = 3,
                habillage = q$IQ_Class)

fviz_contrib(pca_result, axes = 1, choice = "var")
fviz_contribib(pca_result, axes = 2, choice = "var")
fviz_contrib(pca_result, axes = 3, choice = "var")

fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 40))


# ----------- #
# ICE PLOTS -- #

predictor <- Predictor$new(model = xgbModel, data = train_data, y = train_data$Average.IQ)
feature = "Maternal.mortality.ratio"

ice <- FeatureEffect$new(predictor, feature = feature, method = "ice")

plot(ice)


## Doing a histogram on maternal mortality ratio 
maternal = subset(full.df.1$Maternal.mortality.ratio, full.df.1$Maternal.mortality.ratio < 150)
histogram(maternal, xlab = "Maternal Mortality Ratio")



## PDP Plots

# Generate PDP for 'feature1'
pdp_result <- partial(xgbModel, pred.var = "Maternal.mortality.ratio", grid.resolution = 20)

# Plot the PDP
plot(pdp_result, xlab = "Maternal Mortality Ratio (MMR)", ylab = "Partial Dependence", main = "MMR on Average IQ")

par(mfrow = c(1,1))


# Testing other models now for comparissons

# Linear model
lm = lm(Average.IQ ~., data = train_data)
lm_pred = predict(lm, newdata = test_data)
lm_rmse = calculate_rmse(y_acc, lm_pred)
lm_r2 = calculate_r_squared(y_acc, lm_pred)
summary(lm)

# Random Forest Model
rf = randomForest(Average.IQ ~ ., data = train_data)
rf_pred = predict(rf, newdata = test_data)
rf_rmse = calculate_rmse(y_acc, rf_pred)
rf_r2 = calculate_r_squared(y_acc, rf_pred)
varImpPlot(rf)
varUsed(rf)

# ------------- #
# - Decision Tree -- #
decision_tree_model <- rpart(Average.IQ ~ ., data = train_data, method = "anova")
rpart.plot(decision_tree_model)






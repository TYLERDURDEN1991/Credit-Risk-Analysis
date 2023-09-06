# Install required packages
install.packages("tidyverse")
install.packages("tidymodels")
# Load the libraries
library(tidyverse)
library(tidymodels)
library(recipes)
# Loading application data
application_data <- read_csv("D:/Credit/application_record.csv")
# Loading credit data
credit_data <- read_csv("D:/Credit/credit_record.csv")
# First 5 rows of the application data
head(application_data, 5)
str(application_data)
# First 5 rows of the credit data
head(credit_data, 5)
str(credit_data)
# Application data info
glimpse(application_data)
# Credit data info
glimpse(credit_data)
# Checking unique values of application data
unique_values <- application_data %>%
  distinct(ID) %>%
  count()
print(unique_values)
# Removing duplicate values from application data
application_data <- application_data %>%
  distinct(ID, .keep_all = TRUE)
glimpse(application_data)
# Checking unique values of credit data
unique_values <- credit_data %>%
  distinct(ID) %>%
  count()
print(unique_values)
# Records matching in two datasets
common_values <- application_data %>%
  semi_join(credit_data, by = "ID") %>%
  count()
print(common_values)
# Missing values in application data
missing_values <- application_data %>% summarise(across(everything(), ~ sum(is.na(.), na.rm = TRUE))) %>% tidyr::gather(variable, missing_values)
 print(missing_values)

# Miissing values in credit data
missing_values <- credit_data %>% summarise(across(everything(), ~ sum(is.na(.))))
print(missing_values)
# Replacing missing values in 'OCCUPATION_TYPE' column with 'XNA'
application_data$OCCUPATION_TYPE <- ifelse(is.na(application_data$OCCUPATION_TYPE), "NA", application_data$OCCUPATION_TYPE)
# Converting categorical data to numeric data in application record
conversion_recipe <- recipe(application_data) %>%
  update_role(all_outcomes(), new_role = "outcome", old_role = NULL) %>%
  update_role(all_predictors(), new_role = "predictor", old_role = NULL) %>%
  step_string2factor(all_nominal()) %>%
  step_integer(all_nominal())
# Apply the recipe to the data
converted_data <- conversion_recipe %>% prep() %>% bake(new_data = NULL, composition = "matrix")
# Convert the converted data to a data frame
converted_data <- as.data.frame(converted_data)
application_data<-converted_data
glimpse(application_data)
head(application_data, 10)

# Plotting fields with numeric values
application_data %>%
  select(ID, CNT_CHILDREN, AMT_INCOME_TOTAL, DAYS_BIRTH, DAYS_EMPLOYED, CNT_FAM_MEMBERS) %>%
  pivot_longer(-ID, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = ID, y = Value, color = Variable)) +
  geom_point() +
  facet_wrap(~ Variable, scales = "free") +
  labs(x = "ID", y = "Value") +
  theme_minimal()
# Removing outliers from numeric variables
application_data <- application_data %>%
          filter(CNT_CHILDREN > quantile(CNT_CHILDREN, 0.001) &
                 CNT_CHILDREN < quantile(CNT_CHILDREN, 0.999),
                 AMT_INCOME_TOTAL > quantile(AMT_INCOME_TOTAL, 0.001) &
                 AMT_INCOME_TOTAL < quantile(AMT_INCOME_TOTAL, 0.999),
                 CNT_FAM_MEMBERS > quantile(CNT_FAM_MEMBERS, 0.001) &
                 CNT_FAM_MEMBERS < quantile(CNT_FAM_MEMBERS, 0.999))
glimpse(application_data)
# Plotting cleaned numeric variables
application_data %>%
  select(ID, CNT_CHILDREN, AMT_INCOME_TOTAL, DAYS_BIRTH, DAYS_EMPLOYED, CNT_FAM_MEMBERS) %>%
  pivot_longer(-ID, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = ID, y = Value, color = Variable)) +
  geom_point() +
  facet_wrap(~ Variable, scales = "free") +
  labs(x = "ID", y = "Value") +
  theme_minimal()


# Checking unique values for status field in credit data
status_counts <- credit_data %>%
  count(STATUS)
print(status_counts)
# Creating a new DataFrame 'credit_month_data' by grouping 'credit_data' based on the 'ID' column and aggregating the 'MONTHS_BALANCE' column using the 'min' function (earliest month for each ID)

credit_month_data <- credit_data %>%
  group_by(ID) %>%
  summarise(MONTHS_BEGINNING = min(MONTHS_BALANCE))
# Step 2: Renaming the column
#names(credit_month_data)[names(credit_month_data) == "MONTHS_BEGINNING"] <- "MONTHS_BEGINNING"
# Step 3: Multiplying the 'MONTHS_BEGINNING' column by -1 to remove -ve sign
credit_month_data$MONTHS_BEGINNING <-  credit_month_data$MONTHS_BEGINNING * -1
str(credit_month_data)
# Correcting the data in STATUS column
credit_data <- credit_data %>%
  mutate(STATUS = ifelse(STATUS %in% c("X", "C"), -1, STATUS)) %>%
  mutate(STATUS = ifelse(STATUS > 0, 1, 0))
glimpse(credit_data)
head(credit_data, 5)

credit_target_data <- credit_data %>%
  group_by(ID) %>%
  summarise(STATUS = max(STATUS))
# Count of credit status
status_counts <- credit_data %>%
  count(STATUS)
print(status_counts)
status_freq <- credit_data %>%
  count(STATUS) %>%
  mutate(Frequency = n / sum(n))
print(status_freq)
# Grouping credit data by loan applicant ID
credit_data_final <- credit_data %>%
  group_by(ID) %>%
  summarise(across(everything(), max))
head(credit_data_final, 10)
glimpse(credit_data_final)
# Merging both datasets
full_data <- application_data %>%
  inner_join(credit_month_data, by = "ID")
head(full_data)
glimpse(full_data)
full_data <- full_data %>%
  inner_join(credit_target_data, by = "ID")
head(full_data)
str(full_data)
# Checking unique values for status field in merged data
status_counts <- full_data %>%
  count(STATUS)
print(status_counts)
status_freq <- full_data %>%
  count(STATUS) %>%
  mutate(Frequency = n / sum(n))
print(status_freq)
####
# Assuming your dataset is named "full_data"
install.packages("corrplot")
library(corrplot)

correlation_matrix <- cor(full_data)
corrplot(correlation_matrix, method = "color")
#determine correlation threshold
correlation_threshold <- 0.7
##findning highly correlated fileds and decide which one to remove based on their importance in the context
highly_correlated_pairs <- which(correlation_matrix > correlation_threshold & correlation_matrix < 1, arr.ind = TRUE)


#removing CNT_CHILDREN
full_data <- full_data %>%
  select(-CNT_CHILDREN)
glimpse(full_data)
# Splitting data into predictors (X) and response variable (y)
X <- full_data %>%
  select(-STATUS)
y <- full_data$STATUS
glimpse(X)
# Set the random seed for reproducibility
set.seed(42)

# Combine X and y into a single data frame
data <- bind_cols(X, y)
print(data)
data <- data %>%
  rename(OUTCOME = `...19`)
print(data)
data[OUTCOME] <- as.factor(data[OUTCOME])
# Next, define test size
test_size <- 0.7
# Perform the initial split
data_split <- initial_split(data, prop = test_size)

# Extract the training and testing datasets
train_data <- training(data_split)
test_data <- testing(data_split)
# Separate the target variable 'y' from the features in the training and testing datasets
X_train <- select(train_data, -OUTCOME)
X_test <- select(test_data, -OUTCOME)
y_train <- train_data$OUTCOME
y_test <- test_data$OUTCOME
print(y_train)
# Convert the response variable to a factor
y_train <- as.factor(y_train)
y_test <- as.factor(y_test)
# Combine X_train and y_train into a single data frame
train_data <- bind_cols(X_train, y_train)
train_data <- train_data %>%
  rename(OUTCOME = `...19`)
glimpse(train_data)
# Count the frequency of each class in the outcome variable in training_data
class_counts <- train_data %>%
  count(OUTCOME)
glimpse(class_counts)
# Determine the minority class
minority_class <- class_counts %>%
  filter(n == min(n)) %>%
  pull(OUTCOME)
# Split the data into minority and majority classes
minority_data <- train_data %>%
  filter(OUTCOME == minority_class)
majority_data <- train_data %>%
  filter(OUTCOME != minority_class)
# Upsample the minority class
upsampled_data <- minority_data %>%
  sample_n(size = nrow(majority_data), replace = TRUE) %>%
  bind_rows(majority_data)
glimpse(upsampled_data)
X_train_balanced <- select(upsampled_data, -OUTCOME)
y_train_balanced <- upsampled_data$OUTCOME
str(y_train_balanced)
y_train_balanced
head(y_train_balanced,20)
#compare the amount of data before and after upsampling
a <- table(y_train)
b <- table(y_train_balanced)
print(a)
print(b)
glimpse(test_data)


# Upsampling the testing data
test_data <- bind_cols(X_test, y_test)
test_data <- test_data %>%
  rename(OUTCOME = `...19`)
glimpse(test_data)
# Count the frequency of each class in the outcome variable
class_counts <- test_data %>%
  count(OUTCOME)
glimpse(class_counts)
# Determine the minority class
minority_class <- class_counts %>%
  filter(n == min(n)) %>%
  pull(OUTCOME)
# Split the data into minority and majority classes
minority_data <- test_data %>%
  filter(OUTCOME == minority_class)
majority_data <- test_data %>%
  filter(OUTCOME != minority_class)
# Upsample the minority class
upsampled_test_data <- minority_data %>%
  sample_n(size = nrow(majority_data), replace = TRUE) %>%
  bind_rows(majority_data)

glimpse(upsampled_test_data)

X_test_balanced <- upsampled_test_data %>% dplyr::select(-OUTCOME)
y_test_balanced <- upsampled_test_data$OUTCOME
str(y_test_balanced)
class_counts <- upsampled_test_data %>%
  count(OUTCOME)
glimpse(class_counts)
#compare the amount of data before and after upsampling
a <- table(y_test)
b <- table(y_test_balanced)

print(a)
print(b)

#####################################################################################
###LOGISTIC REGRESSION#####
# Train a logistic regression model using the balanced training data

library(yardstick)

model <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(y_train_balanced ~ ., data = X_train_balanced)

# Make predictions on the balanced test data
predictions <- predict(model, X_test_balanced)
y_test_balanced <- factor(y_test_balanced, levels = levels(predictions$.pred_class))

# Calculate accuracy
accuracy <- mean(predictions$.pred_class == y_test_balanced)
print(paste("Accuracy:", accuracy))

count<-table(predictions$.pred_class)
print(count)

conf_matrix <- table(y_test_balanced, predictions$.pred_class)
print(conf_matrix)

conf_matrix_data <- as.data.frame.table(conf_matrix)
colnames(conf_matrix_data) <-  c("True_Class", "Predicted_Class", "Count")
print(conf_matrix_data)
conf_matrix_data$Percentage <- conf_matrix_data$Count / sum(conf_matrix_data$Count) * 100

ggplot(conf_matrix_data, aes(x = True_Class, y = Predicted_Class)) +
  geom_tile(aes(fill = Percentage), color = "white") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix Heatmap",
       x = "True Class",
       y = "Predicted Class",
       fill = "Percentage") +
  theme_minimal()
#########################
#######SVM################

# Create a linear SVM model specification
svm_spec <- svm_linear() %>%
  set_engine("LiblineaR") %>%
  set_mode("classification")
install.packages("LiblineaR")

# Train the SVM model
SVMmodel <- fit(svm_spec, y_train_balanced ~ ., data = X_train_balanced)

# Make predictions on the balanced test data
predictions <- predict(SVMmodel, X_test_balanced)

# Convert predictions to factor with levels from the predictions
y_test_balanced <- factor(y_test_balanced, levels = levels(predictions$.pred_class))

# Calculate accuracy
accuracy <- mean(predictions$.pred_class == y_test_balanced)
print(paste("Accuracy:", accuracy))
count<-table(predictions$.pred_class)
print(count)

conf_matrix <- table(y_test_balanced, predictions$.pred_class)
print(conf_matrix)

conf_matrix_data <- as.data.frame.table(conf_matrix)
colnames(conf_matrix_data) <- c("True_Class", "Predicted_Class", "Count")
print(conf_matrix_data)

conf_matrix_data$Percentage <- conf_matrix_data$Count / sum(conf_matrix_data$Count) * 100

ggplot(conf_matrix_data, aes(x = True_Class, y = Predicted_Class)) +
  geom_tile(aes(fill = Percentage), color = "white") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix Heatmap",
       x = "True Class",
       y = "Predicted Class",
       fill = "Percentage") +
  theme_minimal() 

#######################################
####random forecast################
# Define the Random Forest model specification
rf_spec <- rand_forest(trees = 100) %>%
  set_engine("ranger") %>%
  set_mode("classification")

install.packages("ranger")

# Train the Random Forest model
RFmodel <- fit(rf_spec, y_train_balanced ~ ., data = X_train_balanced)

# Make predictions on the balanced test data
predictions <- predict(RFmodel, X_test_balanced)

# Convert predictions to factor with levels from the predictions
y_test_balanced <- factor(y_test_balanced, levels = levels(predictions$.pred_class))

# Calculate accuracy
accuracy <- mean(predictions$.pred_class == y_test_balanced)
print(paste("Accuracy:", accuracy))

count<-table(predictions$.pred_class)
print(count)

conf_matrix <- table(y_test_balanced, predictions$.pred_class)
print(conf_matrix)

conf_matrix_data <- as.data.frame.table(conf_matrix)
colnames(conf_matrix_data) <- c("True_Class", "Predicted_Class", "Count")
print(conf_matrix_data)

conf_matrix_data$Percentage <- conf_matrix_data$Count / sum(conf_matrix_data$Count) * 100

ggplot(conf_matrix_data, aes(x = True_Class, y = Predicted_Class)) +
  geom_tile(aes(fill = Percentage), color = "white") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix Heatmap",
       x = "True Class",
       y = "Predicted Class",
       fill = "Percentage") +
  theme_minimal() 

#######################################
####xgboost#################
xgb_spec <- boost_tree(trees = 100) %>%
  set_engine("xgboost") %>%
  set_mode("classification")
install.packages("xgboost")

# Train the XGBoost model
XGBmodel <- fit(xgb_spec, y_train_balanced ~ ., data = X_train_balanced)
# Make predictions on the balanced test data
predictions <- predict(XGBmodel, X_test_balanced)
# Convert predictions to factor with levels from the predictions
y_test_balanced <- factor(y_test_balanced, levels = levels(predictions$.pred_class))
# Calculate accuracy
accuracy <- mean(predictions$.pred_class == y_test_balanced)
print(paste("Accuracy:", accuracy))

count<-table(predictions$.pred_class)
print(count)
conf_matrix <- table(y_test_balanced, predictions$.pred_class)
print(conf_matrix)
conf_matrix_data <- as.data.frame.table(conf_matrix)
colnames(conf_matrix_data) <- c("True_Class", "Predicted_Class", "Count")
print(conf_matrix_data)
conf_matrix_data$Percentage <- conf_matrix_data$Count / sum(conf_matrix_data$Count) * 100
ggplot(conf_matrix_data, aes(x = True_Class, y = Predicted_Class)) +
  geom_tile(aes(fill = Percentage), color = "white") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix Heatmap",
       x = "True Class",
       y = "Predicted Class",
       fill = "Percentage") +
  theme_minimal() 

################Feature selection###############
library(tidymodels)
upsampled_data$OUTCOME<- as.factor(upsampled_data$OUTCOME)
upsampled_data <- upsampled_data %>%
  select(-ID)
glimpse(upsampled_data)
# Create a Random Forest model specification using the ranger engine
rf_spec <- rand_forest(trees = 500, mtry = 3, min_n = 20) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("classification")

# Create a recipe to preprocess the data (same as before)
rf_recipe <- recipe(OUTCOME ~ ., data = upsampled_data) %>%
  step_nzv(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes())

# Combine the recipe and model specification into a workflow
rf_wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(rf_recipe)

# Fit the Random Forest model to the data
rf_fit <- rf_wf %>% fit(upsampled_data)
# Extract and visualize feature importance scores using vip
install.packages("vip")
library(vip)
importance_scores <- rf_fit %>%
  extract_fit_parsnip() %>%
  vip()
# Rank features based on importance scores
ranked_features <- importance_scores$data %>%
  dplyr::arrange(desc(Importance))
print(ranked_features)
ggplot(ranked_features, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Variable", y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Select the top n_features_to_select features based on importance scores
selected_features <- importance_scores$data %>%
  dplyr::top_n(12, Importance) %>%
  pull(Variable)
print(selected_features)
# Print the selected features
cat("Selected Features based on Feature Importance:\n")
cat(selected_features, sep = "\n")
str(upsampled_data)
upsampled_data_selected <-upsampled_data[selected_features]
upsampled_data_selected<- cbind(upsampled_data_selected, OUTCOME  = upsampled_data$OUTCOME)
X_train_selected <- X_train_balanced[selected_features]
y_train_selected <- upsampled_data_selected['OUTCOME']
print(upsampled_data_selected)
X_test_selected <- (X_test[selected_features])
str(upsampled_data_selected)
##############################################################
###############################################################################
#####K FOLD CRoss Validation######
##logistics regression####
# Load necessary libraries
library(tidymodels)

# Step 1: Prepare the data (already split into training and testing sets)
# X_train, y_train: Training data and labels
# X_test, y_test: Testing data and labels

# Step 2: Choose the value of K for cross-validation
K <- 5
# Step 3: Create a logistic regression model specification
logreg_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")
# Step 4: Create a recipe (preprocessing pipeline)
logreg_recipe <- recipe(OUTCOME ~ ., data = upsampled_data_selected) %>%
  step_zv(all_predictors()) %>%  # Remove columns with zero variance
  step_scale(all_predictors()) %>%
  step_center(all_predictors())
# Step 5: Combine the model specification and recipe into a workflow
logreg_wf <- workflow() %>%
  add_model(logreg_spec) %>%
  add_recipe(logreg_recipe)
# Step 6: Cross-validation and performance metrics calculation
cv_folds <- vfold_cv(upsampled_data_selected, v = K, strata = OUTCOME)
# Fit the model to each fold using fit_resamples()
print(cv_results)
cv_results <- fit_resamples(logreg_wf, cv_folds)

performance_metrics <- cv_results %>%
  collect_metrics()
print(performance_metrics)

# Filter the rows with metric 'accuracy' and estimator 'binary'
accuracy_metric <- filter(performance_metrics, .metric == "accuracy" & .estimator == "binary")
# Extract the mean accuracy value
mean_accuracy <- pull(accuracy_metric, mean)
std <-pull(accuracy_metric, std_err)
# Print the mean accuracy value
print(mean_accuracy)

# Step 9: Final evaluation on the testing dataset
final_fit <- fit(logreg_wf, data = upsampled_data_selected)  # Train the final model on the entire training dataset
final_predicted <- predict(final_fit, new_data = X_test)  # Get predicted values on the testing dataset
# Convert y_test_balanced to numeric or logical
str(final_predicted_numeric)
y_test_numeric <- as.numeric(y_test) -1
# Convert predicted values to 0-1 encoding
final_predicted_numeric <- as.numeric(as.character(final_predicted$.pred_class))
# Calculate the final accuracy
correct_predictions <- sum(y_test_numeric == final_predicted_numeric)
total_predictions <- length(y_test_numeric)
final_accuracy <- correct_predictions / total_predictions

print(sprintf("Average cross-validation accuracy: %f",mean_accuracy))
print(sprintf("Standard deviation of cross-validation accuracy: %f",std ))
print(sprintf("Final model accuracy on the testing dataset: %f", final_accuracy))
##################################
####ROC Performace Evaluation####
# Step 1: Extract the predicted probabilities for the positive class
# Get predicted probabilities for the positive class from the final model
predicted_prob_pos <- predict(final_fit, new_data = X_test, type = "prob")

print(predicted_prob_pos)
predicted_prob_pos <- predicted_prob_pos$.pred_1
# Step 2: Calculate the ROC curve
library(pROC)
roc_curve <- roc(y_test_numeric, predicted_prob_pos)
print(roc_curve)
# Step 3: Calculate the AUC-ROC
auc_roc <- auc(roc_curve)
# Step 4: Plot the ROC curve (optional)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
legend("bottomright", paste("AUC =", round(auc_roc, 2)), col = "blue", lwd = 2)

# Step 5: Print the AUC-ROC
print(paste("AUC-ROC:", auc_roc))



pr_curve <- roc(y_test_numeric, final_predicted_numeric, levels = c(0, 1))
# Plot the precision-recall curve
plot(pr_curve, main = "Precision-Recall Curve", col = "blue", lwd = 2)
# Add the area under the curve (AUC) to the plot
legend("bottomright", paste("AUC =", round(auc(pr_curve), 2)), col = "blue", lwd = 2)
###confusion###
glimpse(final_predicted)
# Combine 'y_test' and 'y_pred' into a single data frame
# Create the confusion matrix

conf_matrix <- table(y_test, final_predicted$.pred_class)
print(conf_matrix)
conf_matrix_data <- as.data.frame.table(conf_matrix)
colnames(conf_matrix_data) <- c("True_Class", "Predicted_Class", "Count")
print(conf_matrix_data)

conf_matrix_data$Percentage <- conf_matrix_data$Count / sum(conf_matrix_data$Count) * 100

ggplot(conf_matrix_data, aes(x = True_Class, y = Predicted_Class)) +
  geom_tile(aes(fill = Percentage), color = "white") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix Heatmap",
       x = "True Class",
       y = "Predicted Class",
       fill = "Percentage") +
  theme_minimal()

##################### K FOLD SVM###################################
# Step 1: Prepare the data (already split into training and testing sets)
# X_train, y_train: Training data and labels
# X_test, y_test: Testing data and labels

# Step 2: Choose the value of K for cross-validation
K <- 5

install.packages("kernlab")
library(kernlab)

svm_spec <- svm_linear() %>%
  set_engine("kernlab") %>%
  set_mode("classification")  # Set the mode to classification

# Step 2: Create a Workflow
svm_wf <- workflow() %>%
  add_model(svm_spec) %>%
  add_formula(OUTCOME ~ .)  
# Step 6: Cross-validation and performance metrics calculation
cv_folds <- vfold_cv(upsampled_data_selected, v = K, strata = OUTCOME)

# Fit the model to each fold using fit_resamples()
print(cv_folds)
cv_results <- fit_resamples(logreg_wf, cv_folds)

performance_metrics <- cv_results %>%
  collect_metrics()
print(performance_metrics)

# Filter the rows with metric 'accuracy' and estimator 'binary'
accuracy_metric <- filter(performance_metrics, .metric == "accuracy" & .estimator == "binary")
# Extract the mean accuracy value
mean_accuracy <- pull(accuracy_metric, mean)
std <-pull(accuracy_metric, std_err)
# Step 9: Final evaluation on the testing dataset
final_fit <- fit(svm_wf, data = upsampled_data_selected)  # Train the final model on the entire training dataset
final_predicted <- predict(final_fit, new_data = X_test)  # Get predicted values on the testing dataset
# Convert y_test_balanced to numeric or logical
str(final_predicted_numeric)
y_test_numeric <- as.numeric(y_test) -1
# Convert predicted values to 0-1 encoding
final_predicted_numeric <- as.numeric(as.character(final_predicted$.pred_class))
# Calculate the final accuracy
correct_predictions <- sum(y_test_numeric == final_predicted_numeric)
total_predictions <- length(y_test_numeric)
final_accuracy <- correct_predictions / total_predictions
print(sprintf("Average cross-validation accuracy: %f",mean_accuracy))
print(sprintf("Standard deviation of cross-validation accuracy: %f",std ))
print(sprintf("Final model accuracy on the testing dataset: %f", final_accuracy))

####ROC Performace Evaluation####
# Step 1: Extract the predicted probabilities for the positive class
# Get predicted probabilities for the positive class from the final model
predicted_prob_pos <- predict(final_fit, new_data = X_test, type = "prob")

print(predicted_prob_pos)
predicted_prob_pos <- predicted_prob_pos$.pred_1

# Step 2: Calculate the ROC curve
library(pROC)
roc_curve <- roc(y_test_numeric, predicted_prob_pos)
print(roc_curve)
# Step 3: Calculate the AUC-ROC
auc_roc <- auc(roc_curve)

# Step 4: Plot the ROC curve (optional)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
legend("bottomright", paste("AUC =", round(auc_roc, 2)), col = "blue", lwd = 2)


# Step 5: Print the AUC-ROC
print(paste("AUC-ROC:", auc_roc))
pr_curve <- roc(y_test_numeric, final_predicted_numeric, levels = c(0, 1))
# Plot the precision-recall curve
plot(pr_curve, main = "Precision-Recall Curve", col = "blue", lwd = 2)

# Add the area under the curve (AUC) to the plot
legend("bottomright", paste("AUC =", round(auc(pr_curve), 2)), col = "blue", lwd = 2)

###confusion###
glimpse(final_predicted)
# Combine 'y_test' and 'y_pred' into a single data frame

# Create the confusion matrix

conf_matrix <- table(y_test, final_predicted$.pred_class)
print(conf_matrix)

conf_matrix_data <- as.data.frame.table(conf_matrix)
colnames(conf_matrix_data) <- c("True_Class", "Predicted_Class", "Count")
print(conf_matrix_data)
conf_matrix_data$Percentage <- conf_matrix_data$Count / sum(conf_matrix_data$Count) * 100
ggplot(conf_matrix_data, aes(x = True_Class, y = Predicted_Class)) +
  geom_tile(aes(fill = Percentage), color = "white") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix Heatmap",
       x = "True Class",
       y = "Predicted Class",
       fill = "Percentage") +
  theme_minimal()
##############################################################
#################RANDOM FORECAST##########################
# Step 2: Choose the value of K for cross-validation
K <- 5

# Step 3: Create a logistic regression model specification
rf_spec <- rand_forest(trees = 100) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# Step 4: Create a recipe (preprocessing pipeline)
rf_recipe <- recipe(OUTCOME ~ ., data = upsampled_data_selected) %>%
  step_zv(all_predictors()) %>%  # Remove columns with zero variance
  step_scale(all_predictors()) %>%
  step_center(all_predictors())


# Step 5: Combine the model specification and recipe into a workflow
rf_wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(rf_recipe)

# Step 6: Cross-validation and performance metrics calculation
cv_folds <- vfold_cv(upsampled_data_selected, v = K, strata = OUTCOME)

# Fit the model to each fold using fit_resamples()
print(cv_results)
cv_results <- fit_resamples(rf_wf, cv_folds)

performance_metrics <- cv_results %>%
  collect_metrics()
print(performance_metrics)

# Filter the rows with metric 'accuracy' and estimator 'binary'
accuracy_metric <- filter(performance_metrics, .metric == "accuracy" & .estimator == "binary")

# Extract the mean accuracy value
mean_accuracy <- pull(accuracy_metric, mean)
std <-pull(accuracy_metric, std_err)
# Step 9: Final evaluation on the testing dataset
final_fit <- fit(rf_wf, data = upsampled_data_selected)  # Train the final model on the entire training dataset
final_predicted <- predict(final_fit, new_data = X_test)  # Get predicted values on the testing dataset
print(final_predicted)
# Convert y_test_balanced to numeric or logical
str(final_predicted_numeric)
y_test_numeric <- as.numeric(y_test)-1

final_predicted_numeric <- as.numeric(as.character(final_predicted$.pred_class))

# Calculate the final accuracy
correct_predictions <- sum(y_test_numeric == final_predicted_numeric)
total_predictions <- length(y_test_numeric)
final_accuracy <- correct_predictions / total_predictions

print(correct_predictions)
print(sprintf("Average cross-validation accuracy: %f",mean_accuracy))

print(sprintf("Standard deviation of cross-validation accuracy: %f",std ))
print(sprintf("Final model accuracy on the testing dataset: %f", final_accuracy))

####ROC Performace Evaluation####
# Step 1: Extract the predicted probabilities for the positive class
# Get predicted probabilities for the positive class from the final model
predicted_prob_pos <- predict(final_fit, new_data = X_test, type = "prob")

print(predicted_prob_pos)
predicted_prob_pos <- predicted_prob_pos$.pred_1

# Step 2: Calculate the ROC curve
library(pROC)
roc_curve <- roc(y_test_numeric, predicted_prob_pos)
print(roc_curve)
# Step 3: Calculate the AUC-ROC
auc_roc <- auc(roc_curve)

# Step 4: Plot the ROC curve (optional)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
legend("bottomright", paste("AUC =", round(auc_roc, 2)), col = "blue", lwd = 2)


# Step 5: Print the AUC-ROC
print(paste("AUC-ROC:", auc_roc))
pr_curve <- roc(y_test_numeric, final_predicted_numeric, levels = c(0, 1))
# Plot the precision-recall curve
plot(pr_curve, main = "Precision-Recall Curve", col = "blue", lwd = 2)

# Add the area under the curve (AUC) to the plot
legend("bottomright", paste("AUC =", round(auc(pr_curve), 2)), col = "blue", lwd = 2)

###confusion###
glimpse(final_predicted)
# Combine 'y_test' and 'y_pred' into a single data frame

# Create the confusion matrix

conf_matrix <- table(y_test, final_predicted$.pred_class)
print(conf_matrix)

conf_matrix_data <- as.data.frame.table(conf_matrix)
colnames(conf_matrix_data) <- c("True_Class", "Predicted_Class", "Count")
print(conf_matrix_data)
conf_matrix_data$Percentage <- conf_matrix_data$Count / sum(conf_matrix_data$Count) * 100
ggplot(conf_matrix_data, aes(x = True_Class, y = Predicted_Class)) +
  geom_tile(aes(fill = Percentage), color = "white") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix Heatmap",
       x = "True Class",
       y = "Predicted Class",
       fill = "Percentage") +
  theme_minimal()
###########################################################################
################XG-BOOST##########################
# Step 2: Choose the value of K for cross-validation
K <- 5
library(xgboost)
# Step 3: Create a logistic regression model specification
xgb_spec <- boost_tree(trees = 100) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Step 4: Create a recipe (preprocessing pipeline)
xgb_recipe <- recipe(OUTCOME ~ ., data = upsampled_data_selected) %>%
  step_zv(all_predictors()) %>%  # Remove columns with zero variance
  step_scale(all_predictors()) %>%
  step_center(all_predictors())


# Step 5: Combine the model specification and recipe into a workflow
xgb_wf <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(xgb_recipe)
# Step 6: Cross-validation and performance metrics calculation
cv_folds <- vfold_cv(upsampled_data_selected, v = K, strata = OUTCOME)
# Fit the model to each fold using fit_resamples()
print(cv_results)
cv_results <- fit_resamples(xgb_wf, cv_folds)

performance_metrics <- cv_results %>%
  collect_metrics()
print(performance_metrics)

# Filter the rows with metric 'accuracy' and estimator 'binary'
accuracy_metric <- filter(performance_metrics, .metric == "accuracy" & .estimator == "binary")

# Extract the mean accuracy value
mean_accuracy <- pull(accuracy_metric, mean)
std <-pull(accuracy_metric, std_err)

# Step 9: Final evaluation on the testing dataset
final_fit <- fit(xgb_wf, data = upsampled_data_selected)  # Train the final model on the entire training dataset
final_predicted <- predict(final_fit, new_data = X_test)  # Get predicted values on the testing dataset
print(final_predicted)
# Convert y_test_balanced to numeric or logical
str(final_predicted_numeric)
y_test_numeric <- as.numeric(y_test)-1

final_predicted_numeric <- as.numeric(as.character(final_predicted$.pred_class))

# Calculate the final accuracy
correct_predictions <- sum(y_test_numeric == final_predicted_numeric)
total_predictions <- length(y_test_numeric)
final_accuracy <- correct_predictions / total_predictions

print(correct_predictions)
print(sprintf("Average cross-validation accuracy: %f",mean_accuracy))

print(sprintf("Standard deviation of cross-validation accuracy: %f",std ))

print(sprintf("Final model accuracy on the testing dataset: %f", final_accuracy))

####ROC Performace Evaluation####
# Step 1: Extract the predicted probabilities for the positive class
# Get predicted probabilities for the positive class from the final model

predicted_prob_pos <- predict(final_fit, new_data = X_test, type = "prob")

print(predicted_prob_pos)
predicted_prob_pos <- predicted_prob_pos$.pred_1

# Step 2: Calculate the ROC curve
library(pROC)
roc_curve <- roc(y_test_numeric, predicted_prob_pos)
print(roc_curve)
#Area under the curve (AUC): The AUC value is shown as 0.5735.
# Step 3: Calculate the AUC-ROC
auc_roc <- auc(roc_curve)

# Step 4: Plot the ROC curve (optional)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
legend("bottomright", paste("AUC =", round(auc_roc, 2)), col = "blue", lwd = 2)

# Step 5: Print the AUC-ROC
print(paste("AUC-ROC:", auc_roc))
pr_curve <- roc(y_test_numeric, final_predicted_numeric, levels = c(0, 1))
# Plot the precision-recall curve
plot(pr_curve, main = "Precision-Recall Curve", col = "blue", lwd = 2)
# Add the area under the curve (AUC) to the plot
legend("bottomright", paste("AUC =", round(auc(pr_curve), 2)), col = "blue", lwd = 2)

###confusion###
glimpse(final_predicted)
# Combine 'y_test' and 'y_pred' into a single data frame

# Create the confusion matrix

conf_matrix <- table(y_test, final_predicted$.pred_class)
print(conf_matrix)

conf_matrix_data <- as.data.frame.table(conf_matrix)
colnames(conf_matrix_data) <- c("True_Class", "Predicted_Class", "Count")
print(conf_matrix_data)
conf_matrix_data$Percentage <- conf_matrix_data$Count / sum(conf_matrix_data$Count) * 100
ggplot(conf_matrix_data, aes(x = True_Class, y = Predicted_Class)) +
  geom_tile(aes(fill = Percentage), color = "white") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix Heatmap",
       x = "True Class",
       y = "Predicted Class",
       fill = "Percentage") +
  theme_minimal()


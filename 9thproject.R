# -------------------------------------------------
# Credit Risk Modeling: Logistic Regression in R
# -------------------------------------------------
# Install necessary packages (run only once)
install.packages(c("dplyr", "ggplot2", "caret", "pROC"))
# Load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(pROC)

# -------------------------------------------------
# 1. Load Data (sample data of 20 records)
# -------------------------------------------------

# You can save this data into a CSV named "credit_data.csv"
# Or use it directly like this:
credit_data <- data.frame(
  age = c(25,34,45,23,36,52,30,40,29,50,27,33,38,24,44,31,39,26,42,28),
  income = c(35000,54000,72000,28000,61000,83000,47000,69000,39000,77000,
             36000,52000,60000,31000,71000,48000,64000,34000,75000,37000),
  loan_amount = c(5000,12000,10000,3000,15000,25000,6000,11000,4000,23000,
                  5000,9000,10000,3500,12000,7000,9000,4500,20000,6000),
  default = c(0,0,0,1,0,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1)
)

# -------------------------------------------------
# 2. Data Cleaning and Exploration
# -------------------------------------------------

# Check for missing values
colSums(is.na(credit_data))

# Summary statistics
summary(credit_data)

# Visualization: Income vs Default
ggplot(credit_data, aes(x = income, fill = as.factor(default))) +
  geom_histogram(position = "dodge", bins = 10) +
  labs(title = "Income Distribution by Default Status", fill = "Default")

# -------------------------------------------------
# 3. Split into Training and Test Sets
# -------------------------------------------------

set.seed(123)  # for reproducibility
index <- createDataPartition(credit_data$default, p = 0.7, list = FALSE)
train <- credit_data[index, ]
test <- credit_data[-index, ]

# -------------------------------------------------
# 4. Build Logistic Regression Model
# -------------------------------------------------

model <- glm(default ~ age + income + loan_amount, data = train, family = binomial)
summary(model)

# -------------------------------------------------
# 5. Make Predictions
# -------------------------------------------------

# Get predicted probabilities
probs <- predict(model, newdata = test, type = "response")

# Classify based on 0.5 threshold
predicted_class <- ifelse(probs > 0.5, 1, 0)

# -------------------------------------------------
# 6. Evaluate Model
# -------------------------------------------------

# Confusion matrix
confusionMatrix(as.factor(predicted_class), as.factor(test$default))

# ROC curve and AUC
roc_curve <- roc(test$default, probs)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc(roc_curve)

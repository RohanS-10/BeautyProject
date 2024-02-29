# Load necessary libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(lmtest)
library(caret)
library(olsrr)  # for diagnostic plots

# Load the dataset
beauty_data <- read.csv("beauty.csv", header = TRUE)

# Data Preprocessing
# Convert categorical variables to factors
beauty_data <- beauty_data %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character, as.factor)

# Handle missing values (replace with mean for simplicity)
beauty_data[is.na(beauty_data)] <- colMeans(beauty_data, na.rm = TRUE)

# Advanced Data Visualization
# Scatter plot matrix
scatterplot_matrix <- pairs(beauty_data[, c("wage", "exper", "educ", "female")],
                            main = "Scatterplot Matrix")

# Correlation matrix heatmap
cor_matrix <- cor(beauty_data[, c("wage", "exper", "educ", "female")])
corrplot(cor_matrix, method = "color")

# Box plot for wage by education level
boxplotedu <- ggplot(beauty_data, aes(x = educ, y = wage, fill = educ)) +
  geom_boxplot() +
  ggtitle("Box Plot of Wage by Education Level") +
  theme_minimal()

# Statistical Analysis
# t-test for wage difference between genders
t_test_result <- t.test(wage ~ female, data = beauty_data)
print(t_test_result)

# Multiple linear regression model with interaction effect
lm_model <- lm(wage ~ exper + educ + female + married + service + female:educ, data = beauty_data)
summary(lm_model)

# Heteroskedasticity test
hetero_test <- bptest(lm_model)
print(hetero_test)

# Predictive Modeling
# Split the data into training and testing sets
set.seed(123)
split_index <- createDataPartition(beauty_data$wage, p = 0.7, list = FALSE)
train_data <- beauty_data[split_index, ]
test_data <- beauty_data[-split_index, ]

# Train a linear regression model
lm_model <- lm(wage ~ ., data = train_data)
summary(lm_model)

# Evaluate the model on the test set
predictions <- predict(lm_model, newdata = test_data)
mse <- mean((test_data$wage - predictions)^2)
print(paste("Mean Squared Error:", mse))

# Diagnostic plots
# Residuals vs Fitted Values
residualsplot = plot(lm_model, which = 1)

# Normal Q-Q Plot
qqplot = plot(lm_model, which = 2)

# Scale-Location Plot (Square root of standardized residuals vs Fitted Values)
scalelocation = plot(lm_model, which = 3)

# Cook's Distance Plot
cooksplot = plot(lm_model, which = 4)

#Save all plots

project_folder <- "/Users/rohan/Desktop/BeautyData"
ggsave(file.path(project_folder, "cooks_distance_plot.png"), plot = cooksplot)
ggsave(file.path(project_folder, "scale_location_plot.png"), plot = scalelocation)
ggsave(file.path(project_folder, "normal_qq_plot.png"), plot = qqplot)
ggsave(file.path(project_folder, "residual_fitted_plot.png"), plot = residualsplot)
ggsave(file.path(project_folder, "boxplot_edu_plot.png"), plot = boxplotedu)
ggsave(file.path(project_folder, "corr_matrix_plot.png"), plot = cor_matrix)
ggsave(file.path(project_folder, "scat_matrix_plot.png"), plot = scatterplot_matrix)


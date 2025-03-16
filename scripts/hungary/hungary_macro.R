# Complete Analysis Script for Militia Support in Hungary
# This script processes survey data, fits endorsement models, and analyzes marginal effects
# for understanding factors that influence militia support

# Load required packages
library(endorse)
library(haven)
library(ggpubr)
library(missForest)
library(forcats)
library(reshape2)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(Cairo)

#====================================================
# 1. Data Loading and Initial Processing for Hungary
#====================================================

HUData <- read_sav("~/projects/AaD_Research/datasets/scrubbed_datasets/hungary_scrubbed.sav")

# Define questions for the endorsement experiment
data_hu_questions <- c("id", "A10A_1_control_reversed", "A10A_2_control_reversed", "A10A_3_control_reversed",
                       "A10B_1_experiment_reversed", "A10B_2_experiment_reversed", "A10B_3_experiment_reversed"
)
# Select relevant columns for endorsement analysis
data_hu_questions <- HUData[data_hu_questions]

# Define variables to keep
vars <- c("id", "Male", "Age", "Education", "Capital", "IdeologyLR", "FamIncome",  "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "EconGrievanceProspMostFams",
          "GayNeighbor", "GayFamily", "GayLesRights", "ForNeighbor", "ForPartner", "Ukraine",
          "NativeJobs", "NativeRights", "DemonstrateNational", "Religiosity", "VoteFarRight"
)

# Subset and recode variables
data_hu_vars <- HUData[vars]

# Convert all variables to numeric
data_hu_vars <- mutate(data_hu_vars, across(everything(), ~as.numeric(.)))

# Merge the questions and standardized variables datasets
data_hu <- left_join(data_hu_questions, data_hu_vars, by = "id")

# Create named list for response questions
Y <- list(Q1 = c("A10A_1_control_reversed", "A10B_1_experiment_reversed"), 
          Q2 = c("A10A_2_control_reversed", "A10B_2_experiment_reversed"), 
          Q3 = c("A10A_3_control_reversed", "A10B_3_experiment_reversed")
)

#====================================================
# 2. Creating the endorse object
#====================================================

# Creating an endorse object, excluding all covariates that are in the set { traditionalism }

endorse_object <- endorse(Y = Y, 
                          data = data_hu,
                          identical.lambda = FALSE,
                          covariates = FALSE,
                          prop = 0.008,
                          omega2.out = TRUE,
                          hierarchical = FALSE
)


# Extract and standardize MCMC samples
# --------------------------------------
# Extract MCMC samples for lambda and sigma²
lambda_samples <- as.matrix(endorse_object$lambda)
sigma2 <- (endorse_object$x)^2  # Convert standard deviation to variance

# Standardize lambdas for each question
lambda_std <- list(
  q1 = lambda_samples[, "(Intercept).1.1"] / sigma2,
  q2 = lambda_samples[, "(Intercept).2.1"] / sigma2,
  q3 = lambda_samples[, "(Intercept).3.1"] / sigma2
)

# Calculate summary statistics
# --------------------------------------
# Calculate mean and confidence intervals for each question
question_stats <- lapply(lambda_std, function(x) {
  list(
    mean = mean(x),
    ci = quantile(x, probs = c(0.025, 0.975))
  )
})

# Print results
# --------------------------------------
for (i in 1:3) {
  q_name <- paste0("q", i)
  cat(sprintf("Standardized Support for the Militia (Question %d):\n", i))
  cat(sprintf("Mean: %.3f\n", question_stats[[q_name]]$mean))
  cat(sprintf("95%% CI: [%.3f, %.3f]\n\n", 
              question_stats[[q_name]]$ci[1], 
              question_stats[[q_name]]$ci[2]))
}

# Create visualizations
# --------------------------------------
library(ggplot2)
library(reshape2)

# Density plot
# --------------------------------------
# Combine into a data frame for density plot
posterior_df <- data.frame(
  Question = rep(paste0("Q", 1:3), each = length(lambda_std$q1)),
  Support = c(lambda_std$q1, lambda_std$q2, lambda_std$q3)
)

# Create density plot
density_plot <- ggplot(posterior_df, aes(x = Support, fill = Question)) +
  geom_density(alpha = 0.5) +
  labs(title = "Hungary: Standardized Support by Question",
       x = "Support Level (λ / σ²)", 
       y = "Density") +
  theme_minimal()

print(density_plot)

# Box plot
# --------------------------------------
# Create a data frame with explicit column names
box_data <- data.frame(
  "Question A" = lambda_std$q1,
  "Question B" = lambda_std$q2,
  "Question C" = lambda_std$q3
)

# Alternatively, create dataframe in long format directly
box_data_long <- data.frame(
  Question = factor(rep(c("Question A", "Question B", "Question C"), 
                        times = c(length(lambda_std$q1), length(lambda_std$q2), length(lambda_std$q3))),
                    levels = c("Question A", "Question B", "Question C")),
  Support = c(lambda_std$q1, lambda_std$q2, lambda_std$q3)
)

# Create box plot using the long format data
box_plot <- ggplot(box_data_long, aes(x = Question, y = Support, fill = Question)) +
  geom_boxplot(
    alpha = 0.8,
    outlier.shape = 16,
    outlier.size = 1,
    outlier.alpha = 0.3
  ) +
  labs(
    title = "Hungary: Standardized Support by Question",
    x = "Question",
    y = "Support Level (λ / σ²)"
  ) +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B", "#59A14F")) +  # Custom colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"  # Remove legend
  )

print(box_plot)
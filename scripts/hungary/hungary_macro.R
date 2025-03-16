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

# Calculate the row-wise average of the three lambda parameters
avg_lambda <- (lambda_samples[, "(Intercept).1.1"] + 
                 lambda_samples[, "(Intercept).2.1"] + 
                 lambda_samples[, "(Intercept).3.1"]) / 3

# Include this average in the lambda_std list
lambda_std <- list(
  q1 = lambda_samples[, "(Intercept).1.1"] / sigma2,
  q2 = lambda_samples[, "(Intercept).2.1"] / sigma2,
  q3 = lambda_samples[, "(Intercept).3.1"] / sigma2,
  average = avg_lambda / sigma2  # Standardize the average lambda just like the others
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

# Create visualizations
# --------------------------------------
library(ggplot2)
library(reshape2)
library(extrafont)  # For professional fonts
library(gridExtra)  # For adding table below plot

# Load additional fonts if available
# font_import()
# loadfonts(device = "win")  # Adjust for your OS

# Create dataframe in long format with average
box_data_long <- data.frame(
  Question = factor(rep(c("Question A", "Question B", "Question C", "Average"), 
                        times = c(length(lambda_std$q1), length(lambda_std$q2), 
                                  length(lambda_std$q3), length(lambda_std$average))),
                    levels = c("Question A", "Question B", "Question C", "Average")),
  Support = c(lambda_std$q1, lambda_std$q2, lambda_std$q3, lambda_std$average)
)

# Set a professional color palette suitable for publication
# Using a colorblind-friendly palette
journal_colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")

# Create publication-quality box plot
box_plot <- ggplot(box_data_long, aes(x = Question, y = Support, fill = Question)) +
  geom_boxplot(
    alpha = 0.85,
    outlier.shape = 21,
    outlier.size = 2,
    outlier.alpha = 0.7,
    outlier.color = "black",
    outlier.fill = "white",
    width = 0.6,
    lwd = 0.6
  ) +
  # Add horizontal reference line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  # Fixed y-axis limits as requested
  ylim(-1.5, 1.5) +
  # Labels
  labs(
    title = "Hungary: Standardized Support by Question",
    subtitle = "Standardized posterior distributions across survey questions",
    x = "",  # No x-axis label needed
    y = "Standardized Support (λ / σ²)",
    caption = "Note: Boxes represent interquartile range; whiskers extend to 1.5 x IQR"
  ) +
  scale_fill_manual(values = journal_colors) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0, size = 12, color = "gray30"),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11, face = "bold", margin = margin(t = 5)),
    panel.grid.major.y = element_line(color = "gray90", size = 0.3),
    panel.grid.minor.y = element_line(color = "gray95", size = 0.2),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, size = 0.5),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 25, b = 20, l = 25)
  )

# Create a table with summary statistics for display below the plot
stats_table <- data.frame(
  Question = c("Question A", "Question B", "Question C", "Average"),
  Mean = sapply(question_stats, function(x) round(x$mean, 3)),
  Lower_CI = sapply(question_stats, function(x) round(x$ci[1], 3)),
  Upper_CI = sapply(question_stats, function(x) round(x$ci[2], 3))
)

# Format table for display
table_theme <- ttheme_minimal(
  core = list(fg_params = list(hjust = 1, x = 0.9, fontface = "plain"),
              bg_params = list(fill = c("white", "gray95"))),
  colhead = list(fg_params = list(fontface = "bold", hjust = 1, x = 0.9),
                 bg_params = list(fill = "white")),
  rowhead = list(fg_params = list(hjust = 0, x = 0.1)))

# save the plot

ggsave(
  filename = "~/projects/AaD_Research/output/plots/hungary/dist/hungary_macro.pdf",
  plot = box_plot,
  device = Cairo::CairoPDF,
  width = 8,
  height = 6
)
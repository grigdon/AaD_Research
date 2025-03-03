# Complete Analysis Script for Militia Support in Slovakia
# This script processes survey data, fits endorsement models, and analyzes marginal effects
# for understanding factors that influence militia support

# Load required packages
library(endorse)
# library(tidyverse) 
library(haven)
library(ggpubr)
library(missForest)
library(forcats)
library(reshape2)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

#============================================
# 1. Data Loading and Initial Processing
#============================================

# Load the survey data
SKdata <- read_sav("/cloud/project/ReceivedScripts/Slovakia.sav")

# Define questions for the endorsement experiment
questions <- c("id", "q10a_control", "q10b_control", "q10c_control", 
               "q10a_experiment", "q10b_experiment", "q10c_experiment")

# Select relevant columns for endorsement analysis
data_slvk_questions <- SKdata[questions]

# Reverse code responses (1 ↔ 4, 2 ↔ 3)
data_slvk_questions <- data_slvk_questions %>%
  mutate(across(2:7, ~ recode(as.numeric(.), 
                              `1` = 4L, 
                              `2` = 3L, 
                              `3` = 2L, 
                              `4` = 1L, 
                              .default = NA_integer_)))

#============================================
# 2. Variable Preparation and Recoding
#============================================

# Prepare and relabel covariates
data_slvk_vars <- mutate(SKdata,
                         id = id,
                         gender = r1,
                         age = r2pov,
                         education = r4,
                         is_capital = recode(as.integer(r13),
                                             `1` = 1L,
                                             .default = 0L),
                         ideology = recode(as.integer(y1),
                                           `1` = 1L, `2` = 2L, `3` = 3L, 
                                           `4` = 4L, `5` = 5L, `9` = 99L),
                         income = recode(as.integer(r11b),
                                         `0` = 0L, `1` = 1L, `2` = 2L,
                                         `3` = 3L, `4` = 4L, `5` = 5L,
                                         `99` = 99L),
                         DemPolGrievence = q4a,
                         PolicyPolGrievence = q4b,
                         EconGrievenceRetro = q5a,
                         EconGrievenceProspInd = q5b,
                         EconGrievenceProspAgg = q5c,
                         NatPride = q7,
                         NativeRights = q9a,
                         NativeJobs = q9m,
                         LawOrder = q9e,
                         Chauvinism = q9d,
                         ChristianSchool = q9c,
                         GayNeighbor = q8c,
                         GayPartner = q8d,
                         ForNeighbor = q8e,
                         ForPartner = q8f,
                         Ukraine = q8g,
                         DemonstrateNational = q12b,
                         DemonstrateTrad = q12a
)

# Define variables to keep
vars <- c("id", "gender", "age", "education", "is_capital", "income", 
          "ideology", "DemPolGrievence", "PolicyPolGrievence",
          "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg",
          "NatPride", "NativeRights", "NativeJobs", "LawOrder", "Chauvinism",
          "ChristianSchool", "GayNeighbor", "GayPartner", "ForNeighbor",
          "ForPartner", "Ukraine", "DemonstrateNational", "DemonstrateTrad")

# Subset and recode variables
data_slvk_vars <- data_slvk_vars[vars]

# First, let's check what values we actually have in these columns
value_check <- sapply(data_slvk_vars[14:24], function(x) unique(x))
print("Unique values in columns 14-24:")
print(value_check)

# Recode specific variables with a default value for unspecified cases
data_slvk_vars <- mutate(data_slvk_vars, 
                         across(14:24, ~ recode(as.numeric(.),
                                                `1` = 4L,
                                                `2` = 3L,
                                                `3` = 2L,
                                                `4` = 1L,
                                                .default = NA_integer_)))

# Convert all variables to numeric
data_slvk_vars <- mutate(data_slvk_vars, across(everything(), ~as.numeric(.)))

# Check the number of columns before applying the missing value treatment
n_cols <- ncol(data_slvk_vars)
print(paste("Number of columns in data_slvk_vars:", n_cols))

# Handle missing values for all columns except ID (adjust range based on actual number of columns)
data_slvk_vars <- mutate(data_slvk_vars, 
                         across(2:n_cols, ~ifelse(. %in% c(99, 9), NA, .)))

#============================================
# 3. Missing Data Imputation
#============================================

# Impute missing values using Random Forest
data_slvk_imp <- missForest(as.data.frame(data_slvk_vars))
data_slvk_vars <- as_tibble(data_slvk_imp$ximp)

# First, let's see what columns we have
print("Column names in data_slvk_vars:")
print(names(data_slvk_vars))

# Get the number of columns (excluding the ID column which is column 1)
n_cols <- ncol(data_slvk_vars)

# Standardize all variables except the ID column
data_slvk_vars <- mutate(data_slvk_vars, across(2:n_cols, ~ as.vector(scale(.))))

# Verify the standardization worked
print("Summary of standardized variables:")
print(summary(data_slvk_vars))

#============================================
# 4. Endorsement Experiment Setup
#============================================

# Merge the questions and standardized variables datasets
data_slvk <- left_join(data_slvk_questions, data_slvk_vars, by = "id")

# Create named list for response questions
Y <- list(Q1 = c("q10a_control", "q10a_experiment"), 
          Q2 = c("q10b_control", "q10b_experiment"), 
          Q3 = c("q10c_control", "q10c_experiment"))

#============================================
# 5. Model Fitting
#============================================

# Fit the full endorsement model
endorseFULL1 <- endorse(Y = Y, 
                        data = data_slvk, 
                        identical.lambda = FALSE, 
                        covariates = TRUE, 
                        formula.indiv = formula(~ age + gender + education + 
                                                  is_capital + ideology + income + 
                                                  DemPolGrievence + PolicyPolGrievence + 
                                                  EconGrievenceRetro + EconGrievenceProspInd + 
                                                  EconGrievenceProspAgg + NatPride + 
                                                  NativeRights + NativeJobs + 
                                                  DemonstrateNational + LawOrder + 
                                                  Chauvinism + ChristianSchool + 
                                                  DemonstrateTrad + GayNeighbor + 
                                                  GayPartner + ForNeighbor + 
                                                  ForPartner + Ukraine), 
                        hierarchical = FALSE)

#============================================
# 6. Improved Marginal Effects Analysis
#============================================

# Completely rewritten calculate_marginal_effects function with clear parameter handling
calculate_marginal_effects <- function(model, covariate_name, values = c(-2, -1, 0, 1, 2)) {
  # Get model information
  item_names <- names(model$call$Y)
  num_items <- length(item_names)
  
  # Initialize results container
  all_results <- data.frame()
  
  # Find parameter columns for this covariate across all items
  lambda_cols <- colnames(model$lambda)
  
  # Loop through items
  for (item in 1:num_items) {
    # Find parameter name for this covariate in this item
    param_name <- paste0(covariate_name, ".", item, ".1")
    
    # Check if parameter exists
    if (!(param_name %in% lambda_cols)) {
      warning(paste("Parameter", param_name, "not found in model"))
      next
    }
    
    # Find intercept for this item
    intercept_name <- paste0("(Intercept).", item, ".1")
    if (!(intercept_name %in% lambda_cols)) {
      warning(paste("Intercept parameter", intercept_name, "not found in model"))
      next
    }
    
    # Get variance parameter for this item
    omega2_name <- paste0("omega2.", item, ".1")
    if (!(omega2_name %in% colnames(model$omega2))) {
      warning(paste("Variance parameter", omega2_name, "not found in model"))
      next
    }
    
    # Get posterior samples
    n_samples <- nrow(model$lambda)
    intercept_samples <- model$lambda[, intercept_name]
    coef_samples <- model$lambda[, param_name]
    variance_samples <- model$omega2[, omega2_name]
    
    # For each value of the covariate
    for (value in values) {
      # Calculate linear predictor for each posterior sample
      linear_pred <- intercept_samples + coef_samples * value
      
      # Calculate probability of support for each sample
      prob_samples <- numeric(n_samples)
      for (i in 1:n_samples) {
        prob_samples[i] <- pnorm(0, mean = linear_pred[i], sd = sqrt(variance_samples[i]), 
                                 lower.tail = FALSE)
      }
      
      # Compute summary statistics
      prob_median <- median(prob_samples)
      prob_lower <- quantile(prob_samples, 0.025)
      prob_upper <- quantile(prob_samples, 0.975)
      
      # Create and add row to results
      result_row <- data.frame(
        covariate = covariate_name,
        value = value,
        item = item_names[item],
        prob_support = prob_median,
        lower_ci = prob_lower, 
        upper_ci = prob_upper
      )
      
      all_results <- rbind(all_results, result_row)
    }
  }
  
  # Check if we got any results
  if (nrow(all_results) == 0) {
    warning(paste("No successful calculations for", covariate_name))
    return(data.frame())  # Return empty data frame
  }
  
  return(all_results)
}

# Improved plotting function with better error handling and formatting
plot_marginal_effects <- function(effects_data, covariate_name) {
  # Check if we have data to plot
  if (nrow(effects_data) == 0) {
    warning(paste("No data to plot for", covariate_name))
    return(NULL)
  }
  
  # Create plot
  p <- ggplot(effects_data, aes(x = value, y = prob_support, group = item, color = item)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = item), alpha = 0.2, color = NA) +
    labs(x = paste("Standardized", covariate_name),
         y = "Probability of Support",
         title = paste("Effect of", covariate_name, "on Militia Support")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          plot.title = element_text(size = 12, face = "bold")) +
    ylim(0, 1)  # Probability is bounded between 0 and 1
  
  return(p)
}

#============================================
# 7. Execute Marginal Effects Analysis
#============================================
# Define covariates of interest with comprehensive selection
covariates_of_interest <- c(
  "age", "gender", "education", "is_capital", "income",
  "ideology", "DemPolGrievence", "PolicyPolGrievence",
  "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg", 
  "NatPride", "NativeRights", "NativeJobs", "Chauvinism", "DemonstrateNational", "DemonstrateTrad",
  "LawOrder", "ChristianSchool", "GayNeighbor", "GayPartner", "ForNeighbor", "ForPartner", "Ukraine"
)

# Create output directory
dir.create("outputs", showWarnings = FALSE)

# Print model structure information for debugging
cat("Model formula:\n")
print(endorseFULL1$call$formula.indiv)

cat("\nFirst few rows of model matrix:\n")
print(head(endorseFULL1$model.matrix.indiv[, 1:5]))

cat("\nParameter naming pattern example:\n")
print(head(colnames(endorseFULL1$lambda), 10))

# Process covariates with enhanced error handling
plots_list <- list()
for (cov in covariates_of_interest) {
  cat("\n=== Processing", cov, "===\n")
  
  tryCatch({
    # Calculate marginal effects
    cat("Calculating marginal effects...\n")
    effects <- calculate_marginal_effects(endorseFULL1, cov)
    
    # Check if we got results
    if (nrow(effects) == 0) {
      cat("No effects calculated for", cov, "\n")
      next
    }
    
    # Print summary of results
    cat("Summary of calculated effects:\n")
    print(summary(effects$prob_support))
    
    # Create plot
    cat("Creating plot...\n")
    plot <- plot_marginal_effects(effects, cov)
    
    # Store if successful
    if (!is.null(plot)) {
      plots_list[[cov]] <- plot
      cat("Successfully created plot for", cov, "\n")
      
      # Save individual plot
      output_path <- file.path(getwd(), "outputs", paste0("marginal_effect_", cov, ".pdf"))
      ggsave(output_path, plot, width = 8, height = 6)
      cat("Saved plot to:", output_path, "\n")
    }
  }, error = function(e) {
    cat("ERROR processing", cov, ":", e$message, "\n")
    cat("Error traceback:\n")
    print(traceback())
  })
}

# Only combine plots that were successfully created
successful_plots <- plots_list[!sapply(plots_list, is.null)]
if (length(successful_plots) > 0) {
  cat("\nCreating combined plot with", length(successful_plots), "successful plots\n")
  
  # Determine sensible layout based on number of plots
  n_plots <- length(successful_plots)
  n_cols <- min(3, n_plots)
  n_rows <- ceiling(n_plots / n_cols)
  
  # Create combined plot with appropriate layout
  combined_plot <- ggarrange(plotlist = successful_plots, 
                             ncol = n_cols, 
                             nrow = n_rows,
                             common.legend = TRUE)
  
  # Save the combined plot
  output_path <- file.path(getwd(), "outputs", "marginal_effects_combined.pdf")
  ggsave(output_path, combined_plot, 
         width = 4 * n_cols, 
         height = 4 * n_rows)
  cat("Saved combined plot to:", output_path, "\n")
} else {
  cat("\nWARNING: No successful plots were generated!\n")
  
  # Final diagnostic information 
  cat("\n--- Final Diagnostic Information ---\n")
  cat("Parameter names in lambda (first 30):\n")
  print(head(colnames(endorseFULL1$lambda), 30))
  
  cat("\nParameter names in omega2:\n")
  print(colnames(endorseFULL1$omega2))
  
  cat("\nModel matrix column names:\n")
  print(colnames(endorseFULL1$model.matrix.indiv))
  
  cat("\nModel call structure:\n")
  print(str(endorseFULL1$call))
}

# Save model object for future reference
saveRDS(endorseFULL1, file.path(getwd(), "outputs", "endorsement_model.rds"))

cat("\n=== Analysis Complete ===\n")
cat("Results are saved in the 'outputs' directory\n")
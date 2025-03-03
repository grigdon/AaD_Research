
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

# Convert imputed data to tibble
data_slvk_vars <- as_tibble(data_slvk_imp$ximp)

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

# Create named list for response questions
Y <- list(Q1 = c("q10a_control", "q10a_experiment"), 
          Q2 = c("q10b_control", "q10b_experiment"), 
          Q3 = c("q10c_control", "q10c_experiment"))

#============================================
# 4.5 Combine Question and Variable Data
#============================================

# Merge the questions data with the variables data using the ID column
data_slvk <- data_slvk_questions %>%
  left_join(data_slvk_vars, by = "id")

# Verify the merge was successful
print("Dimensions of final dataset:")
print(dim(data_slvk))
print("Columns in final dataset:")
print(names(data_slvk))

# Check for any missing values in key columns
missing_summary <- sapply(data_slvk, function(x) sum(is.na(x)))
print("Missing values in each column:")
print(missing_summary)

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
# 6. Marginal Effects Analysis Functions
#============================================
#' Calculate marginal effects for endorsement experiments with debugging
#' @param model_out The fitted endorse model object
#' @param covariate_name Name of the covariate to analyze
#' @param values Vector of values to evaluate the marginal effect at
#' @param n_endorsers Number of endorsers in the experiment
# 
calculate_marginal_effects <- function(model_out, covariate_name, 
                                       values = c(-2, -1, 0, 1, 2),
                                       n_endorsers = 3) {
  # Debug information
  cat("\nDebugging information for", covariate_name, ":\n")
  
  # Input validation
  if (!covariate_name %in% colnames(model_out$model.matrix.indiv)) {
    stop(sprintf("Covariate '%s' not found in model matrix", covariate_name))
  }
  
  results <- list()
  model_matrix_orig <- model_out$model.matrix.indiv
  
  # First, check the actual structure of omega2 and psi2
  cat("Structure of omega2:", str(model_out$omega2), "\n")
  cat("Structure of psi2:", str(model_out$psi2), "\n")
  
  for (i.endorser in 1:n_endorsers) {
    pred_probs <- matrix(NA, nrow = length(values), ncol = 3)
    
    for (i.value in 1:length(values)) {
      # Modify model matrix
      model_matrix_mod <- model_matrix_orig
      covariate_col <- which(colnames(model_matrix_orig) == covariate_name)
      model_matrix_mod[, covariate_col] <- values[i.value]
      
      # Get parameter name for this endorser
      param_name <- paste0(covariate_name, ".", i.endorser, ".1")
      
      # Calculate predicted means
      pred_smean <- model_matrix_mod[, covariate_col, drop = FALSE] %*% 
        t(model_out$lambda[, param_name, drop = FALSE])
      
      # Initialize probability vector
      pred_prob <- numeric(nrow(model_out$lambda))
      
      # Modified to handle different omega2/psi2 structures
      for (i.mcmc in 1:length(pred_prob)) {
        # Get variance components - adjust these lines based on actual structure
        omega2 <- model_out$omega2[i.mcmc, i.endorser] # Modified indexing
        psi2 <- model_out$psi2[i.mcmc, i.endorser]    # Modified indexing
        
        # Calculate probability
        pred_prob[i.mcmc] <- mean(pnorm(
          q = 0,
          mean = pred_smean[, i.mcmc],
          sd = sqrt(omega2 + psi2),
          lower.tail = FALSE
        ))
      }
      
      # Store quantiles
      pred_probs[i.value, ] <- quantile(pred_prob, probs = c(0.025, 0.5, 0.975))
    }
    
    results[[i.endorser]] <- data.frame(
      value = values,
      q025 = pred_probs[, 1],
      median = pred_probs[, 2],
      q975 = pred_probs[, 3],
      endorser = sprintf("Endorser %d", i.endorser)
    )
  }
  
  return(do.call(rbind, results))
}

# Modified analysis workflow to handle errors more gracefully
analyze_and_plot_marginal_effects <- function(model, covariates, 
                                              values = c(-2, -1, 0, 1, 2)) {
  all_effects <- list()
  all_plots <- list()
  
  for (cov in covariates) {
    cat("\nAnalyzing", cov, "...\n")
    
    effects <- try({
      calculate_marginal_effects(model, cov, values)
    })
    
    if (!inherits(effects, "try-error")) {
      all_effects[[cov]] <- effects
      plot <- plot_marginal_effects(effects, cov)
      all_plots[[cov]] <- plot
      print(plot)
      
      # Save individual plot
      pdf(paste0("marginal_effects_", cov, ".pdf"))
      print(plot)
      dev.off()
    } else {
      cat("Error analyzing", cov, "\n")
    }
  }
  
  return(list(effects = all_effects, plots = all_plots))
}

# Define covariates of interest
covariates_of_interest <- c(
  "age", "gender", "education", "is_capital", 
  "ideology", "income", "NatPride", "NativeRights", 
  "NativeJobs", "Chauvinism", "Ukraine"
)

# Run the analysis
results <- analyze_and_plot_marginal_effects(
  model = endorseFULL1,
  covariates = covariates_of_interest
)

# Create a combined plot of all effects
library(gridExtra)

# Arrange all plots in a grid
combined_plot <- do.call(grid.arrange, 
                         c(results$plots, 
                           ncol = 2))  # Adjust ncol as needed

# Save combined plot
pdf("all_marginal_effects.pdf", 
    width = 12, 
    height = 6 * ceiling(length(covariates_of_interest)/2))
print(combined_plot)
dev.off()

# Generate summary table of effects
summary_table <- data.frame(
  Covariate = character(),
  Median_Effect = numeric(),
  Lower_CI = numeric(),
  Upper_CI = numeric(),
  stringsAsFactors = FALSE
)

for (cov in names(results$effects)) {
  effects_data <- results$effects[[cov]]
  summary_table <- rbind(summary_table, data.frame(
    Covariate = cov,
    Median_Effect = median(effects_data$median),
    Lower_CI = median(effects_data$q025),
    Upper_CI = median(effects_data$q975)
  ))
}

# Print summary table
print(summary_table)

# Save summary table
write.csv(summary_table, "marginal_effects_summary.csv", row.names = FALSE)

#============================================
# 7. Run the Analysis
#============================================

# Define covariates of interest
covariates_of_interest <- c(
  "age", "gender", "education", "is_capital", 
  "ideology", "income", "NatPride", "NativeRights", 
  "NativeJobs", "Chauvinism", "Ukraine"
)

# Run the analysis
results <- analyze_and_plot_marginal_effects(
  model = endorseFULL1,
  covariates = covariates_of_interest
)

#============================================
# 8. Generate Summary Statistics
#============================================

# Calculate summary statistics for key variables
summary_stats <- data_slvk %>%
  select(age, gender, education, is_capital, ideology, income) %>%
  summary()

# Print summary statistics
print(summary_stats)

# End of script
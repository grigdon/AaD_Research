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
# 5. Model Fitting
#============================================

# Merge data back in
data_slvk <- as_tibble(merge(data_slvk_vars, data_slvk_questions, by = "id"))

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
# Define the covariates of interest
covariates_of_interest <- c("age", "gender", "education", "is_capital", 
                            "ideology", "income", "DemPolGrievence", 
                            "PolicyPolGrievence", "NatPride", "NativeRights", 
                            "NativeJobs", "LawOrder", "Chauvinism")

# First, let's understand the structure of the model output
print("Structure of delta (coefficients):")
print(head(colnames(endorseFULL1$delta)))
print(dim(endorseFULL1$delta))

calculate_marginal_effects <- function(model_out, covariate_name, values = c(-2, -1, 0, 1, 2)) {
  # Extract model components
  delta_post <- model_out$delta
  lambda_post <- model_out$lambda
  omega2_post <- model_out$omega2
  
  # First, identify the number of endorsers from omega2 column names
  omega2_cols <- colnames(omega2_post)
  n_endorsers <- length(unique(gsub(".*\\.", "", omega2_cols)))
  
  # Get column index for covariate of interest
  cov_cols <- which(colnames(delta_post) == covariate_name)
  if(length(cov_cols) == 0) {
    stop(paste("Covariate", covariate_name, "not found in coefficients."))
  }
  
  # Initialize results list
  results <- list()
  
  # For each endorser (question)
  for(i_endorser in 1:n_endorsers) {
    # Initialize matrix for probability results
    probs_matrix <- matrix(NA, nrow = length(values), ncol = nrow(delta_post))
    
    # Extract the omega2 for this endorser
    omega2_col <- paste0("omega2.", i_endorser, ".1")
    if(!(omega2_col %in% colnames(omega2_post))) {
      warning(paste("Could not find omega2 for endorser", i_endorser))
      next
    }
    
    # For each posterior sample
    for(i_mcmc in 1:nrow(delta_post)) {
      # Get coefficient value for this covariate
      coef_value <- delta_post[i_mcmc, cov_cols]
      
      # Get omega2 for this endorser
      omega2_value <- omega2_post[i_mcmc, omega2_col]
      
      # For each standardized value of the covariate
      for(i_val in 1:length(values)) {
        # Calculate the effect of changing the covariate value
        effect_size <- values[i_val] * coef_value
        
        # Calculate probability using probit transformation
        # Since we're using standardized values and effect size is in standardized units
        probs_matrix[i_val, i_mcmc] <- pnorm(
          effect_size,  # Direct effect size
          mean = 0,     # Reference point (baseline)
          sd = sqrt(omega2_value),  # Uncertainty from model
          lower.tail = FALSE
        )
      }
    }
    
    # Summarize results for this endorser
    results[[i_endorser]] <- data.frame(
      value = values,
      q025 = apply(probs_matrix, 1, function(x) quantile(x, 0.025)),
      median = apply(probs_matrix, 1, function(x) quantile(x, 0.5)),
      q975 = apply(probs_matrix, 1, function(x) quantile(x, 0.975)),
      endorser = paste0("Q", i_endorser)
    )
  }
  
  # Combine results
  final_results <- do.call(rbind, results)
  return(final_results)
}
# Define the plotting function
plot_marginal_effects <- function(effects_data, covariate_name) {
  p <- ggplot(effects_data, aes(x = value, y = median, group = endorser)) +
    geom_line(aes(color = endorser), linewidth = 1) +
    geom_ribbon(aes(ymin = q025, ymax = q975, fill = endorser), alpha = 0.2) +
    labs(title = paste("Marginal Effects of", covariate_name),
         x = paste(covariate_name, "value (standardized)"),
         y = "Probability of Support") +
    theme_minimal() +
    theme(legend.position = "bottom")
  return(p)
}

# Create output directory
dir.create("outputs", showWarnings = FALSE)

# Debug information to understand model structure
print(paste("Model has", length(endorseFULL1$Y), "questions (endorsers)"))
print(paste("Available covariates in model:", 
            paste(colnames(endorseFULL1$delta), collapse=", ")))

# Create and store plots with improved debugging
plots_list <- list()
successful_covs <- character(0)

for(cov in covariates_of_interest) {
  message(paste("Processing covariate:", cov))
  
  # Try to calculate effects with detailed error handling
  effects <- tryCatch({
    calculate_marginal_effects(endorseFULL1, cov)
  }, error = function(e) {
    message(paste("Error calculating effects for", cov, ":", e$message))
    return(NULL)
  })
  
  # Only try to plot if effects were calculated
  if(!is.null(effects)) {
    plots_list[[cov]] <- tryCatch({
      plot <- plot_marginal_effects(effects, cov)
      successful_covs <- c(successful_covs, cov)
      message(paste("Successfully processed:", cov))
      plot
    }, error = function(e) {
      message(paste("Error plotting effects for", cov, ":", e$message))
      return(NULL)
    })
  }
}

# Only combine plots that were successfully created
successful_plots <- plots_list[!sapply(plots_list, is.null)]

if(length(successful_plots) > 0) {
  message(paste("Creating combined plot with", length(successful_plots), "successful plots"))
  message(paste("Successfully processed covariates:", paste(successful_covs, collapse=", ")))
  
  # Save individual plots first (as backup)
  for(cov in names(successful_plots)) {
    output_path <- file.path(getwd(), "outputs", paste0("marginal_effect_", cov, ".pdf"))
    ggsave(output_path, successful_plots[[cov]], width = 8, height = 6)
    message(paste("Saved individual plot to:", output_path))
  }
  
  # Create combined plot if there are multiple successful plots
  if(length(successful_plots) > 1) {
    combined_plot <- ggarrange(plotlist = successful_plots, 
                               ncol = 2, 
                               nrow = ceiling(length(successful_plots)/2))
    
    output_path <- file.path(getwd(), "outputs", "marginal_effects_combined.pdf")
    ggsave(output_path, combined_plot, 
           width = 16, height = 6 * ceiling(length(successful_plots)/2))
    message(paste("Saved combined plot to:", output_path))
  }
} else {
  warning("No successful plots were generated! Check the model structure and variable names.")
  
  # Additional debugging information
  print("Model structure check:")
  print(str(endorseFULL1))
  print("First few rows of delta matrix:")
  print(head(endorseFULL1$delta))
}
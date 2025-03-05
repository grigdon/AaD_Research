# Complete Analysis Script for Militia Support in Slovakia
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

# Graph the endorsement experiment to ensure it falls in line with the Siroky findings.

df <- data.frame(as.matrix(endorseFULL1$delta))
ci <- data.frame(variables = colnames(df[2:25]), mean = apply(df[2:25], 2, mean), sd = apply(df[2:25], 2, sd))
ci <- mutate(ci, max = mean + 1.96 * sd, min = mean - 1.96 * sd)
ci$variables <- c("Age", "Gender", "Education", "Capital", "Ideology", "Income", "DemPolGrievence", "PolicyPolGrievence", "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg", "NatPride", "NativeRights", "NativeJobs", "DemonstrateNational", "LawOrder", "Chauvinism", "ChristianSchool", "DemonstrateTrad", "GayNeighbor", "GayPartner", "ForNeighbor", "ForPartner", "Ukraine")
ci <- mutate(ci, variables= fct_reorder(variables, mean))


ggplot(ci, aes(x = variables, y = mean)) + geom_point() + geom_errorbar(aes(ymin = min, ymax = max)) + coord_flip() + geom_abline(slope = 0, intercept = 0, color = "red") + ggtitle("Model 2: Full Model") + theme(plot.title = element_text(hjust = 0.5)) 


#==========================================================#
# Improved Marginal Effects Functions -- Version_7         #
#==========================================================#

# Define the covariates of interest
covariates_of_interest <- c("age", "gender", "education", "is_capital", 
                            "ideology", "income", "DemPolGrievence", 
                            "PolicyPolGrievence", "NatPride", "NativeRights", 
                            "NativeJobs", "LawOrder", "Chauvinism")

# Modified function to calculate the marginal effects with categorical values
calculate_marginal_effects <- function(model_out, covariate_name, 
                                       values = c(-1.5, -0.5, 0.5, 1.5)) {
  
  # Extract posterior samples
  delta_post <- model_out$delta
  lambda_post <- model_out$lambda
  omega2_post <- model_out$omega2
  
  # Find columns corresponding to the covariate
  delta_cols <- grep(paste0("^", covariate_name, "$"), colnames(delta_post))
  if(length(delta_cols) == 0) {
    stop(paste("Covariate", covariate_name, "not found in coefficients."))
  }
  
  # Find omega2 columns
  omega2_cols <- colnames(omega2_post)[grep("omega2", colnames(omega2_post))]
  endorser_nums <- as.numeric(gsub("omega2\\.(\\d+)\\..*", "\\1", omega2_cols))
  n_endorsers <- length(unique(endorser_nums))
  
  # Debug information
  print(paste("Processing", n_endorsers, "endorsement questions"))
  print(paste("Endorser numbers found:", paste(sort(unique(endorser_nums)), collapse=", ")))
  
  # Create categorical labels
  categorical_labels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
  
  # Initialize results list
  results <- list()
  
  # Process each endorser
  for(i_endorser in sort(unique(endorser_nums))) {
    
    # Get omega2 column for this endorser
    omega2_col <- paste0("omega2.", i_endorser, ".1")
    
    # Debug information
    print(paste("Processing endorser", i_endorser, "using omega2 column:", omega2_col))
    
    if(!(omega2_col %in% colnames(omega2_post))) {
      warning(paste("Could not find omega2 for endorser", i_endorser))
      next
    }
    
    # Initialize matrix for probability results
    probs_matrix <- matrix(NA, nrow = length(values), ncol = nrow(delta_post))
    
    # Process each MCMC sample
    for(i_mcmc in 1:nrow(delta_post)) {
      # Get coefficient value
      coef_value <- delta_post[i_mcmc, delta_cols]
      
      # Get omega2 value
      omega2_value <- omega2_post[i_mcmc, omega2_col]
      
      # Process each value
      for(i_val in 1:length(values)) {
        # Calculate effect size (direction-preserving)
        effect_size <- values[i_val] * coef_value
        
        # Calculate probability with correct orientation
        # Use lower.tail = TRUE for consistency with coefficient interpretation
        probs_matrix[i_val, i_mcmc] <- pnorm(
          effect_size,
          mean = 0,
          sd = sqrt(omega2_value),
          lower.tail = TRUE
        )
      }
    }
    
    # Create data frame for this endorser
    results[[as.character(i_endorser)]] <- data.frame(
      value = values,
      category = factor(categorical_labels, levels = categorical_labels),
      q025 = apply(probs_matrix, 1, function(x) quantile(x, 0.025)),
      q250 = apply(probs_matrix, 1, function(x) quantile(x, 0.25)),
      median = apply(probs_matrix, 1, function(x) quantile(x, 0.5)),
      q750 = apply(probs_matrix, 1, function(x) quantile(x, 0.75)),
      q975 = apply(probs_matrix, 1, function(x) quantile(x, 0.975)),
      endorser = paste0("Q", i_endorser)
    )
  }
  
  # Combine and return results
  final_results <- do.call(rbind, results)
  return(final_results)
}

# Improved plotting function for box and whisker plots
plot_marginal_effects <- function(effects_data, covariate_name) {
  
  # Better policy labels
  question_labels <- c(
    "Q1" = "Infrastructure Policy",
    "Q2" = "Border Protection", 
    "Q3" = "National Security"
  )
  
  # Prepare plot data
  plot_data <- effects_data
  plot_data$question_label <- question_labels[plot_data$endorser]
  
  # Set color palette
  my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c")
  
  # Create box and whisker plot
  p <- ggplot(plot_data, aes(x = category, y = median, fill = endorser)) +
    geom_boxplot(
      aes(
        ymin = q025,
        lower = q250,
        middle = median,
        upper = q750,
        ymax = q975
      ),
      stat = "identity",
      width = 0.7,
      position = position_dodge(width = 0.8)
    ) +
    labs(
      title = paste("Effect of", gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", covariate_name), "on Militia Support"),
      subtitle = "Across three different policy domains (95% confidence intervals)",
      x = paste(gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", covariate_name)),
      y = "Probability of Support"
    ) +
    scale_fill_manual(values = my_colors, labels = question_labels) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

# Sets a list to be populated with the generated plots.
plots_list <- list()
successful_covs <- character(0)

# Process each covariate
for(cov in covariates_of_interest) {
  message(paste("Processing covariate:", cov))
  
  # Try to calculate marginal effects with error handling
  effects <- tryCatch({
    calculate_marginal_effects(
      endorseFULL1, 
      cov, 
      values = c(-1.5, -0.5, 0.5, 1.5)  # Values corresponding to categorical levels
    )
  }, error = function(e) {
    message(paste("Error calculating effects for", cov, ":", e$message))
    return(NULL)
  })
  
  # Generate and store plot if effects were calculated successfully
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

# Filter out unsuccessful plots
successful_plots <- plots_list[!sapply(plots_list, is.null)]

# Save plots to output directory
if(length(successful_plots) > 0) {
  message(paste("Creating combined plots with", length(successful_plots), "successful plots"))
  
  # Create output directory if it doesn't exist
  output_dir <- file.path(getwd(), "outputs_boxplots")
  if(!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Save individual plots
  for(cov in names(successful_plots)) {
    output_path <- file.path(output_dir, paste0("marginal_effect_", cov, ".pdf"))
    ggsave(output_path, successful_plots[[cov]], width = 8, height = 6)
    message(paste("Saved individual plot to:", output_path))
  }
  
  # Create combined plots, 4 per page
  n_plots <- length(successful_plots)
  plots_per_page <- 4
  n_pages <- ceiling(n_plots / plots_per_page)
  
  for(page in 1:n_pages) {
    start_idx <- (page-1) * plots_per_page + 1
    end_idx <- min(page * plots_per_page, n_plots)
    
    if(start_idx > n_plots) break
    
    page_plots <- successful_plots[start_idx:end_idx]
    
    combined_plot <- ggarrange(
      plotlist = page_plots, 
      ncol = 2, 
      nrow = ceiling(length(page_plots)/2),
      common.legend = TRUE,
      legend = "bottom"
    )
    
    output_path <- file.path(output_dir, paste0("marginal_effects_boxplots_page", page, ".pdf"))
    ggsave(output_path, combined_plot, width = 16, height = 12)
    message(paste("Saved combined plot page", page, "to:", output_path))
  }
} else {
  warning("No successful plots were generated! Check the model structure and variable names.")
}
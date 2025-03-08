# Complete Analysis Script for Militia Support in Czechia
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
# 1. Data Loading and Initial Processing for Czechia
#====================================================

CZData <- read_sav("~/projects/AaD_Research/datasets/scrubbed_datasets/czechia_scrubbed.sav")

# Define questions for the endorsement experiment
questions <- c("id", "Q10AA_control_reversed", "Q10AB_control_reversed", "Q10AC_control_reversed",
               "Q10BA_experiment_reversed", "Q10BB_experiment_reversed", "Q10BC_experiment_reversed"
)
# Select relevant columns for endorsement analysis
data_cz_questions <- CZData[questions]

# Define variables to keep
vars <- c("id", "Male", "Age", "Education", "Capital", "IdeologyLR", "Income", "FamIncome", "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "EconGrievanceProspMostFams",
          "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine",
          "NativeJobs", "NativeRights", "Religiosity", "VoteFarRight")

# Subset and recode variables
data_cz_vars <- CZData[vars]

# Convert all variables to numeric
data_cz_vars <- mutate(data_cz_vars, across(everything(), ~as.numeric(.)))

# Merge the questions and standardized variables datasets
data_cz <- left_join(data_cz_questions, data_cz_vars, by = "id")

# Create named list for response questions
Y <- list(Q1 = c("Q10AA_control_reversed", "Q10BA_experiment_reversed"), 
          Q2 = c("Q10AB_control_reversed", "Q10BB_experiment_reversed"), 
          Q3 = c("Q10AC_control_reversed", "Q10BC_experiment_reversed"))

#====================================================
# 2. Creating the endorse object
#====================================================

# Creating an endorse object, excluding all covariates that are in the set { traditionalism }

endorse_object <- endorse(Y = Y, 
                          data = data_cz,
                          identical.lambda = FALSE,
                          covariates = TRUE,
                          formula.indiv = formula( ~ Male + Age + Education + Capital + IdeologyLR + Income + FamIncome + DemPolGrievance +
                                                   PolicyPolGrievance + EconGrievanceRetro + EconGrievanceProspInd + EconGrievanceProspAgg +
                                                   EconGrievanceProspMostFams + GayNeighbor + GayFamily + ForNeighbor + ForPartner + Ukraine +
                                                   NativeJobs + NativeRights + Religiosity + VoteFarRight
                                                  ),
                          omega2.out = TRUE,
                          hierarchical = FALSE
)
#====================================================
# 2.5 Output the acceptance ratio for each question
#====================================================

# Extract acceptance ratios from the endorse object
acceptance_ratios <- data.frame(
  Question = paste("Question", 1:3),
  Ratio = endorse_object$accept.ratio
)

# Create a bar plot of acceptance ratios
acceptance_plot <- ggplot(acceptance_ratios, aes(x = Question, y = Ratio)) +
  geom_bar(stat = "identity", fill = "#4682B4", width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", Ratio)), vjust = -0.5, size = 4) +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1, by = 0.1)) +
  labs(
    title = "Czechia: Metropolis-Hastings Acceptance Ratios by Question",
    x = NULL,
    y = "Acceptance Ratio"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold")
  )

# Save the plot
ggsave("~/projects/AaD_Research/output/metro/czechia_acceptance_ratios.pdf", 
       acceptance_plot, width = 8, height = 6)

#====================================================
# 3. Plotting coefficient plots from the delta matrix 
#====================================================

# Create the dataframe using posterior samples
delta_matrix_values <- data.frame(
  mean = apply(endorse_object$delta[, 2:23], 2, mean),
  lower = apply(endorse_object$delta[, 2:23], 2, quantile, 0.025),
  upper = apply(endorse_object$delta[, 2:23], 2, quantile, 0.975)
)

# Add variable names and categories
delta_matrix_values$variables <- colnames(endorse_object$delta)[2:23]
delta_matrix_values$category <- NA


# Define categories
ses_demographics <- c("Age", "Male", "Education", "Capital", "IdeologyLR", "Income", "FamIncome", "Religiosity")
political_economic_grievances <- c("DemPolGrievance", "PolicyPolGrievance", "EconGrievanceRetro", "EconGrievanceProspInd",
                                   "EconGrievanceProspAgg", "EconGrievanceProspMostFams")
nationalism <- c( "NativeRights", "NativeJobs", "VoteFarRight")
boundary_maintenance <- c("GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine")

# Assign categories
delta_matrix_values <- delta_matrix_values %>%
  mutate(
    category = case_when(
      variables %in% ses_demographics ~ "SES Demographics",
      variables %in% political_economic_grievances ~ "Political & Economic Grievances",
      variables %in% nationalism ~ "Nationalism",
      variables %in% boundary_maintenance ~ "Boundary Maintenance & Prejudice"
    )
  )

# Reorder variables within each category by mean
delta_matrix_values <- delta_matrix_values %>%
  group_by(category) %>%
  mutate(variables = fct_reorder(variables, mean)) %>%
  ungroup()

# Reorder categories
category_order <- c(
  "SES Demographics", 
  "Political & Economic Grievances", 
  "Nationalism", 
  "Boundary Maintenance & Prejudice"
)
delta_matrix_values$category <- factor(delta_matrix_values$category, levels = category_order)

# Define custom labels for variables
custom_labels <- c(
  "Age" = "Age",
  "Male" = "Male",
  "Education" = "Education",
  "Capital" = "Capital",
  "IdeologyLR" = "Political Ideology",
  "Income" = "Personal Income",
  "FamIncome" = "Family Income",
  "DemPolGrievance" = "Political Grievance (Democracy)",
  "PolicyPolGrievance" = "Policy Grievance",
  "EconGrievanceRetro" = "Economic Grievance (Retro)",
  "EconGrievanceProspInd" = "Economic Grievance (Prospective-Ind)",
  "EconGrievanceProspAgg" = "Economic Grievance (Prospective-Agg)",
  "EconGrievanceProspMostFams" = "Economic Grievance (ProspMostFams)",
  "NativeRights" = "Native Rights",
  "NativeJobs" = "Native Jobs",
  "VoteFarRight" = "Far Right Voter",
  "Religiosity" = "Religiosity",
  "GayNeighbor" = "Anti-Gay Neighbor",
  "GayFamily" = "Anti-Gay Family",
  "ForNeighbor" = "Anti-Foreigner Neighbor",
  "ForPartner" = "Anti-Foreigner Neighbor",
  "Ukraine" = "Anti-Ukrainian Refugee"
)


# Create the plot
plot <- ggplot(delta_matrix_values, aes(x = variables, y = mean)) +
  geom_point(size = 1, shape = 10) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 1, size = .25) + 
  coord_flip() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.5) + 
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  scale_x_discrete(labels = custom_labels) +
  theme_classic() +
  ggtitle("Czechia: Coefficient Estimates by Explanatory Variable") + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    strip.text.y = element_text(angle = 0, hjust = 0, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  labs(x = NULL, y = "Coefficient Estimate")

# Save to PDF
ggsave("~/projects/AaD_Research/output/plots/czechia/coef/czechia_coef_plot.pdf", plot, width = 12, height = 10)

#===================================================
# Bayesian Covariate Analysis: Boxplot Visualization
#===================================================

#===================
# 1. Data Assignment
#===================

# The covariates you wish to analyze.
# Note: Must be identical to column titles.

covariates_of_interest <- c(
  "Male", "Age", "Education", "Capital", "IdeologyLR", "Income", "FamIncome", "DemPolGrievance", "PolicyPolGrievance",
  "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "EconGrievanceProspMostFams",
  "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine",
  "NativeJobs", "NativeRights", "Religiosity", "VoteFarRight"
)

# Variable labels for plotting, i.e., the covariate "Age" would be displayed as "Age Group"

variable_labels <- c(
  "Age" = "Age Group",
  "Male" = "Gender",
  "Education" = "Education Level",
  "Capital" = "Capital Resident",
  "IdeologyLR" = "Political Ideology",
  "Income" = "Household Income",
  "FamIncome" = "Family Income",
  "DemPolGrievance" = "Democratic Grievance",
  "PolicyPolGrievance" = "Policy Grievance",
  "EconGrievanceRetro" = "Economic Grievance (Retrospective)",
  "EconGrievanceProspInd" = "Economic Grievance (Prospective Individual)",
  "EconGrievanceProspAgg" = "Economic Grievance (Prospective Aggregate)",
  "EconGrievanceProspMostFams" = "Economic Grievance (Prospective Most Families)",
  "GayNeighbor" = "Accept Gay Neighbor",
  "GayFamily" = "Accept Gay Family",
  "ForNeighbor" = "Accept Foreign Neighbor",
  "ForPartner" = "Accept Foreign Partner",
  "Ukraine" = "Ukraine Support",
  "NativeJobs" = "Native Jobs Priority",
  "NativeRights" = "Native Rights Support",
  "Religiosity" = "Religiosity",
  "VoteFarRight" = "Far-Right Voting"
)

# Setting labels for binary variables, i.e., T/F

binary_covariates <- list(
  Male = list(values = c(1, 2), labels = c("Female", "Male")),
  Capital = list(values = c(1, 2), labels = c("Rural", "Capital")),
  VoteFarRight = list(values = c(0, 1), labels = c("Other", "Far-Right"))
)

# Setting labels for ordinal variables, i.e., {1, 2, 3, 4, 5}

ordinal_covariates <- list(
  Age = list(
    values = c(1, 2, 3, 4, 5, 6),
    labels = c("15-19", "20-29", "30-39", "40-54", "55-64", "65+")
  ),
  Education = list(
    values = c(1, 2, 3),
    labels = c("Basic Education", "High School with Maturita", "Higher Education")
  ),
  Income = list(
    values = c(0, 1, 2, 3, 4, 5),
    labels = c("None", "<300 EUR", "300-500 EUR", "501-700 EUR", "701-900 EUR", "900+ EUR")
  ),
  FamIncome = list(
    values = c(1, 2, 3, 4, 5),
    labels = c("< 3,000 CZK", "3,000-5,999 CZK", "6,000-8,999 CZK", "9,000-12,999 CZK", ">13,000 CZK")
  ),
  IdeologyLR = list(
    values = c(1, 2, 3, 4, 5),
    labels = c("Def Left", "Rather Left", "Middle", "Rather Right", "Def Right")
  ),
  DemPolGrievance = list(
    values = c(1, 2, 3, 4),
    labels = c("very sat", "rather sat", "rather unsat", "very unsat")
  ),
  PolicyPolGrievance = list(
    values = c(1, 2, 3, 4),
    labels = c("very sat", "rather sat", "rather unsat", "very unsat")
  ),
  EconGrievanceRetro = list(
    values = c(1, 2, 3, 4, 5),
    labels = c("much better", "better", "same", "worse", "much worse")
  ),
  EconGrievanceProspInd = list(
    values = c(1, 2, 3, 4, 5),
    labels = c("much better", "better", "same", "worse", "much worse")
  ),
  EconGrievanceProspAgg = list(
    values = c(1, 2, 3, 4, 5),
    labels = c("much better", "better", "same", "worse", "much worse")
  ),
  EconGrievanceProspMostFams = list(
    values = c(1, 2, 3, 4, 5),
    labels = c("much better", "better", "same", "worse", "much worse")
  ),
  GayNeighbor = list(
    values = c(1, 2, 3, 4),
    labels = c("would certainly NOT bother me", "would probably NOT mind", 
               "would probably mind", "would certainly mind")
  ),
  GayFamily = list(
    values = c(1, 2, 3, 4),
    labels = c("would certainly NOT bother me", "would probably NOT mind", 
               "would probably mind", "would certainly mind")
  ),
  ForNeighbor = list(
    values = c(1, 2, 3, 4),
    labels = c("would certainly NOT bother me", "would probably NOT mind", 
               "would probably mind", "would certainly mind")
  ),
  ForPartner = list(
    values = c(1, 2, 3, 4),
    labels = c("would certainly NOT bother me", "would probably NOT mind", 
               "would probably mind", "would certainly mind")
  ),
  Ukraine = list(
    values = c(1, 2, 3, 4),
    labels = c("would certainly NOT bother me", "would probably NOT mind", 
               "would probably mind", "would certainly mind")
  ),
  NativeJobs = list(
    values = c(1, 2, 3, 4),
    labels = c("def disagree", "rather disagree", "rather agree", "def agree")
  ),
  NativeRights = list(
    values = c(1, 2, 3, 4),
    labels = c("def disagree", "rather disagree", "rather agree", "def agree")
  ),
  Religiosity = list(
    values = c(1, 2, 3, 4, 5),
    labels = c("never", "few times a year", "once per month", "once per week", "few times a week")
  )
)
#==================
# 2. Core Functions
#==================

calculate_marginal_effects <- function(model_out, covariate_name) {
  # Parameter setup
  if(covariate_name %in% names(binary_covariates)) {
    spec <- binary_covariates[[covariate_name]]
    values <- spec$values
    labels <- if(!is.null(spec$labels)) spec$labels else c("No", "Yes")
  } else if(covariate_name %in% names(ordinal_covariates)) {
    spec <- ordinal_covariates[[covariate_name]]
    values <- spec$values
    labels <- spec$labels
  } else {
    values <- seq(-2, 2, length.out = 5)
    labels <- c("err", "err", "err", "err", "err")
  }
  
  # Parameter extraction
  delta_post <- model_out$delta
  lambda_post <- model_out$lambda
  omega2_post <- model_out$omega2
  sigma2_post <- if("sigma2" %in% names(model_out)) {
    model_out$sigma2
  } else {
    matrix(1, nrow = nrow(delta_post), ncol = 1)
  }
  
  # Find parameter columns
  delta_col <- grep(paste0("^", covariate_name, "$"), colnames(delta_post))
  lambda_cols <- grep(paste0("^", covariate_name, "\\.\\d+\\.1$"), colnames(lambda_post))
  
  if(length(delta_col) == 0 || length(lambda_cols) == 0) {
    stop("Covariate not found in model parameters: ", covariate_name)
  }
  
  # Get endorser information
  omega2_cols <- colnames(omega2_post)[grep("omega2", colnames(omega2_post))]
  endorser_nums <- unique(as.numeric(gsub("omega2\\.(\\d+)\\..*", "\\1", omega2_cols)))
  
  # Initialize probability array
  combined_probs <- array(NA, dim = c(length(values), nrow(delta_post), length(endorser_nums)))
  
  # Process each endorser
  for(i_endorser in seq_along(endorser_nums)) {
    e_num <- endorser_nums[i_endorser]
    
    omega2_col <- paste0("omega2.", e_num, ".1")
    lambda_col <- paste0(covariate_name, ".", e_num, ".1")
    
    if(!all(c(omega2_col, lambda_col) %in% c(colnames(omega2_post), colnames(lambda_post)))) next
    
    for(i_mcmc in 1:nrow(delta_post)) {
      lambda_val <- lambda_post[i_mcmc, lambda_col]
      delta_val <- delta_post[i_mcmc, delta_col]
      total_effect <- lambda_val + delta_val
      total_var <- omega2_post[i_mcmc, omega2_col] + sigma2_post[i_mcmc, 1]
      
      for(i_val in seq_along(values)) {
        effect_size <- values[i_val] * total_effect
        z_score <- effect_size / sqrt(total_var)
        combined_probs[i_val, i_mcmc, i_endorser] <- pnorm(z_score)
      }
    }
  }
  
  # Average across endorsers
  averaged_probs <- apply(combined_probs, c(1,2), mean, na.rm = TRUE)
  
  data.frame(
    value = values,
    category = factor(labels, levels = labels),
    mean = apply(averaged_probs, 1, mean),
    q025 = apply(averaged_probs, 1, quantile, 0.025),
    q250 = apply(averaged_probs, 1, quantile, 0.25),
    median = apply(averaged_probs, 1, median),
    q750 = apply(averaged_probs, 1, quantile, 0.75),
    q975 = apply(averaged_probs, 1, quantile, 0.975)
  )
}

plot_marginal_effects <- function(effects_data, covariate_name) {
  ggplot(effects_data, aes(x = category)) +
    geom_boxplot(
      aes(
        ymin = q025,
        lower = q250,
        middle = median,
        upper = q750,
        ymax = q975
      ),
      stat = "identity",
      fill = "#2c7bb6",
      color = "darkblue",
      width = 0.6,
      outlier.shape = NA
    ) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkred") +
    labs(
      title = variable_labels[covariate_name],
      x = "Category",
      y = "Probability of Support"
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = scales::percent_format()
    ) +
    theme_bw(base_size = 12) +
    theme(
      text = element_text(family = "Times"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    )
}

#=========================
# 3. Function Calls (Main)
#=========================

# Main analysis
message("Starting Bayesian endorsement analysis...\n")
start_time <- Sys.time()
output_dir <- "~/projects/AaD_Research/output/plots/czechia/covar"

# Create output directory if needed
if(!dir.exists(output_dir)) {
  dir.create(output_dir)
  message("Created output directory: ", normalizePath(output_dir))
}

total_covariates <- length(covariates_of_interest)
success_count <- 0
error_count <- 0

message("Processing ", total_covariates, " covariates:")
pb <- txtProgressBar(min = 0, max = total_covariates, style = 3)

for(i in seq_along(covariates_of_interest)) {
  cov <- covariates_of_interest[i]
  setTxtProgressBar(pb, i)
  
  tryCatch({
    # Calculate effects and generate plot
    effects <- calculate_marginal_effects(endorse_object, cov)
    p <- plot_marginal_effects(effects, cov)
    
    # Save plot immediately
    ggsave(
      filename = file.path(output_dir, paste0(cov, "_effect.pdf")),
      plot = p,
      width = 8,
      height = 6,
      device = "pdf"
    )
    
    # Track success
    success_count <- success_count + 1
    message("\n✅ Successfully processed: ", cov)
    
  }, error = function(e) {
    # Track errors
    error_count <- error_count + 1
    message("\n❌ Failed to process ", cov, ": ", conditionMessage(e))
  })
}
close(pb)

# Final report
message("\n===== Analysis Summary =====")
message("Successfully processed: ", success_count, " covariates")
message("Failed to process:     ", error_count, " covariates")
message("Execution time:        ", round(difftime(Sys.time(), start_time, units = "mins"), 1), " minutes")
message("Output location:       ", normalizePath(output_dir))
message("===================================")

# clears environment variables
rm(list = ls())
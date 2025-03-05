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
library(Cairo)

#====================================================
# 1. Data Loading and Initial Processing for Slovakia
#====================================================

SKdata <- read_sav("~/projects/AaD_Research/datasets/scrubbed_datasets/slovakia_scrubbed_dataset.sav")

# Define questions for the endorsement experiment
questions <- c("id", "q10a_control", "q10b_control", "q10c_control", 
               "q10a_experiment", "q10b_experiment", "q10c_experiment")

# Select relevant columns for endorsement analysis
data_slvk_questions <- SKdata[questions]

# Define variables to keep
vars <- c("id", "male", "age", "educ", "capital", "ideology", "income", "DemPolGrievance", "PolicyPolGrievance",
      "DemonstrateTrad", "DemonstrateNational", "PetitionSameSex", "VoteFarRight", "VotePrevFarRight",
      "ideologyLC", "SocialMediaUse", "InternetUse", "SlovakNationality", "FAMincome", "Nationalist",
      "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg", "EconGrievenceProspMostFams",
      "NatPride", "RomaPartner", "RomaNeighbor", "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine",
      "ChristianSchool", "MaleChauvinism", "LawOrder", "ChurchPolitics", "Abortion", "TradMarriage", "SexbMarriage",
      "ChildHome", "MaleJobs", "NativeJobs", "NativeRights", "Religiosity")

# Subset and recode variables
data_slvk_vars <- SKdata[vars]

# Convert all variables to numeric
data_slvk_vars <- mutate(data_slvk_vars, across(everything(), ~as.numeric(.)))

# Merge the questions and standardized variables datasets
data_slvk <- left_join(data_slvk_questions, data_slvk_vars, by = "id")

# Create named list for response questions
Y <- list(Q1 = c("q10a_control", "q10a_experiment"), 
          Q2 = c("q10b_control", "q10b_experiment"), 
          Q3 = c("q10c_control", "q10c_experiment"))

#====================================================
# 2. Creating the endorse object
#====================================================

# Creating an endorse object, excluding all covariates that are in the set { traditionalism }

endorse_object <- endorse(Y = Y, 
                          data = data_slvk,
                          identical.lambda = FALSE,
                          covariates = TRUE,
                          formula.indiv = formula( ~ age + male + educ + 
                                                     capital + ideology + income + 
                                                     DemPolGrievance + PolicyPolGrievance + 
                                                     EconGrievenceRetro + EconGrievenceProspInd + 
                                                     EconGrievenceProspAgg + 
                                                     NativeRights + NativeJobs + NatPride +
                                                     DemonstrateNational + SlovakNationality +
                                                     GayNeighbor + LawOrder + MaleChauvinism + ChristianSchool +
                                                     GayFamily + ForNeighbor + DemonstrateTrad +
                                                     ForPartner + Ukraine + VoteFarRight + Nationalist + 
                                                     FAMincome + Religiosity
                                                  ),
                          hierarchical = FALSE
)

#====================================================
# 3. Plotting coefficient plots from the delta matrix 
#====================================================

# Create the dataframe using posterior samples
delta_matrix_values <- data.frame(
  mean = apply(endorse_object$delta[, 2:30], 2, mean),
  lower = apply(endorse_object$delta[, 2:30], 2, quantile, 0.025),
  upper = apply(endorse_object$delta[, 2:30], 2, quantile, 0.975)
)

# Add variable names and categories
delta_matrix_values$variables <- colnames(endorse_object$delta)[2:30]
delta_matrix_values$category <- NA

# Define categories
ses_demographics <- c("age", "male", "educ", "capital", "ideology", "income", "FAMincome")
political_economic_grievances <- c("DemPolGrievance", "PolicyPolGrievance", 
                                   "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg")
nationalism <- c("NatPride", "NativeRights", "NativeJobs", "DemonstrateNational", 
                 "SlovakNationality", "Nationalist", "VoteFarRight")
traditionalism <- c("LawOrder", "MaleChauvinism", "ChristianSchool", "DemonstrateTrad", "Religiosity")
boundary_maintenance <- c("GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine")

# Assign categories
delta_matrix_values <- delta_matrix_values %>%
  mutate(
    category = case_when(
      variables %in% ses_demographics ~ "SES Demographics",
      variables %in% political_economic_grievances ~ "Political & Economic Grievances",
      variables %in% nationalism ~ "Nationalism",
      variables %in% traditionalism ~ "Traditionalism",
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
  "Traditionalism", 
  "Boundary Maintenance & Prejudice"
)
delta_matrix_values$category <- factor(delta_matrix_values$category, levels = category_order)

# Define custom labels for variables
custom_labels <- c(
  "age" = "Age",
  "male" = "Male",
  "educ" = "Education",
  "capital" = "Capital",
  "ideology" = "Political Ideology",
  "income" = "Personal Income",
  "FAMincome" = "Family Income",
  "DemPolGrievance" = "Political Grievance (Democracy)",
  "PolicyPolGrievance" = "Policy Grievance",
  "EconGrievenceRetro" = "Economic Grievance (Retro)",
  "EconGrievenceProspInd" = "Economic Grievance (Prospective-Ind)",
  "EconGrievenceProspAgg" = "Economic Grievance (Prospective-Agg)",
  "NatPride" = "National Pride",
  "NativeRights" = "Native Rights",
  "NativeJobs" = "Native Jobs",
  "DemonstrateNational" = "Demonstrated for National Values",
  "SlovakNationality" = "Slovak Nationality",
  "Nationalist" = "Prefers Nationalist Politics",
  "VoteFarRight" = "Far Right Voter",
  "LawOrder" = "Law & Order Support",
  "MaleChauvinism" = "Male Chauvinism Support",
  "ChristianSchool" = "Christian Schools Support",
  "DemonstrateTrad" = "Demonstrate Traditionalism",
  "Religiosity" = "Religiosity",
  "GayNeighbor" = "Anti-Gay Neighbor",
  "GayFamily" = "Anti-Gay Family",
  "ForNeighbor" = "Anti-Foreigner Neighbor",
  "ForPartner" = "Anti-Foreigner Neighbor",
  "Ukraine" = "Anti-Ukrainian Refugee"
)


# Create the plot
ggplot(delta_matrix_values, aes(x = variables, y = mean)) +
  geom_point(size = 1, shape = 10) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 1, size = .25) + 
  coord_flip() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.5) + 
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  scale_x_discrete(labels = custom_labels) +
  theme_classic() +
  ggtitle("Model 2: Coefficient Estimates by Category") + 
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

#============================================
# 4. Marginal Effects Function
#============================================

# Define the covariates of interest
covariates_of_interest <- c("age", "male", "educ", "capital", "ideology", "income", "FAMincome", 
                            "DemPolGrievance", "PolicyPolGrievance", "EconGrievenceRetro", 
                            "EconGrievenceProspInd", "EconGrievenceProspAgg", "NatPride", "NativeRights",
                            "NativeJobs", "DemonstrateNational", "SlovakNationality", "Nationalist", 
                            "VoteFarRight", "LawOrder", "MaleChauvinism", "ChristianSchool", "DemonstrateTrad",
                            "Religiosity", "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine"
                            )

# Expanded covariates categorization
binary_covariates <- list(
  gender = list(
    values = c(1, 2),
    labels = c("Male", "Female")
  ),
  is_capital = list(
    values = c(1, 2),
    labels = c("Not Capital", "Capital")
  ),
  VoteFarRight = list(
    values = c(0, 1),
    labels = c("No", "Yes")
  ),
  Nationalist = list(
    values = c(0, 1),
    labels = c("No", "Yes")
  ),
  DemonstrateNational = list(
    values = c(0, 1),
    labels = c("No", "Yes")
  )
)

special_covariates <- list(
  age = list(
    values = c(1, 2, 3, 4, 5),
    labels = c("Very Young", "Young", "Middle-Aged", "Older", "Elderly")
  ),
  educ = list(
    values = c(1, 2, 3),
    labels = c("Low", "Medium", "High")
  ),
  income = list(
    values = c(1, 2, 3, 4),
    labels = c("Low", "Lower-Middle", "Upper-Middle", "High")
  )
)

# Default labels for standard continuous covariates
default_labels <- c("Very Low", "Low", "Moderate", "High", "Very High")

# Improved marginal effects calculation function
calculate_marginal_effects <- function(
    model_out, 
    covariate_name, 
    values = NULL, 
    labels = NULL, 
    custom_quantiles = c(-1.5, -0.5, 0.5, 1.5)
) {
  # Determine appropriate values and labels
  if (is.null(values) || is.null(labels)) {
    if (covariate_name %in% names(binary_covariates)) {
      # Binary covariates
      values <- binary_covariates[[covariate_name]]$values
      labels <- binary_covariates[[covariate_name]]$labels
    } else if (covariate_name %in% names(special_covariates)) {
      # Special covariates with predefined levels
      values <- special_covariates[[covariate_name]]$values
      labels <- special_covariates[[covariate_name]]$labels
    } else {
      # Default continuous covariates
      values <- custom_quantiles
      labels <- default_labels[1:length(values)]
    }
  }
  
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
  
  # Initialize probability array
  combined_probs <- array(NA, dim = c(length(values), nrow(delta_post), n_endorsers))
  valid_endorser_idx <- integer(0)
  
  # Process each endorser
  for(i_endorser in sort(unique(endorser_nums))) {
    omega2_col <- paste0("omega2.", i_endorser, ".1")
    if(!(omega2_col %in% colnames(omega2_post))) next
    
    for(i_mcmc in 1:nrow(delta_post)) {
      coef_value <- delta_post[i_mcmc, delta_cols]
      omega2_value <- omega2_post[i_mcmc, omega2_col]
      
      for(i_val in 1:length(values)) {
        effect_size <- values[i_val] * coef_value
        z_score <- effect_size / sqrt(omega2_value)
        combined_probs[i_val, i_mcmc, length(valid_endorser_idx) + 1] <- pnorm(z_score)
      }
    }
    valid_endorser_idx <- c(valid_endorser_idx, i_endorser)
  }
  
  # Average probabilities
  averaged_probs <- apply(combined_probs, c(1,2), mean, na.rm = TRUE)
  
  # Create result data frame
  result <- data.frame(
    value = values,
    category = factor(labels, levels = labels),
    q025 = apply(averaged_probs, 1, quantile, 0.025),
    q250 = apply(averaged_probs, 1, quantile, 0.25),
    median = apply(averaged_probs, 1, median),
    q750 = apply(averaged_probs, 1, quantile, 0.75),
    q975 = apply(averaged_probs, 1, quantile, 0.975)
  )
  
  return(result)
}

# Improved plotting function for box and whisker plots
plot_marginal_effects <- function(effects_data, covariate_name) {
  
  # Prepare plot data
  plot_data <- effects_data
  
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
      endorse_object, 
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
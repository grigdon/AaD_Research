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
                          hierarchical = FALSE
)

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

# Save to PDF
ggsave("~/projects/AaD_Research/output/plots/czechia/coef/czechia_coef_plot.pdf", plot, width = 12, height = 10)

#============================================
# 4. Marginal Effects Function
#============================================

# Define the covariates of interest
covariates_of_interest <- c("Male", "Age", "Education", "Capital", "IdeologyLR", "Income", "FamIncome", "DemPolGrievance", "PolicyPolGrievance",
                            "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "EconGrievanceProspMostFams",
                            "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine",
                            "NativeJobs", "NativeRights", "Religiosity", "VoteFarRight"
                            )

# Variable labels for publication
variable_labels <- c(
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

# Expanded covariates categorization
binary_covariates <- list(
  Male = list(
    values = c(1, 2),
    labels = c("Male", "Female")
  ),
  Capital = list(
    values = c(1, 2),
    labels = c("Not Capital", "Capital")
  ),
  VoteFarRight = list(
    values = c(0, 1),
    labels = c("No", "Yes")
  )
)

special_covariates <- list(
  Age = list(
    values = c(1, 2, 3, 4, 5),
    labels = c("Very Young", "Young", "Middle-Aged", "Older", "Elderly")
  ),
  Education = list(
    values = c(1, 2, 3),
    labels = c("Low", "Medium", "High")
  ),
  Income = list(
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

#============================================
# 5. Categorize Plots Function
#============================================

# Create categorized plots function for academic publication
create_categorized_plots <- function(plots_list, output_dir = "~/projects/AaD_Research/output/plots/czechia/marginal") {
  # Define categories
  categories <- list(
    "SES_Demographics" = c("Age", "Male", "Education", "Capital", "IdeologyLR", "Income", "FamIncome", "Religiosity"),
    "Political_Economic_Grievances" = c("DemPolGrievance", "PolicyPolGrievance", "EconGrievanceRetro", "EconGrievanceProspInd",
                                        "EconGrievanceProspAgg", "EconGrievanceProspMostFams"),
    "Nationalism" = c("NativeRights", "NativeJobs", "VoteFarRight"),
    "Boundary_Maintenance" = c("GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine")
  )
  
  # Category labels for plots
  category_labels <- c(
    "SES_Demographics" = "Socioeconomic and Demographic Factors",
    "Political_Economic_Grievances" = "Political and Economic Grievances",
    "Nationalism" = "Nationalism and National Identity",
    "Boundary_Maintenance" = "Group Boundary Maintenance"
  )
  
  # Create output directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Process each category
  for(cat_name in names(categories)) {
    # Get variables in this category
    cat_vars <- categories[[cat_name]]
    
    # Filter plots for this category
    cat_plots <- plots_list[names(plots_list) %in% cat_vars]
    cat_plots <- cat_plots[!sapply(cat_plots, is.null)]
    
    if(length(cat_plots) > 0) {
      # Determine layout based on number of plots
      n_plots <- length(cat_plots)
      if(n_plots <= 4) {
        n_col <- 2
        n_row <- ceiling(n_plots/2)
        width <- 8.5  # Standard journal width (inches)
        height <- 4 * n_row
      } else if(n_plots <= 6) {
        n_col <- 3
        n_row <- 2
        width <- 8.5
        height <- 6
      } else {
        n_col <- 3
        n_row <- ceiling(n_plots/3)
        width <- 8.5
        height <- 3 * n_row
      }
      
      # Create combined plot
      combined_plot <- ggarrange(
        plotlist = cat_plots, 
        ncol = n_col, 
        nrow = n_row,
        common.legend = TRUE,
        legend = "bottom"
      )
      
      # Add category title
      combined_plot <- annotate_figure(
        combined_plot,
        top = text_grob(
          category_labels[cat_name],
          face = "bold", size = 12, family = "Times"
        )
      )
      
      # Save combined plot
      output_path <- file.path(output_dir, paste0("FigureCategory_", cat_name, ".pdf"))
      ggsave(output_path, combined_plot, width = width, height = height, dpi = 300, units = "in")
      message(paste("Saved category figure to:", output_path))
      
      # Save individual plots with improved academic formatting
      for(var_name in names(cat_plots)) {
        # Apply academic formatting to individual plot
        academic_plot <- cat_plots[[var_name]] +
          theme(
            text = element_text(family = "Times"),
            axis.title = element_text(size = 11),
            axis.text = element_text(size = 10),
            plot.title = element_text(size = 12, hjust = 0.5),
            plot.subtitle = element_text(size = 10, hjust = 0.5),
            panel.grid.major.y = element_line(color = "gray90", size = 0.2),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank()
          )
        
        # Save individual academic plot
        ind_output_path <- file.path(output_dir, paste0("academic_", var_name, ".pdf"))
        ggsave(ind_output_path, academic_plot, width = 5, height = 4, dpi = 300, units = "in")
      }
    }
  }
}

#============================================
# 5. Plot Function
#============================================

# Improve the individual plot function for academic publication
plot_marginal_effects <- function(effects_data, covariate_name) {
  # Prepare plot data
  plot_data <- effects_data
  
  # Get formatted variable name for display
  var_label <- ifelse(
    covariate_name %in% names(variable_labels),
    variable_labels[covariate_name],
    gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", covariate_name)
  )
  
  # Set academic color palette
  box_color <- "#3c78d8"  # Professional blue for academic publishing
  
  # Create box and whisker plot with academic styling
  p <- ggplot(plot_data, aes(x = category, y = median)) +
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
      position = position_dodge(width = 0.8),
      fill = box_color,
      alpha = 0.8,
      color = "black"
    ) +
    labs(
      title = var_label,
      x = NULL,
      y = "Probability of Support"
    ) +
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = seq(0, 1, 0.2),
      labels = scales::percent_format(accuracy = 1)
    ) +
    theme_bw(base_size = 11) +
    theme(
      text = element_text(family = "Times"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      axis.text = element_text(color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "none",
      plot.margin = margin(10, 10, 10, 10)
    )
  
  return(p)
}

################################
# End of Functions
################################

############## Run the functions

successful_plots <- list()

for(cov in covariates_of_interest) {
  print(paste("processing covariate:", cov))
  effect_data <- calculate_marginal_effects(endorse_object, cov)
  print(paste("successfully provessed covariate:", cov))
  successful_plots[[cov]] <- plot_marginal_effects(effect_data, cov)
}

############## End run functions

output_dir <- "~/projects/AaD_Research/output/plots/czechia/marginal"

# To use this updated plotting approach, add this after your existing code:
if(length(successful_plots) > 0) {
  # Make sure ggpubr is loaded
  if(!require(ggpubr)) {
    install.packages("ggpubr")
    library(ggpubr)
  }
  
  # Create the categorized plots
  create_categorized_plots(successful_plots)
  
  # Create a comprehensive appendix figure if needed
  all_vars_output <- file.path(output_dir, "AppendixFigure_AllMarginalEffects.pdf")
  
  # For the appendix, arrange all plots in a grid
  all_plots <- ggarrange(
    plotlist = successful_plots,
    ncol = 4,
    nrow = ceiling(length(successful_plots)/4),
    common.legend = TRUE,
    legend = "bottom"
  )
  
  all_plots <- annotate_figure(
    all_plots,
    top = text_grob(
      "Appendix Figure 1: Marginal Effects of All Predictors on Militia Support",
      face = "bold", size = 14, family = "Times"
    ),
    bottom = text_grob(
      "Note: Boxes show 50% credible intervals; whiskers show 95% credible intervals.",
      hjust = 0, x = 0, size = 10, family = "Times", face = "italic"
    )
  )
  
  ggsave(all_vars_output, all_plots, width = 11, height = 14, dpi = 300, limitsize = FALSE)
  message(paste("Saved appendix figure to:", all_vars_output))
}
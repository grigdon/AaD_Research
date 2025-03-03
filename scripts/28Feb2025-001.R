#==========================================================#
# Improved Marginal Effects Functions -- Version_9         #
#==========================================================#

# Load necessary libraries
library(ggplot2)
library(ggpubr)

# Define the covariates of interest and binary covariates' info
covariates_of_interest <- c("age", "gender", "education", "is_capital", 
                            "ideology", "income", "DemPolGrievence", 
                            "PolicyPolGrievence", "NatPride", "NativeRights", 
                            "NativeJobs", "LawOrder", "Chauvinism")

# Updated binary and special covariates info
binary_covariates <- list(
  gender = list(
    values = c(0, 1),
    labels = c("Male", "Female")
  ),
  is_capital = list(
    values = c(0, 1),
    labels = c("Not Capital", "Capital")
  )
)

special_covariates <- list(
  age = list(
    values = c(1, 2, 3, 4, 5),
    labels = c("1", "2", "3", "4", "5")
  ),
  education = list(
    values = c(1, 2, 3),
    labels = c("1", "2", "3")
  )
)

# Default labels for standard continuous covariates
standard_labels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")

# Modified function to handle binary and continuous covariates
calculate_marginal_effects <- function(model_out, covariate_name, 
                                       values, labels) {
  
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
        z_score <- effect_size / sqrt(omega2_value)  # Correct calculation
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

# Improved plotting function with dynamic labels
plot_marginal_effects <- function(effects_data, covariate_name) {
  formatted_name <- gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", covariate_name)
  
  ggplot(effects_data, aes(x = category, y = median)) +
    geom_boxplot(
      aes(ymin = q025, lower = q250, middle = median, upper = q750, ymax = q975),
      stat = "identity",
      fill = "#4682B4"
    ) +
    labs(
      title = paste("Effect of", formatted_name, "on Militia Support"),
      x = formatted_name,
      y = "Probability of Support"
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generate plots with updated labels
plots_list <- list()
for(cov in covariates_of_interest) {
  # Determine which labels and values to use based on covariate type
  if(cov %in% names(binary_covariates)) {
    # For binary covariates (gender, is_capital)
    bin_info <- binary_covariates[[cov]]
    values <- bin_info$values
    labels <- bin_info$labels
  } else if(cov %in% names(special_covariates)) {
    # For special cases (age, education)
    special_info <- special_covariates[[cov]]
    values <- special_info$values
    labels <- special_info$labels
  } else {
    # For all other continuous covariates using agreement scale
    values <- c(-1.5, -0.5, 0.5, 1.5)
    labels <- standard_labels
  }
  
  effects <- tryCatch({
    calculate_marginal_effects(gaber_endorse, cov, values, labels)
  }, error = function(e) NULL)
  
  if(!is.null(effects)) {
    plots_list[[cov]] <- plot_marginal_effects(effects, cov)
  }
}

# Save plots to output directory
if(length(plots_list) > 0) {
  message(paste("Creating combined plots with", length(plots_list), "successful plots"))
  
  # Create output directory if it doesn't exist
  output_dir <- file.path(getwd(), "outputs_combined_boxplots")
  if(!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Save individual plots
  for(cov in names(plots_list)) {
    output_path <- file.path(output_dir, paste0("marginal_effect_", cov, ".pdf"))
    ggsave(output_path, plots_list[[cov]], width = 8, height = 6)
    message(paste("Saved individual plot to:", output_path))
  }
  
  # Create combined plots, 4 per page
  n_plots <- length(plots_list)
  plots_per_page <- 4
  n_pages <- ceiling(n_plots / plots_per_page)
  
  for(page in 1:n_pages) {
    start_idx <- (page-1) * plots_per_page + 1
    end_idx <- min(page * plots_per_page, n_plots)
    
    if(start_idx > n_plots) break
    
    page_plots <- plots_list[start_idx:end_idx]
    
    combined_plot <- ggarrange(
      plotlist = page_plots, 
      ncol = 2, 
      nrow = ceiling(length(page_plots)/2)
    )
    
    output_path <- file.path(output_dir, paste0("combined_marginal_effects_page", page, ".pdf"))
    ggsave(output_path, combined_plot, width = 16, height = 12)
    message(paste("Saved combined plot page", page, "to:", output_path))
  }
} else {
  warning("No successful plots were generated! Check the model structure and variable names.")
}
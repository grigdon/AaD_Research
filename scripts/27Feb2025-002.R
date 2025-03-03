#==========================================================#
# Improved Marginal Effects Functions -- Version_8         #
#==========================================================#

# Define the covariates of interest
covariates_of_interest <- c("age", "gender", "education", "is_capital", 
                            "ideology", "income", "DemPolGrievence", 
                            "PolicyPolGrievence", "NatPride", "NativeRights", 
                            "NativeJobs", "LawOrder", "Chauvinism")

# Modified function to calculate the marginal effects with categorical values
# and combine across all endorsement questions
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
  
  # Initialize a combined probability matrix that will hold values for all endorsers
  # Dimensions: values x MCMC samples x endorsers
  combined_probs <- array(NA, dim = c(length(values), nrow(delta_post), n_endorsers))
  
  # Index to track which endorsers we've actually processed
  valid_endorser_idx <- integer(0)
  
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
        
        # Calculate probability
        combined_probs[i_val, i_mcmc, length(valid_endorser_idx) + 1] <- pnorm(
          effect_size,
          mean = 0,
          sd = sqrt(omega2_value),
          lower.tail = TRUE
        )
      }
    }
    
    # Add this endorser to our valid list
    valid_endorser_idx <- c(valid_endorser_idx, i_endorser)
  }
  
  # Check if we have valid endorsers
  if(length(valid_endorser_idx) == 0) {
    stop("No valid endorsers processed")
  }
  
  # Trim the array to only include valid endorsers
  combined_probs <- combined_probs[, , 1:length(valid_endorser_idx), drop = FALSE]
  
  # Calculate the average probability across all endorsers for each MCMC sample
  # This gives us one probability distribution per categorical value
  averaged_probs <- matrix(NA, nrow = length(values), ncol = nrow(delta_post))
  for(i_val in 1:length(values)) {
    for(i_mcmc in 1:nrow(delta_post)) {
      averaged_probs[i_val, i_mcmc] <- mean(combined_probs[i_val, i_mcmc, ], na.rm = TRUE)
    }
  }
  
  # Create a data frame with the summary statistics
  result <- data.frame(
    value = values,
    category = factor(categorical_labels, levels = categorical_labels),
    q025 = apply(averaged_probs, 1, function(x) quantile(x, 0.025)),
    q250 = apply(averaged_probs, 1, function(x) quantile(x, 0.25)),
    median = apply(averaged_probs, 1, function(x) quantile(x, 0.5)),
    q750 = apply(averaged_probs, 1, function(x) quantile(x, 0.75)),
    q975 = apply(averaged_probs, 1, function(x) quantile(x, 0.975))
  )
  
  return(result)
}

# Improved plotting function for single box plot per covariate
plot_marginal_effects <- function(effects_data, covariate_name) {
  
  # Format covariate name for title
  formatted_name <- gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", covariate_name)
  
  # Create box and whisker plot
  p <- ggplot(effects_data, aes(x = category, y = median)) +
    geom_boxplot(
      aes(
        ymin = q025,
        lower = q250,
        middle = median,
        upper = q750,
        ymax = q975
      ),
      stat = "identity",
      width = 0.6,
      fill = "#4682B4"  # Using a nice blue color
    ) +
    labs(
      title = paste("Effect of", formatted_name, "on Militia Support"),
      subtitle = "Combined across all policy domains (95% confidence intervals)",
      x = formatted_name,
      y = "Probability of Support"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
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
      gaber_endorse, 
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
  output_dir <- file.path(getwd(), "outputs_combined_boxplots")
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
      nrow = ceiling(length(page_plots)/2)
    )
    
    output_path <- file.path(output_dir, paste0("combined_marginal_effects_page", page, ".pdf"))
    ggsave(output_path, combined_plot, width = 16, height = 12)
    message(paste("Saved combined plot page", page, "to:", output_path))
  }
} else {
  warning("No successful plots were generated! Check the model structure and variable names.")
}
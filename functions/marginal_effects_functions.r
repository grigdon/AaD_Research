#==========================================================#
# Marginal Effects  Functions -- Gabe Rigdon -- Version_6  #
#==========================================================#

# Define the covariates of interest

covariates_of_interest <- c("age", "gender", "education", "is_capital", 
                            "ideology", "income", "DemPolGrievence", 
                            "PolicyPolGrievence", "NatPride", "NativeRights", 
                            "NativeJobs", "LawOrder", "Chauvinism")

# As follows is a function to calculate the marginal effects of each covariate passed into the function
# The function accepts:
#                       - the output from the the endorse function
#                       - the covariate of interest, i.e., age, gender, etc., 
#                       - a numeric vector representing the standardized levels 

calculate_marginal_effects <- function(model_out, covariate_name, values = c(-2, -1, 0, 1, 2)) {
  
  # the model output contains three matrices: delta, lambda, and omega2
  #
  # delta_post: The matrix of regression coefficients (posterior samples) for all covariates.
  # lambda_post: A matrix of factor loadings.
  # omega2_post: A matrix of variance parameters associated with each endorsement question.
  
  delta_post <- model_out$delta
  lambda_post <- model_out$lambda
  omega2_post <- model_out$omega2
  
  # This function uses grep with a regex to find the columns in the delta matrix
  # that match the name of the covariate.
  #
  # If the name of the covariate is not found in the delta matrix, an error is printed.
  
  delta_cols <- grep(paste0("^", covariate_name, "$"), colnames(delta_post))
  if(length(delta_cols) == 0) {
    stop(paste("Covariate", covariate_name, "not found in coefficients."))
  }
  
  # This similarly uses grep with a regex to find all columns containing omega_2
  # in their names. These columns correspond to varience parameters for the endorse. params.
  
  omega2_cols <- colnames(omega2_post)[grep("omega2", colnames(omega2_post))]
  
  # This regex extracts only the numbers after the omega2 substring. 
  # This is used to find out how many unique endorsements questions there are.
  
  endorser_nums <- as.numeric(gsub("omega2\\.(\\d+)\\..*", "\\1", omega2_cols))
  n_endorsers <- length(unique(endorser_nums))
  
  # Debug information
  
  print(paste("Processing", n_endorsers, "endorsement questions"))
  print(paste("Endorser numbers found:", paste(sort(unique(endorser_nums)), collapse=", ")))
  
  # Initializes an empty list that will soon be used to store the results.
  
  results <- list()
  
  # Iterates over the total number of endorser questions in ascending order
  
  for(i_endorser in sort(unique(endorser_nums))) {
    
    # Get the proper omega2 column for this endorser
    
    omega2_col <- paste0("omega2.", i_endorser, ".1")
    
    # Debug information
    
    print(paste("Processing endorser", i_endorser, "using omega2 column:", omega2_col))
    
    # Prints warning if the omega2 endorser is not found. 
    
    if(!(omega2_col %in% colnames(omega2_post))) {
      warning(paste("Could not find omega2 for endorser", i_endorser))
      next
    }
    
    # Initialize matrix for probability results
    
    probs_matrix <- matrix(NA, nrow = length(values), ncol = nrow(delta_post))
    
    # For each posterior sample (each MCMC sample)
    
    for(i_mcmc in 1:nrow(delta_post)) {
      
      # Get coefficient value for this covariate
      
      coef_value <- delta_post[i_mcmc, delta_cols]
      
      # Get omega2 for this endorser
      
      omega2_value <- omega2_post[i_mcmc, omega2_col]
      
      # For each standardized value of the covariate
      
      for(i_val in 1:length(values)) {
        
        # Calculate the effect of changing the covariate value
        
        effect_size <- values[i_val] * coef_value
        
        # Calculate probability using the cumulative normal dist function, pnorm() 
        # with a probit link (binary mapping) with omega2 uncertainty.
        
        probs_matrix[i_val, i_mcmc] <- pnorm(
          effect_size,  # Effect size
          mean = 0,     # Reference point
          sd = sqrt(omega2_value),  # Uncertainty from model
          lower.tail = FALSE
        )
      }
    } # At this point, probs_matrix is filled with prob. estimates for each covariate
    # across all posterior samples.
    
    #################################################################################
    
    # Create data frame for this endorser with 2.5th and 97.5th percentile bounds
    # A label is then assigned indicating the question number
    
    results[[as.character(i_endorser)]] <- data.frame(
      value = values,
      q025 = apply(probs_matrix, 1, function(x) quantile(x, 0.025)),
      median = apply(probs_matrix, 1, function(x) quantile(x, 0.5)),
      q975 = apply(probs_matrix, 1, function(x) quantile(x, 0.975)),
      endorser = paste0("Q", i_endorser)
    )
  }
  
  # Combine results, and return the results.
  
  final_results <- do.call(rbind, results)
  return(final_results)
  
} # End of function.

#####################################################################################


# A tailored function for printing the marginal effects results matrices
# The function accepts:
#                       - the output from the the calculate_marginal_effects function
#                       - the covariate name of interest, i.e., age, gender, etc., 

plot_marginal_effects <- function(effects_data, covariate_name) {
  
  # Opportunity to create better labels. Note -> this could have been
  # performed in the function 'calculate_marginal_effects' as well.
  
  question_labels <- c(
    "Q1" = "Policy_Name_1",
    "Q2" = "Policy_Name_2", 
    "Q3" = "Policy_Name_3"
  )
  
  # Merge new question labels 
  
  plot_data <- effects_data
  plot_data$question_label <- question_labels[plot_data$endorser]
  
  # Set a color pallette
  
  my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c")
  
  # Generates a plot object, p, of the marginal effects. 
  
  p <- ggplot(plot_data, aes(x = value, y = median, group = endorser)) +
    geom_line(aes(color = endorser), linewidth = 1.2) +
    geom_ribbon(aes(ymin = q025, ymax = q975, fill = endorser), alpha = 0.15) +
    labs(title = paste("Effect of", gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", covariate_name), "on Militia Support"),
         subtitle = "Across three different policy domains",
         x = paste(gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", covariate_name), "(standardized)"),
         y = "Probability of Support") +
    scale_color_manual(values = my_colors, labels = question_labels) +
    scale_fill_manual(values = my_colors, labels = question_labels) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
    )
  return(p)
}

# Sets a list to be populated with the generated plots.
# Sets an empty char vector for the covariates for which plotting was successful.
plots_list <- list()
successful_covs <- character(0)

for(cov in covariates_of_interest) {
  message(paste("Processing covariate:", cov))
  
  # Try to calculate marginals effects with error handling
  # Will return null if an error is detected
  
  effects <- tryCatch({
    calculate_marginal_effects(gaber_endorse, cov)
  }, error = function(e) {
    message(paste("Error calculating effects for", cov, ":", e$message))
    return(NULL)
  })
  
  # If effects is not null (the marginal effects were successfully calculated),
  # then store the plot under successful_covs. If not, return null.
  
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

# Filter out plots that did not generate and create successful_plots from plots_list

successful_plots <- plots_list[!sapply(plots_list, is.null)]

# Now, save individual and combined plots to a folder called "outputs"

if(length(successful_plots) > 0) {
  message(paste("Creating combined plots with", length(successful_plots), "successful plots"))
  
  # Save individual plots
  
  for(cov in names(successful_plots)) {
    output_path <- file.path(getwd(), "outputs_varying_lambda", paste0("marginal_effect_", cov, ".pdf"))
    ggsave(output_path, successful_plots[[cov]], width = 8, height = 6)
    message(paste("Saved individual plot to:", output_path))
  }
  
  # Split plots into groups of 4 for multiple panels
  
  n_plots <- length(successful_plots)
  plots_per_page <- 4
  n_pages <- ceiling(n_plots / plots_per_page)
  
  for(page in 1:n_pages) {
    start_idx <- (page-1) * plots_per_page + 1
    end_idx <- min(page * plots_per_page, n_plots)
    
    # Make sure we don't go beyond the available plots
    
    if(start_idx > n_plots) break
    
    page_plots <- successful_plots[start_idx:end_idx]
    
    combined_plot <- ggarrange(plotlist = page_plots, 
                               ncol = 2, 
                               nrow = ceiling(length(page_plots)/2))
    
    output_path <- file.path(getwd(), "outputs_varying_lambda", paste0("marginal_effects_combined_page", page, ".pdf"))
    ggsave(output_path, combined_plot, 
           width = 16, height = 12)
    message(paste("Saved combined plot page", page, "to:", output_path))
  }
} else {
  warning("No successful plots were generated! Check the model structure and variable names.")
}
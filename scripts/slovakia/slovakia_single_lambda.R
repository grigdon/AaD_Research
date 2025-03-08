# Function to calculate support probabilities for different values of an ordinal covariate
calculate_support_probabilities <- function(
    model_data,             # Your dataset containing all variables
    Y_list,                 # List of policy questions in the format required by endorse()
    covariate_name,         # Name of your ordinal covariate (e.g., "DemPolGrievance")
    covariate_values = NULL, # Optional: specific values to analyze (defaults to all unique values)
    other_covariates = NULL, # Optional: other covariates to include in the model
    group_names = NULL,     # Optional: names of political groups for nicer output
    control_baselines = FALSE, # Whether to include control condition baselines
    plot_results = TRUE     # Whether to generate plots
) {
  # Check if endorse package is installed
  if (!requireNamespace("endorse", quietly = TRUE)) {
    stop("The 'endorse' package is needed. Please install it with install.packages('endorse')")
  }
  
  # Get unique values of covariate if not provided
  if (is.null(covariate_values)) {
    covariate_values <- sort(unique(model_data[[covariate_name]]))
  }
  
  # Create formula for model
  if (is.null(other_covariates)) {
    model_formula <- as.formula(paste0("~", covariate_name))
  } else {
    covariates_string <- paste(c(covariate_name, other_covariates), collapse = " + ")
    model_formula <- as.formula(paste0("~", covariates_string))
  }
  
  # Fit the endorsement model
  cat("Fitting endorsement model...\n")
  model <- endorse::endorse(
    Y = Y_list,
    data = model_data,
    covariates = TRUE,
    formula.indiv = model_formula
  )
  cat("Model fitting complete!\n")
  
  # Create new data for predictions
  # For each covariate value, create a row with mean values for other covariates
  new_data <- data.frame(matrix(ncol = length(c(covariate_name, other_covariates)), 
                                nrow = length(covariate_values)))
  names(new_data) <- c(covariate_name, other_covariates)
  
  # Set the target covariate values
  new_data[[covariate_name]] <- covariate_values
  
  # Set other covariates to their means
  if (!is.null(other_covariates)) {
    for (cov in other_covariates) {
      if (is.factor(model_data[[cov]])) {
        # For factor variables, use most common level
        new_data[[cov]] <- factor(rep(names(sort(table(model_data[[cov]]), decreasing = TRUE)[1]), 
                                      length(covariate_values)),
                                  levels = levels(model_data[[cov]]))
      } else {
        # For numeric variables, use mean
        new_data[[cov]] <- rep(mean(model_data[[cov]], na.rm = TRUE), length(covariate_values))
      }
    }
  }
  
  # Generate predictions
  cat("Generating predictions...\n")
  predictions <- predict(model, newdata = new_data, type = "prob.support")
  
  # Extract and format results
  results <- as.data.frame(summary(predictions)$statistics[, "Mean"])
  
  # Get the number of groups (excluding control if needed)
  n_groups <- ncol(results) / length(Y_list)
  
  # Create meaningful names for groups if not provided
  if (is.null(group_names)) {
    if (control_baselines) {
      group_names <- paste0("Group", 0:(n_groups-1))
    } else {
      group_names <- paste0("Group", 1:n_groups)
    }
  }
  
  # Reshape results for better presentation
  formatted_results <- data.frame(
    Covariate_Value = covariate_values
  )
  
  # Add columns for each group and policy
  policy_names <- names(Y_list)
  
  for (p_idx in 1:length(policy_names)) {
    policy <- policy_names[p_idx]
    
    start_idx <- (p_idx - 1) * n_groups + 1
    if (!control_baselines) start_idx <- start_idx + 1  # Skip control
    
    end_idx <- if(control_baselines) p_idx * n_groups else p_idx * n_groups - 1
    
    group_results <- results[, start_idx:end_idx]
    
    if (ncol(group_results) == 1) {
      # If only one group, no need for column names
      formatted_results[[paste0(policy)]] <- group_results
    } else {
      # For multiple groups, add a column for each
      group_idx_offset <- if(control_baselines) 0 else 1
      for (g_idx in 1:ncol(group_results)) {
        g_name <- group_names[g_idx + group_idx_offset]
        formatted_results[[paste0(policy, "_", g_name)]] <- group_results[, g_idx]
      }
    }
  }
  
  # Generate plots if requested
  if (plot_results) {
    cat("Generating plots...\n")
    # Check if ggplot2 is installed
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("The 'ggplot2' package is needed for plotting. Install with install.packages('ggplot2')")
    } else {
      # We'll create a plot for each policy
      plot_list <- list()
      
      for (p_idx in 1:length(policy_names)) {
        policy <- policy_names[p_idx]
        
        # Prepare data for plotting
        plot_data <- data.frame(
          Covariate_Value = rep(covariate_values, n_groups - (!control_baselines)),
          Group = factor(rep(group_names[if(control_baselines) 1:n_groups else 2:n_groups], 
                             each = length(covariate_values))),
          Probability = numeric(length(covariate_values) * (n_groups - (!control_baselines)))
        )
        
        # Fill in probability values
        for (g_idx in 1:(n_groups - (!control_baselines))) {
          g_offset <- if(control_baselines) g_idx else g_idx + 1
          g_name <- group_names[g_offset]
          col_name <- paste0(policy, if(ncol(group_results) > 1) paste0("_", g_name) else "")
          plot_data$Probability[(g_idx-1)*length(covariate_values) + 1:(length(covariate_values))] <- 
            formatted_results[[col_name]]
        }
        
        # Create the plot
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Covariate_Value, y = Probability, 
                                                     color = Group, group = Group)) +
          ggplot2::geom_line(size = 1) +
          ggplot2::geom_point(size = 3) +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            title = paste0("Probability of Support by ", covariate_name, " Value"),
            subtitle = paste0("Policy: ", policy),
            x = covariate_name,
            y = "Probability of Support"
          ) +
          ggplot2::scale_y_continuous(limits = c(0, 1))
        
        plot_list[[policy]] <- p
        print(p)
      }
      
      # Return plot list as part of results
      formatted_results$plots <- plot_list
    }
  }
  
  cat("Analysis complete!\n")
  return(list(
    model = model,
    predictions = predictions,
    results = formatted_results
  ))
}

calculate_support_probabilities(data_slvk, Y, "DemPolGrievance")
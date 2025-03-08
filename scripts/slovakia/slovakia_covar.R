# Function to analyze support by DemPolGrievance for all questions and plot them side by side
analyze_all_questions_by_grievance <- function(endorse_object, data_frame, num_questions = 3) {
  # Set up the plotting area for multiple plots
  par(mfrow = c(1, num_questions), mar = c(4, 4, 3, 1))
  
  # Store results for all questions
  all_results <- list()
  
  for(question_idx in 1:num_questions) {
    # Get posterior samples of lambda coefficients
    lambda_samples <- endorse_object$lambda
    
    # Find column names for this question
    dem_grievance_col <- paste0("DemPolGrievance.", question_idx, ".1")
    q_dem_gr_idx <- which(colnames(lambda_samples) == dem_grievance_col)
    
    if(length(q_dem_gr_idx) == 0) {
      warning(paste0("Could not find DemPolGrievance column for question ", question_idx, ": ", dem_grievance_col))
      next
    }
    
    # Get intercept index for this question
    intercept_col <- paste0("(Intercept).", question_idx, ".1")
    intercept_idx <- which(colnames(lambda_samples) == intercept_col)
    
    if(length(intercept_idx) == 0) {
      warning(paste0("Could not find intercept column for question ", question_idx, ": ", intercept_col))
      next
    }
    
    # Get omega (variance) parameter for this question/group
    if("omega2" %in% names(endorse_object)) {
      omega_samples <- sqrt(endorse_object$omega2[, question_idx])
    } else {
      # If omega2 isn't available, use a default standardization
      omega_samples <- rep(1, nrow(lambda_samples))
    }
    
    # Calculate support probabilities for each DemPolGrievance level
    grievance_levels <- 1:4
    results <- matrix(NA, nrow = length(grievance_levels), ncol = 3)
    colnames(results) <- c("DemPolGrievance", "Mean_Prob", "SD_Prob")
    results[,"DemPolGrievance"] <- grievance_levels
    
    for(i in seq_along(grievance_levels)) {
      level <- grievance_levels[i]
      
      # For each MCMC iteration, calculate Pr(s > 0) = Φ(λ/ω)
      intercept_values <- lambda_samples[, intercept_idx]
      grievance_values <- lambda_samples[, q_dem_gr_idx]
      
      lambda_effect <- intercept_values + level * grievance_values
      probs <- pnorm(lambda_effect / omega_samples)
      
      # Store mean and SD
      results[i, "Mean_Prob"] <- mean(probs)
      results[i, "SD_Prob"] <- sd(probs)
    }
    
    # Convert to data frame
    results_df <- as.data.frame(results)
    all_results[[question_idx]] <- results_df
    
    # Plot the results
    plot(
      x = results_df$DemPolGrievance,
      y = results_df$Mean_Prob,
      type = "b",
      xlab = "Democratic Political Grievance Level",
      ylab = if(question_idx == 1) "Predicted Probability of Support" else "",
      main = paste("Question", question_idx),
      ylim = c(0, 1),
      xaxt = "n",
      pch = 16,
      col = "blue"
    )
    axis(1, at = 1:4, labels = 1:4)
    
    # Add confidence intervals
    arrows(
      x0 = results_df$DemPolGrievance,
      y0 = pmax(0, results_df$Mean_Prob - 1.96 * results_df$SD_Prob),
      x1 = results_df$DemPolGrievance,
      y1 = pmin(1, results_df$Mean_Prob + 1.96 * results_df$SD_Prob),
      length = 0.05, angle = 90, code = 3,
      col = "blue"
    )
    
    # Add grid lines for better readability
    grid()
  }
  
  # Reset par settings
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  
  # Return all results
  return(all_results)
}


# Basic version with three plots side by side
all_results <- analyze_all_questions_by_grievance(endorse_object, data_slvk)
# Function to analyze support by any covariate for all questions and save as PDF
analyze_all_questions_by_covariate <- function(endorse_object, data_frame, covariate, 
                                               num_questions = 3, output_dir = "plots") {
  # Create output directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Validate covariate exists
  if(!(covariate %in% names(data_frame))) {
    stop(paste("Covariate", covariate, "not found in the data frame"))
  }
  
  # Determine variable type
  is_numeric <- is.numeric(data_frame[[covariate]])
  
  # Create combined results dataframe
  combined_results <- data.frame()
  
  # Process each question
  for(question_idx in 1:num_questions) {
    # Get posterior samples
    lambda_samples <- endorse_object$lambda
    
    # Find parameter columns
    covariate_col <- paste0(covariate, ".", question_idx, ".1")
    intercept_col <- paste0("(Intercept).", question_idx, ".1")
    
    # Skip if missing columns
    if(!covariate_col %in% colnames(lambda_samples)) next
    if(!intercept_col %in% colnames(lambda_samples)) next
    
    # Get variance parameter
    if("omega2" %in% names(endorse_object)) {
      omega_samples <- sqrt(endorse_object$omega2[, question_idx])
    } else {
      omega_samples <- rep(1, nrow(lambda_samples))
    }
    
    # Calculate covariate levels
    if(is_numeric) {
      cov_levels <- quantile(data_frame[[covariate]], probs = seq(0, 1, 0.25), na.rm = TRUE)
      cov_levels <- unique(round(cov_levels, 2))
    } else {
      cov_levels <- sort(unique(data_frame[[covariate]]))
    }
    
    # Calculate probabilities
    results <- matrix(NA, nrow = length(cov_levels), ncol = 3)
    colnames(results) <- c(covariate, "Mean_Prob", "SD_Prob")
    results[,covariate] <- cov_levels
    
    for(i in seq_along(cov_levels)) {
      level <- cov_levels[i]
      intercept <- lambda_samples[, intercept_col]
      effect <- lambda_samples[, covariate_col]
      lambda <- intercept + level * effect
      probs <- pnorm(lambda / omega_samples)
      results[i, "Mean_Prob"] <- mean(probs)
      results[i, "SD_Prob"] <- sd(probs)
    }
    
    # Store results
    results_df <- as.data.frame(results)
    results_df$Question <- paste("Question", question_idx)
    combined_results <- rbind(combined_results, results_df)
  }
  
  # Create custom theme
  custom_theme <- function() {
    theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = if(is_numeric) 0 else 45, hjust = 1),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 12, hjust = 0),
        panel.spacing = unit(1.5, "lines"),
        legend.position = "none"
      )
  }
  
  # Create plot
  p <- ggplot(combined_results, aes(x = .data[[covariate]], y = Mean_Prob)) +
    geom_point(size = 2.5, color = "#0072B2") +
    geom_errorbar(
      aes(ymin = pmax(0, Mean_Prob - 1.96*SD_Prob),
          ymax = pmin(1, Mean_Prob + 1.96*SD_Prob)),
      width = if(is_numeric) 0.05 else 0.2,
      color = "#0072B2",
      linewidth = 0.8
    ) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkred", alpha = 0.7) +
    facet_wrap(~Question, nrow = 1) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      title = paste("Support by", covariate),
      x = if(is_numeric) "Covariate Value" else "Category",
      y = "Predicted Probability of Support"
    ) +
    custom_theme()
  
  # Handle discrete variables
  if(!is_numeric) {
    p <- p + 
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
      theme(axis.text.x = element_text(size = 10))
  } else {
    p <- p +
      scale_x_continuous(n.breaks = 6)
  }
  
  # Save plot
  ggsave(
    file.path(output_dir, paste0("support_by_", covariate, ".pdf")), 
    plot = p,
    width = 12,
    height = 6,
    device = cairo_pdf
  )
  
  return(combined_results)
}

# Wrapper function to process all covariates
analyze_all_covariates <- function(endorse_object, data_frame, covariates, 
                                   num_questions = 3, output_dir = "plots") {
  all_results <- list()
  
  for(covariate in covariates) {
    tryCatch({
      results <- analyze_all_questions_by_covariate(
        endorse_object,
        data_frame,
        covariate,
        num_questions,
        output_dir
      )
      all_results[[covariate]] <- results
    }, error = function(e) {
      message("Error processing ", covariate, ": ", conditionMessage(e))
    })
  }
  
  return(all_results)
}


# Define the covariates of interest
covariates_of_interest <- c("age", "male", "educ", "capital", "ideology", "income", "FAMincome", 
                            "DemPolGrievance", "PolicyPolGrievance", "EconGrievenceRetro", 
                            "EconGrievenceProspInd", "EconGrievenceProspAgg", "NatPride", "NativeRights",
                            "NativeJobs", "DemonstrateNational", "SlovakNationality", "Nationalist", 
                            "VoteFarRight", "LawOrder", "MaleChauvinism", "ChristianSchool", "DemonstrateTrad",
                            "Religiosity", "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine"
)


all_covariate_results <- analyze_all_covariates(
  endorse_object,
  data_slvk,
  covariates_of_interest,
  num_questions = 3,
  output_dir = "~/projects/AaD_Research/output/plots/slovakia/test"
)



# Calculates the probability of support for each ordinal value of a covariate

analyze_support_boxplots <- function(endorse_object, data_frame, covariates, 
                                     num_questions = 3, output_dir = "plots",
                                     questions_titles = NULL) {
  
  # Set default question titles if not provided
  
  if(is.null(questions_titles)) {
    questions_titles <- paste("Question", 1:num_questions)
  }
  
  cat("Output files will be saved to:", normalizePath(output_dir), "\n")
  
  # Create a list to store all results
  
  all_results <- list()
  
  # Process each covariate
  
  for(covariate in covariates) {
    cat("\nProcessing", covariate, "...\n")
    
    # Skip if covariate not found in data frame
    
    if(!(covariate %in% names(data_frame))) {
      cat("Covariate", covariate, "not found in the data frame. Skipping.\n")
      next
    }
    
    # Format covariate name for plot labels
    
    formatted_covariate <- gsub("([a-z])([A-Z])", "\\1 \\2", covariate)
    formatted_covariate <- gsub("_", " ", formatted_covariate)
    formatted_covariate <- gsub("([a-z])([0-9])", "\\1 \\2", formatted_covariate)
    formatted_covariate <- paste0(toupper(substr(formatted_covariate, 1, 1)), 
                                  substr(formatted_covariate, 2, nchar(formatted_covariate)))
    
    
    # Sort Covariates
    
    plot_covariate <- covariate
    covariate_levels <- sort(unique(data_frame[[covariate]]))
    
    # Create PDF file for each covariate
    
    pdf_file <- file.path(output_dir, paste0("support_by_", covariate, "_boxplot.pdf"))
    cat("Creating PDF file:", pdf_file, "\n")
   
    pdf(pdf_file, width = 10, height = 6, family = "Helvetica")
    
    # Set up the plotting area for multiple plots
    
    layout(matrix(1:num_questions, nrow = 1), widths = c(1.2, rep(1, num_questions-1)))
    
    par(mar = c(5, 5, 4, 2), mgp = c(3, 1, 0), 
        cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.3, 
        lwd = 1.5, family = "Helvetica")
    
    if(requireNamespace("RColorBrewer", quietly = TRUE)) {
      palette <- RColorBrewer::brewer.pal(9, "Blues")[5:9]
      box_fill <- palette[3]
      box_outline <- palette[5]
      whisker_color <- palette[5]
    } else {
      box_fill <- "lightsteelblue"
      box_outline <- "steelblue"
      whisker_color <- "steelblue"
    }
    
    # Store results for this covariate
    covariate_results <- list()
    
    # Process posterior samples for all questions
    for(question_idx in 1:num_questions) {
      # Get posterior samples of lambda coefficients
      lambda_samples <- endorse_object$lambda
      
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
      
      # Create a data frame to store posterior probabilities by covariate level
      boxplot_data <- list()
      
      # Extract and organize posterior probabilities by covariate level
      if(is_numeric) {
        # For numeric covariates, need to compute effect for each category
        # Find column for this covariate
        covariate_col <- paste0(covariate, ".", question_idx, ".1")
        q_covar_idx <- which(colnames(lambda_samples) == covariate_col)
        
        if(length(q_covar_idx) == 0) {
          warning(paste0("Could not find ", covariate, " column for question ", question_idx, ": ", covariate_col))
          next
        }
        
        # Get coefficients
        intercept_values <- lambda_samples[, intercept_idx]
        covariate_values <- lambda_samples[, q_covar_idx]
        
        # For each category, calculate posterior probabilities
        for(i in seq_along(covariate_levels)) {
          # Use midpoint of category as representative value
          if(i == 1) {
            cat_value <- (breaks[1] + breaks[2]) / 2
          } else if(i == length(covariate_levels)) {
            cat_value <- (breaks[i] + breaks[i+1]) / 2
          } else {
            cat_value <- (breaks[i] + breaks[i+1]) / 2
          }
          
          # Calculate linear predictor and transform to probability
          lambda_effect <- intercept_values + cat_value * covariate_values
          probs <- pnorm(lambda_effect / omega_samples)
          
          # Store probabilities for this category
          boxplot_data[[i]] <- probs
        }
      } else {
        # For categorical covariates, need to handle dummy variables
        # For each level, find the corresponding coefficient
        for(i in seq_along(covariate_levels)) {
          level <- covariate_levels[i]
          
          # Get base intercept
          lambda_effect <- lambda_samples[, intercept_idx]
          
          # If not reference level, add the level effect
          if(i > 1) {
            # Construct column name for this level
            if(grepl(" ", level)) {
              # If level contains spaces, it might use different formatting
              level_col_options <- c(
                paste0(covariate, level, ".", question_idx, ".1"),
                paste0(covariate, gsub(" ", "", level), ".", question_idx, ".1"),
                paste0(covariate, ".", level, ".", question_idx, ".1")
              )
              
              # Try to find the right column
              found <- FALSE
              for(col_name in level_col_options) {
                level_idx <- which(colnames(lambda_samples) == col_name)
                if(length(level_idx) > 0) {
                  lambda_effect <- lambda_effect + lambda_samples[, level_idx]
                  found <- TRUE
                  break
                }
              }
              
              if(!found) {
                warning(paste0("Could not find column for ", covariate, " level ", level, " for question ", question_idx))
                # Use intercept only as fallback
              }
            } else {
              # Standard format for level column
              level_col <- paste0(covariate, level, ".", question_idx, ".1")
              level_idx <- which(colnames(lambda_samples) == level_col)
              
              if(length(level_idx) > 0) {
                lambda_effect <- lambda_effect + lambda_samples[, level_idx]
              } else {
                warning(paste0("Could not find column for ", covariate, " level ", level, " for question ", question_idx))
                # Use intercept only as fallback
              }
            }
          }
          
          # Calculate probabilities
          probs <- pnorm(lambda_effect / omega_samples)
          
          # Store probabilities for this level
          boxplot_data[[i]] <- probs
        }
      }
      
      # Create the box plot if we have data
      if(length(boxplot_data) > 0) {
        # Create names for the box plot
        names(boxplot_data) <- covariate_levels
        
        # Create the box plot
        boxplot(boxplot_data,
                main = questions_titles[question_idx],
                xlab = ifelse(question_idx == 1 || question_idx == 2, paste(formatted_covariate), ""),
                ylab = ifelse(question_idx == 1, "Predicted Probability of Support", ""),
                ylim = c(0, 1),
                col = box_fill,
                border = box_outline,
                whiskcol = whisker_color,
                staplelwd = 0,  # No staples for cleaner look
                outpch = 16,    # Solid dots for outliers
                outcol = adjustcolor(box_outline, alpha.f = 0.5),
                outcex = 0.8,   # Smaller outlier points
                medlwd = 2,     # Thicker median line
                boxlwd = 1.5,   # Thicker box lines
                whisklty = 1,   # Solid whisker lines
                las = 2)        # Rotate axis labels if needed
        
        # Add a reference line at 0.5
        abline(h = 0.5, lty = 2, col = "darkgray", lwd = 1)
        
        # Add light grid behind the boxes
        grid(lty = "dotted", col = "gray90", nx = NA)
        
        # Add box around the plot for a crisp finish
        box(lwd = 1.5)
        
        # Store summary statistics
        summaries <- lapply(boxplot_data, function(x) {
          c(mean = mean(x), 
            sd = sd(x),
            median = median(x),
            q25 = quantile(x, 0.25),
            q75 = quantile(x, 0.75))
        })
        
        covariate_results[[question_idx]] <- summaries
      }
    }
    
    # Add overall title at the top
    mtext(paste("Effect of", formatted_covariate, "on Support"), 
          outer = TRUE, line = -1.5, cex = 1.5, font = 2)
    
    # Explicitly close PDF device
    cat("Closing PDF device for:", covariate, "\n")
    dev.off()
    
    # Verify file was created
    if(file.exists(pdf_file)) {
      cat("Successfully created PDF file:", pdf_file, "\n")
    } else {
      warning("Failed to create PDF file:", pdf_file)
    }
    
    # Add results to overall results list
    all_results[[covariate]] <- covariate_results
  }
  
  # Reset par settings
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  
  # Final check of output files
  cat("\nVerifying all output files:\n")
  expected_files <- file.path(output_dir, paste0("support_by_", covariates, "_boxplot.pdf"))
  existing_files <- expected_files[file.exists(expected_files)]
  missing_files <- expected_files[!file.exists(expected_files)]
  
  cat("Created", length(existing_files), "out of", length(expected_files), "expected PDF files\n")
  
  if(length(missing_files) > 0) {
    cat("Missing files:\n")
    cat(paste(" -", basename(missing_files)), sep = "\n")
  }
  
  # Return all results
  return(all_results)
}
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

#comment
#comment

omega_matrix <- as.data.frame(endorse_object$omega2)
lambda_matrix <- as.data.frame(endorse_object$lambda)
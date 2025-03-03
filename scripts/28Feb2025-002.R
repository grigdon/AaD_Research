#==========================================================#
# Research Paper-Quality Marginal Effects -- Version_10    #
#==========================================================#

# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(Cairo)  # For high-quality PDF output

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

# Variable name mapping for publication-quality presentation
variable_labels <- c(
  age = "Age",
  gender = "Gender",
  education = "Education Level",
  is_capital = "Capital City Resident",
  ideology = "Ideology",
  income = "Income Level",
  DemPolGrievence = "Democratic Grievance",
  PolicyPolGrievence = "Policy Grievance",
  NatPride = "National Pride",
  NativeRights = "Native Rights",
  NativeJobs = "Native Jobs Protection",
  LawOrder = "Law and Order",
  Chauvinism = "Nationalism"
)

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

# Publication-quality plotting function
plot_marginal_effects <- function(effects_data, covariate_name) {
  # Get formatted variable name for publication
  var_label <- ifelse(
    covariate_name %in% names(variable_labels),
    variable_labels[covariate_name],
    gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", covariate_name)
  )
  
  # Set plot theme for publication
  publication_theme <- theme_minimal() +
    theme(
      text = element_text(family = "serif", size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      panel.border = element_rect(color = "gray80", fill = NA, size = 0.5)
    )
  
  # Create publication-quality plot
  ggplot(effects_data, aes(x = category, y = median)) +
    # Add ribbon for 95% CI
    geom_ribbon(aes(ymin = q025, ymax = q975), fill = "gray80", alpha = 0.3) +
    # Add line and point for median
    geom_line(group = 1, color = "gray50", linetype = "dashed", size = 0.5) +
    geom_point(size = 3, color = "#4682B4") +
    # Add error bars for 50% CI
    geom_errorbar(aes(ymin = q250, ymax = q750), width = 0.2, color = "#4682B4", size = 0.8) +
    # Labels
    labs(
      title = paste("Effect of", var_label, "on Support for Militia"),
      x = var_label,
      y = "Predicted Probability of Support"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    publication_theme
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

# Save plots to output directory with publication-quality settings
if(length(plots_list) > 0) {
  message(paste("Creating publication-quality plots with", length(plots_list), "variables"))
  
  # Create output directory if it doesn't exist
  output_dir <- file.path(getwd(), "publication_plots")
  if(!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Cairo PDF settings for high-quality publication output
  cairo_pdf_settings <- list(
    width = 8,
    height = 6,
    family = "serif",
    pointsize = 12,
    fallback_resolution = 300
  )
  
  # Save individual plots with consistent formatting
  for(cov in names(plots_list)) {
    output_path <- file.path(output_dir, paste0("marginal_effect_", cov, ".pdf"))
    ggsave(
      output_path, 
      plots_list[[cov]], 
      width = cairo_pdf_settings$width, 
      height = cairo_pdf_settings$height,
      device = cairo_pdf,
      dpi = 300
    )
    message(paste("Saved publication plot to:", output_path))
  }
  
  # Create combined plots for figures in paper
  n_plots <- length(plots_list)
  plots_per_page <- 4
  n_pages <- ceiling(n_plots / plots_per_page)
  
  for(page in 1:n_pages) {
    start_idx <- (page-1) * plots_per_page + 1
    end_idx <- min(page * plots_per_page, n_plots)
    
    if(start_idx > n_plots) break
    
    page_plots <- plots_list[start_idx:end_idx]
    
    # Add annotations for publication figure
    combined_plot <- ggarrange(
      plotlist = page_plots, 
      ncol = 2, 
      nrow = ceiling(length(page_plots)/2),
      common.legend = TRUE,
      legend = "bottom",
      labels = LETTERS[1:length(page_plots)],  # Add panel labels A, B, C, D
      font.label = list(size = 14, face = "bold")
    )
    
    # Add figure title for publication
    combined_plot <- annotate_figure(
      combined_plot,
      top = text_grob(
        paste("Figure", page, ": Marginal Effects on Support for Militia"),
        face = "bold", size = 16, family = "serif"
      ),
      bottom = text_grob(
        "Note: Points represent median predicted probabilities with 50% (thick bars) and 95% (shaded areas) credible intervals.",
        hjust = 0, x = 0, size = 10, family = "serif", face = "italic"
      )
    )
    
    # Save combined figure for publication
    output_path <- file.path(output_dir, paste0("Figure", page, "_MarginalEffects.pdf"))
    ggsave(
      output_path, 
      combined_plot, 
      width = 10, 
      height = 8,
      device = cairo_pdf,
      dpi = 300
    )
    message(paste("Saved publication figure", page, "to:", output_path))
  }
  
  # Create a single comprehensive figure for all variables
  if(n_plots > 6) {
    # For many variables, arrange in a compact grid
    comprehensive_plot <- ggarrange(
      plotlist = plots_list,
      ncol = 3,
      nrow = ceiling(n_plots/3),
      common.legend = TRUE,
      legend = "bottom",
      labels = LETTERS[1:length(plots_list)],
      font.label = list(size = 12, face = "bold")
    )
    
    # Add figure caption for publication
    comprehensive_plot <- annotate_figure(
      comprehensive_plot,
      top = text_grob(
        "Marginal Effects of All Variables on Support for Militia",
        face = "bold", size = 16, family = "serif"
      ),
      bottom = text_grob(
        "Note: Each panel shows the predicted probability of militia support. Points represent median probabilities with 50% (thick bars) and 95% (shaded areas) credible intervals.",
        hjust = 0, x = 0, size = 10, family = "serif", face = "italic"
      )
    )
    
    # Save comprehensive figure
    output_path <- file.path(output_dir, "Figure_AllMarginalEffects.pdf")
    ggsave(
      output_path,
      comprehensive_plot,
      width = 16,
      height = 14,
      device = cairo_pdf,
      dpi = 300,
      limitsize = FALSE
    )
    message(paste("Saved comprehensive figure to:", output_path))
  }
} else {
  warning("No successful plots were generated! Check the model structure and variable names.")
}
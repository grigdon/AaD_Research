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

# Create endorse object for slovakia

SKdata <- read_sav("~/projects/AaD_Research/datasets/scrubbed_datasets/slovakia_scrubbed.sav")

# Define questions for the endorsement experiment
questions <- c("id", "q10a_control_reversed", "q10b_control_reversed", "q10c_control_reversed", 
               "q10a_experiment_reversed", "q10b_experiment_reversed", "q10c_experiment_reversed")

# Select relevant columns for endorsement analysis
data_slvk_questions <- SKdata[questions]

# Define vars to keep 

vars <- c("id", "age", "male", "educ", "capital", "ideology", "income", "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg", "NativeRights", "NativeJobs",
          "DemonstrateNational", "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine", "VoteFarRight",
          "FAMincome", "Religiosity"  
)

# Subset and recode variables
data_slvk_vars <- SKdata[vars]

# Convert all variables to numeric
data_slvk_vars <- mutate(data_slvk_vars, across(everything(), ~as.numeric(.)))

# Merge the questions and standardized variables datasets
data_slvk <- left_join(data_slvk_questions, data_slvk_vars, by = "id")

# Create named list for response questions
Y <- list(Q1 = c("q10a_control_reversed", "q10a_experiment_reversed"), 
          Q2 = c("q10b_control_reversed", "q10b_experiment_reversed"), 
          Q3 = c("q10c_control_reversed", "q10c_experiment_reversed"))


# Creating an endorse object, excluding all covariates that are in the set { traditionalism }

slovakia_endorse_object <- endorse(Y = Y, 
                          data = data_slvk,
                          identical.lambda = FALSE,
                          covariates = FALSE,
                          prop = 0.010,
                          omega2.out = TRUE,
                          hierarchical = FALSE
)

# removes everything but the endorse object

rm(list = setdiff(ls(), c("slovakia_endorse_object", "poland_endorse_object", "czechia_endorse_object", "hungary_endorse_obect")))


# Create endorse object for poland


PLData <- read_sav("~/projects/AaD_Research/datasets/scrubbed_datasets/poland_scrubbed.sav")

# Define questions for the endorsement experiment
questions <- c("id", "Q10A_control_reversed", "Q10B_control_reversed", "Q10C_control_reversed", 
               "Q10D_experiment_reversed", "Q10E_experiment_reversed", "Q10F_experiment_reversed")

# Select relevant columns for endorsement analysis
data_pol_questions <- PLData[questions]

# Define variables to keep
vars <- c("id", "Male", "Age", "Education", "Capital", "IdeologyLR", "Income",  "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "GayNeighbor", "GayFamily", "ForNeighbor", 
          "ForPartner", "Ukraine", "NativeJobs", "NativeRights", "DemonstrateNational", "Religiosity", "VoteFarRight")

# Subset and recode variables
data_pol_vars <- PLData[vars]

# Convert all variables to numeric
data_pol_vars <- mutate(data_pol_vars, across(everything(), ~as.numeric(.)))

# Merge the questions and standardized variables datasets
data_pol <- left_join(data_pol_questions, data_pol_vars, by = "id")

# Create named list for response questions
Y <- list(Q1 = c("Q10A_control_reversed", "Q10D_experiment_reversed"), 
          Q2 = c("Q10B_control_reversed", "Q10E_experiment_reversed"), 
          Q3 = c("Q10C_control_reversed", "Q10F_experiment_reversed"))


# Creating an endorse object, excluding all covariates that are in the set { traditionalism }

poland_endorse_object <- endorse(Y = Y, 
                          data = data_pol,
                          prop = 0.004,
                          identical.lambda = FALSE,
                          covariates = FALSE,
                          omega2.out = TRUE,
                          hierarchical = FALSE
)

rm(list = setdiff(ls(), c("slovakia_endorse_object", "poland_endorse_object", "czechia_endorse_object", "hungary_endorse_obect")))

# Create endorse object for hungary


HUData <- read_sav("~/projects/AaD_Research/datasets/scrubbed_datasets/hungary_scrubbed.sav")

# Define questions for the endorsement experiment
data_hu_questions <- c("id", "A10A_1_control_reversed", "A10A_2_control_reversed", "A10A_3_control_reversed",
                       "A10B_1_experiment_reversed", "A10B_2_experiment_reversed", "A10B_3_experiment_reversed"
)
# Select relevant columns for endorsement analysis
data_hu_questions <- HUData[data_hu_questions]

# Define variables to keep
vars <- c("id", "Male", "Age", "Education", "Capital", "IdeologyLR", "FamIncome",  "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "EconGrievanceProspMostFams",
          "GayNeighbor", "GayFamily", "GayLesRights", "ForNeighbor", "ForPartner", "Ukraine",
          "NativeJobs", "NativeRights", "DemonstrateNational", "Religiosity", "VoteFarRight"
)

# Subset and recode variables
data_hu_vars <- HUData[vars]

# Convert all variables to numeric
data_hu_vars <- mutate(data_hu_vars, across(everything(), ~as.numeric(.)))

# Merge the questions and standardized variables datasets
data_hu <- left_join(data_hu_questions, data_hu_vars, by = "id")

# Create named list for response questions
Y <- list(Q1 = c("A10A_1_control_reversed", "A10B_1_experiment_reversed"), 
          Q2 = c("A10A_2_control_reversed", "A10B_2_experiment_reversed"), 
          Q3 = c("A10A_3_control_reversed", "A10B_3_experiment_reversed")
)

# Creating an endorse object, excluding all covariates that are in the set { traditionalism }
hungary_endorse_object <- endorse(Y = Y, 
                          data = data_hu,
                          identical.lambda = FALSE,
                          covariates = FALSE,
                          prop = 0.008,
                          omega2.out = TRUE,
                          hierarchical = FALSE
)

rm(list = setdiff(ls(), c("slovakia_endorse_object", "poland_endorse_object", "czechia_endorse_object", "hungary_endorse_obect")))

# Create endorse object for czechia

CZData <- read_sav("~/projects/AaD_Research/datasets/scrubbed_datasets/czechia_scrubbed.sav")

# Define questions for the endorsement experiment
questions <- c("id", "Q10AA_control_reversed", "Q10AB_control_reversed", "Q10AC_control_reversed",
               "Q10BA_experiment_reversed", "Q10BB_experiment_reversed", "Q10BC_experiment_reversed"
)
# Select relevant columns for endorsement analysis
data_cz_questions <- CZData[questions]

# Define variables to keep
vars <- c("id", "Male", "Age", "Education", "Capital", "IdeologyLR", "Income", "FamIncome", "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "GayNeighbor", "GayFamily", "ForNeighbor",
          "ForPartner", "Ukraine", "NativeJobs", "NativeRights", "Religiosity", "VoteFarRight", "DemonstrateNational")

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

# Creating an endorse object

czechia_endorse_object <- endorse(Y = Y, 
                          data = data_cz,
                          identical.lambda = FALSE,
                          covariates = FALSE,
                          prop = 0.010,
                          omega2.out = TRUE,
                          hierarchical = FALSE
)

rm(list = setdiff(ls(), c("slovakia_endorse_object", "poland_endorse_object", "czechia_endorse_object", "hungary_endorse_obect")))



### Now begin extracting lambda, sigma2 values for each country;

slovakia_lambda_samples <- as.matrix(slovakia_endorse_object$lambda)
slovakia_sigma2 <-(slovakia_endorse_object$x)^2

poland_lambda_samples <- as.matrix(poland_endorse_object$lambda)
poland_sigma2 <-(poland_endorse_object$x)^2

hungary_lambda_samples <- as.matrix(hungary_endorse_object$lambda)
hungary_sigma2 <-(hungary_endorse_object$x)^2

czechia_lambda_samples <- as.matrix(czechia_endorse_object$lambda)
czechia_sigma2 <-(czechia_endorse_object$x)^2

### average the lambda samples

slovakia_avg_lambda <- (slovakia_lambda_samples[, "(Intercept).1.1"] + 
                          slovakia_lambda_samples[, "(Intercept).2.1"] + 
                          slovakia_lambda_samples[, "(Intercept).3.1"]) / 3

poland_avg_lambda <- (poland_lambda_samples[, "(Intercept).1.1"] + 
                        poland_lambda_samples[, "(Intercept).2.1"] + 
                        poland_lambda_samples[, "(Intercept).3.1"]) / 3

hungary_avg_lambda <- (hungary_lambda_samples[, "(Intercept).1.1"] + 
                         hungary_lambda_samples[, "(Intercept).2.1"] + 
                         hungary_lambda_samples[, "(Intercept).3.1"]) / 3

czechia_avg_lambda <- (czechia_lambda_samples[, "(Intercept).1.1"] + 
                         czechia_lambda_samples[, "(Intercept).2.1"] + 
                         czechia_lambda_samples[, "(Intercept).3.1"]) / 3

### Create list of standardized samples

standardized_lambda_samples <- list(
  slovakia = (slovakia_avg_lambda / slovakia_sigma2),
  poland = (poland_avg_lambda / poland_sigma2),
  hungary = (hungary_avg_lambda / hungary_sigma2),
  czechia = (czechia_avg_lambda / czechia_sigma2)
)

# --------------------------------------
# Calculate mean and confidence intervals for each question
question_stats <- lapply(standardized_lambda_samples, function(x) {
  list(
    mean = mean(x),
    ci = quantile(x, probs = c(0.025, 0.975))
  )
})

# Create visualizations
# --------------------------------------
library(ggplot2)
library(reshape2)
library(extrafont)  # For professional fonts
library(gridExtra)  # For adding table below plot

# Load additional fonts if available
# font_import()
# loadfonts(device = "win")  # Adjust for your OS

# Create dataframe in long format with average
box_data_long <- data.frame(
  Question = factor(rep(c("Czechia", "Slovakia", "Poland", "Hungary"), 
                        times = c(length(standardized_lambda_samples$czechia), length(standardized_lambda_samples$slovakia), 
                                  length(standardized_lambda_samples$poland), length(standardized_lambda_samples$hungary))),
                    levels = c("Czechia", "Slovakia", "Poland", "Hungary")),
  Support = c(standardized_lambda_samples$czechia, standardized_lambda_samples$slovakia, standardized_lambda_samples$poland, standardized_lambda_samples$hungary)
)

# Set a professional color palette suitable for publication
# Using a colorblind-friendly palette
journal_colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")

# Create publication-quality box plot
box_plot <- ggplot(box_data_long, aes(x = Question, y = Support, fill = Question)) +
  geom_boxplot(
    alpha = 0.85,
    outlier.shape = 21,
    outlier.size = 2,
    outlier.alpha = 0.7,
    outlier.color = "black",
    outlier.fill = "white",
    width = 0.6,
    lwd = 0.6
  ) +
  # Add horizontal reference line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  # Fixed y-axis limits as requested
  ylim(-1.5, 1.5) +
  # Labels
  labs(
    title = "Standardized Support by Country",
    subtitle = "Standardized posterior distributions across survey questions",
    x = "",  # No x-axis label needed
    y = "Standardized Support (λ / σ²)",
    caption = "Note: Boxes represent interquartile range; whiskers extend to 1.5 x IQR"
  ) +
  scale_fill_manual(values = journal_colors) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0, size = 12, color = "gray30"),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11, face = "bold", margin = margin(t = 5)),
    panel.grid.major.y = element_line(color = "gray90", size = 0.3),
    panel.grid.minor.y = element_line(color = "gray95", size = 0.2),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, size = 0.5),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 25, b = 20, l = 25)
  )

# Create a table with summary statistics for display below the plot
stats_table <- data.frame(
  Question = c("Question A", "Question B", "Question C", "Average"),
  Mean = sapply(question_stats, function(x) round(x$mean, 3)),
  Lower_CI = sapply(question_stats, function(x) round(x$ci[1], 3)),
  Upper_CI = sapply(question_stats, function(x) round(x$ci[2], 3))
)

# Format table for display
table_theme <- ttheme_minimal(
  core = list(fg_params = list(hjust = 1, x = 0.9, fontface = "plain"),
              bg_params = list(fill = c("white", "gray95"))),
  colhead = list(fg_params = list(fontface = "bold", hjust = 1, x = 0.9),
                 bg_params = list(fill = "white")),
  rowhead = list(fg_params = list(hjust = 0, x = 0.1)))

# Save plot

print(box_plot)

ggsave(
  filename = "~/projects/AaD_Research/output/plots/all/all_macro.pdf",
  plot = box_plot,
  device = Cairo::CairoPDF,
  width = 8,
  height = 6
)
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

#====================================================
# 1. Data Loading and Initial Processing for Slovakia
#====================================================

SKdata <- read_sav("~/projects/AaD_Research/datasets/scrubbed_datasets/slovakia_scrubbed_dataset.sav")

# Define questions for the endorsement experiment
questions <- c("id", "q10a_control", "q10b_control", "q10c_control", 
               "q10a_experiment", "q10b_experiment", "q10c_experiment")

# Select relevant columns for endorsement analysis
data_slvk_questions <- SKdata[questions]

# Define variables to keep
vars <- c("id", "male", "age", "educ", "capital", "ideology", "income", "DemPolGrievance", "PolicyPolGrievance",
      "DemonstrateTrad", "DemonstrateNational", "PetitionSameSex", "VoteFarRight", "VotePrevFarRight",
      "ideologyLC", "SocialMediaUse", "InternetUse", "SlovakNationality", "FAMincome", "Nationalist",
      "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg", "EconGrievenceProspMostFams",
      "NatPride", "RomaPartner", "RomaNeighbor", "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine",
      "ChristianSchool", "MaleChauvinism", "LawOrder", "ChurchPolitics", "Abortion", "TradMarriage", "SexbMarriage",
      "ChildHome", "MaleJobs", "NativeJobs", "NativeRights", "Religiosity")

# Subset and recode variables
data_slvk_vars <- SKdata[vars]

# Convert all variables to numeric
data_slvk_vars <- mutate(data_slvk_vars, across(everything(), ~as.numeric(.)))

# Merge the questions and standardized variables datasets
data_slvk <- left_join(data_slvk_questions, data_slvk_vars, by = "id")

# Create named list for response questions
Y <- list(Q1 = c("q10a_control", "q10a_experiment"), 
          Q2 = c("q10b_control", "q10b_experiment"), 
          Q3 = c("q10c_control", "q10c_experiment"))

#====================================================
# 2. Creating the endorse object
#====================================================

# Creating an endorse object, excluding all covariates that are in the set { traditionalism }

endorse_object <- endorse(Y = Y, 
                          data = data_slvk,
                          identical.lambda = FALSE,
                          covariates = TRUE,
                          formula.indiv = formula( ~ age + male + educ + 
                                                     capital + ideology + income + 
                                                     DemPolGrievance + PolicyPolGrievance + 
                                                     EconGrievenceRetro + EconGrievenceProspInd + 
                                                     EconGrievenceProspAgg + 
                                                     NativeRights + NativeJobs + NatPride +
                                                     DemonstrateNational + SlovakNationality +
                                                     GayNeighbor + LawOrder + MaleChauvinism + ChristianSchool +
                                                     GayFamily + ForNeighbor + DemonstrateTrad +
                                                     ForPartner + Ukraine + VoteFarRight + Nationalist + 
                                                     FAMincome + Religiosity
                                                  ),
                          hierarchical = FALSE
)

#====================================================
# 3. Plotting coefficient plots from the delta matrix 
#====================================================

# Create the dataframe using posterior samples
delta_matrix_values <- data.frame(
  mean = apply(endorse_object$delta[, 2:30], 2, mean),
  lower = apply(endorse_object$delta[, 2:30], 2, quantile, 0.025),
  upper = apply(endorse_object$delta[, 2:30], 2, quantile, 0.975)
)

# Add variable names and categories
delta_matrix_values$variables <- colnames(endorse_object$delta)[2:30]
delta_matrix_values$category <- NA

# Define categories
ses_demographics <- c("age", "male", "educ", "capital", "ideology", "income", "FAMincome")
political_economic_grievances <- c("DemPolGrievance", "PolicyPolGrievance", 
                                   "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg")
nationalism <- c("NatPride", "NativeRights", "NativeJobs", "DemonstrateNational", 
                 "SlovakNationality", "Nationalist", "VoteFarRight")
traditionalism <- c("LawOrder", "MaleChauvinism", "ChristianSchool", "DemonstrateTrad", "Religiosity")
boundary_maintenance <- c("GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine")

# Assign categories
delta_matrix_values <- delta_matrix_values %>%
  mutate(
    category = case_when(
      variables %in% ses_demographics ~ "SES Demographics",
      variables %in% political_economic_grievances ~ "Political & Economic Grievances",
      variables %in% nationalism ~ "Nationalism",
      variables %in% traditionalism ~ "Traditionalism",
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
  "Traditionalism", 
  "Boundary Maintenance & Prejudice"
)
delta_matrix_values$category <- factor(delta_matrix_values$category, levels = category_order)

# Define custom labels for variables
custom_labels <- c(
  "age" = "Age",
  "male" = "Male",
  "educ" = "Education",
  "capital" = "Capital",
  "ideology" = "Political Ideology",
  "income" = "Personal Income",
  "FAMincome" = "Family Income",
  "DemPolGrievance" = "Political Grievance (Democracy)",
  "PolicyPolGrievance" = "Policy Grievance",
  "EconGrievenceRetro" = "Economic Grievance (Retro)",
  "EconGrievenceProspInd" = "Economic Grievance (Prospective-Ind)",
  "EconGrievenceProspAgg" = "Economic Grievance (Prospective-Agg)",
  "NatPride" = "National Pride",
  "NativeRights" = "Native Rights",
  "NativeJobs" = "Native Jobs",
  "DemonstrateNational" = "Demonstrated for National Values",
  "SlovakNationality" = "Slovak Nationality",
  "Nationalist" = "Prefers Nationalist Politics",
  "VoteFarRight" = "Far Right Voter",
  "LawOrder" = "Law & Order Support",
  "MaleChauvinism" = "Male Chauvinism Support",
  "ChristianSchool" = "Christian Schools Support",
  "DemonstrateTrad" = "Demonstrate Traditionalism",
  "Religiosity" = "Religiosity",
  "GayNeighbor" = "Anti-Gay Neighbor",
  "GayFamily" = "Anti-Gay Family",
  "ForNeighbor" = "Anti-Foreigner Neighbor",
  "ForPartner" = "Anti-Foreigner Neighbor",
  "Ukraine" = "Anti-Ukrainian Refugee"
)


# Create the plot
ggplot(delta_matrix_values, aes(x = variables, y = mean)) +
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

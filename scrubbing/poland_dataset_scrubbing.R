# Load required packages
library(haven)
library(dplyr)
library(missForest)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(forcats)
library(reshape2)
library(readxl)

#--------------------------------
# 1. Data Loading & Initial Setup
#--------------------------------

PData <- read_dta("~/projects/AaD_Research/datasets/raw_datasets/poland_raw_dataset.dta")

questions <- c("id", "Q10A_control_reversed", "Q10B_control_reversed", "Q10C_control_reversed", "Q10D_experiment_reversed", "Q10E_experiment_reversed", "Q10F_experiment_reversed")

data_pol_questions <- PData[questions]

#-----------------------------------
# 2. Variable Preparation & Recoding
#-----------------------------------

data_pol_vars <- PData %>%
  select(id, Male, Age, Education, Capital, IdeologyLR, Income, DemPolGrievance, PolicyPolGrievance,
         EconGrievanceRetro, EconGrievanceProspInd, EconGrievanceProspAgg, EconGrievanceProspMostFams,
         GayNeighbor, GayFamily, GayLesRights, ForNeighbor, ForPartner, Ukraine,
         NativeJobs, NativeRights, DemonstrateNational, Religiosity, VoteFarRight
  )

vars <- c("id", "Male", "Age", "Education", "Capital", "IdeologyLR", "Income",  "DemPolGrievance", "PolicyPolGrievance",
  "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "EconGrievanceProspMostFams",
  "GayNeighbor", "GayFamily", "GayLesRights", "ForNeighbor", "ForPartner", "Ukraine",
  "NativeJobs", "NativeRights", "DemonstrateNational", "Religiosity", "VoteFarRight"
)

# Subset and recode variables
data_pol_vars <- data_pol_vars[vars]

# Convert all variables to numeric
data_pol_vars <- mutate(data_pol_vars, across(everything(), ~as.numeric(.)))

#---------------------------------------------------
# 3. Missing Data Imputation & Final Transformations
#---------------------------------------------------
# data_slvk_vars <- data_slvk_vars %>%
#   mutate(across(c(ideology, income), as.factor))
# imputed <- missForest(as.data.frame(data_slvk_vars))
# data_slvk_vars <- as_tibble(imputed$ximp)
# 
# # Recode gender (1 → 0, 2 → 1) and bin age into 5 categories
# data_slvk_vars <- data_slvk_vars %>%
#   mutate(
#     gender = recode(gender, `1` = 0, `2` = 1),
#     age = as.integer(cut(age,
#                          breaks = c(18, 32, 45, 55, 65, 90),
#                          labels = 1:5,
#                          include.lowest = TRUE,
#                          right = TRUE))
#   )

#-------------------------------
# 4. Merge & Export Clean Data
#-------------------------------
final_data <- merge(data_pol_vars, data_pol_questions, by = "id")
write_sav(final_data, "~/projects/AaD_Research/datasets/scrubbed_datasets/poland_scrubbed.sav")

rm(list = ls())
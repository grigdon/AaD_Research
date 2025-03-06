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

HGData <- read_dta("~/projects/AaD_Research/datasets/raw_datasets/hungary_raw_dataset.dta")

questions <- c("id","A10A_1_control_reversed", "A10A_2_control_reversed", "A10A_3_control_reversed", "A10B_1_experiment_reversed", "A10B_2_experiment_reversed", "A10B_3_experiment_reversed")

data_hu_questions <- HGData[questions]

#-----------------------------------
# 2. Variable Preparation & Recoding
#-----------------------------------

data_hu_vars <- HGData %>%
  select(id, Male, Age, Education, Capital, IdeologyLR, FamIncome, DemPolGrievance, PolicyPolGrievance,
         EconGrievanceRetro, EconGrievanceProspInd, EconGrievanceProspAgg, EconGrievanceProspMostFams,
         GayNeighbor, GayFamily, GayLesRights, ForNeighbor, ForPartner, Ukraine,
         NativeJobs, NativeRights, DemonstrateNational, Religiosity, VoteFarRight
  )

vars <- c("id", "Male", "Age", "Education", "Capital", "IdeologyLR", "FamIncome",  "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "EconGrievanceProspMostFams",
          "GayNeighbor", "GayFamily", "GayLesRights", "ForNeighbor", "ForPartner", "Ukraine",
          "NativeJobs", "NativeRights", "DemonstrateNational", "Religiosity", "VoteFarRight"
)

# Subset and recode variables
data_hu_vars <- data_hu_vars[vars]

# Convert all variables to numeric
data_hu_vars <- mutate(data_hu_vars, across(everything(), ~as.numeric(.)))

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
final_data <- merge(data_hu_vars, data_hu_questions, by = "id")
write_sav(final_data, "~/projects/AaD_Research/datasets/scrubbed_datasets/hungary_scrubbed.sav")

rm(list = ls())
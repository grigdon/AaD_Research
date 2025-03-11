library(haven)
library(dplyr)
library(missForest)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(forcats)
library(reshape2)
library(readxl)
library(tibble)

#--------------------------------
# 1. Data Loading & Initial Setup
#--------------------------------

CZData <- read_dta("~/projects/AaD_Research/datasets/raw_datasets/czechia_raw_dataset.dta")

# Select endorsement experiment questions and reverse code responses
questions <- c("Q10AA_control_reversed", "Q10AB_control_reversed", "Q10AC_control_reversed",
               "Q10BA_experiment_reversed", "Q10BB_experiment_reversed", "Q10BC_experiment_reversed"
)

# Replaced 'CD' with 'id' and changed data type
data_cz_questions <- as.data.frame(CZData[questions])

data_cz_questions <- rowid_to_column(data_cz_questions, "id")

data_cz_questions <- mutate(data_cz_questions, across(everything(), ~as.numeric(.)))

#-----------------------------------
# 2. Variable Preparation & Recoding
#-----------------------------------

data_cz_vars <- CZData %>%
  select(CD, Male, Age, Education, Capital, IdeologyLR, Income, FamIncome, DemPolGrievance, PolicyPolGrievance,
         EconGrievanceRetro, EconGrievanceProspInd, EconGrievanceProspAgg, EconGrievanceProspMostFams,
         GayNeighbor, GayFamily, ForNeighbor, ForPartner, Ukraine,
         NativeJobs, NativeRights, Religiosity, VoteFarRight, DemonstrateNational
  )

vars <- c("Male", "Age", "Education", "Capital", "IdeologyLR", "Income", "FamIncome", "DemPolGrievance", "PolicyPolGrievance",
          "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "EconGrievanceProspMostFams",
          "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine",
          "NativeJobs", "NativeRights", "Religiosity", "VoteFarRight", "DemonstrateNational")

# Subset and recode variables
data_cz_vars <- as.data.frame(data_cz_vars[vars])

data_cz_vars <- rowid_to_column(data_cz_vars, "id")

# Convert all variables to numeric
data_cz_vars <- mutate(data_cz_vars, across(everything(), ~as.numeric(.)))

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

#------------------------------
# 4. Merge & Export Clean Data
#------------------------------

final_data <- merge(data_cz_vars, data_cz_questions, by = "id")
write_sav(final_data, "~/projects/AaD_Research/datasets/scrubbed_datasets/czechia_scrubbed.sav")

rm(list = ls())
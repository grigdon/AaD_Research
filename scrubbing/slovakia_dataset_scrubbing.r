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

SKdata <- read_dta("~/projects/AaD_Research/datasets/raw_datasets/slovakia_raw_dataset.dta")

# Select endorsement experiment questions and reverse code responses
questions <- c("id", "q10a_control", "q10b_control", "q10c_control", 
               "q10a_experiment", "q10b_experiment", "q10c_experiment")
data_slvk_questions <- SKdata[questions] %>%
  mutate(across(2:7, ~ recode(as.numeric(.), 
                              `1` = 4L, `2` = 3L, `3` = 2L, `4` = 1L, 
                              .default = NA_integer_)))

#-----------------------------------
# 2. Variable Preparation & Recoding
#-----------------------------------

data_slvk_vars <- SKdata %>%
  select(id, male, age, educ, capital, ideology, income, DemPolGrievance, PolicyPolGrievance, DemonstrateTrad, DemonstrateNational,
         PetitionSameSex, VoteFarRight, VotePrevFarRight, ideologyLC, SocialMediaUse, InternetUse, SlovakNationality, FAMincome, Nationalist,
         EconGrievenceRetro, EconGrievenceProspInd, EconGrievenceProspAgg, EconGrievenceProspMostFams, NatPride, RomaPartner,
         RomaNeighbor, GayNeighbor, GayFamily, ForNeighbor, ForPartner, Ukraine, ChristianSchool, MaleChauvinism, LawOrder, ChurchPolitics,
         Abortion, TradMarriage, SexbMarriage, ChildHome, MaleJobs, NativeJobs, NativeRights, Religiosity
  )

vars <- c("id", "male", "age", "educ", "capital", "ideology", "income", "DemPolGrievance", "PolicyPolGrievance",
          "DemonstrateTrad", "DemonstrateNational", "PetitionSameSex", "VoteFarRight", "VotePrevFarRight",
          "ideologyLC", "SocialMediaUse", "InternetUse", "SlovakNationality", "FAMincome", "Nationalist",
          "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg", "EconGrievenceProspMostFams",
          "NatPride", "RomaPartner", "RomaNeighbor", "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine",
          "ChristianSchool", "MaleChauvinism", "LawOrder", "ChurchPolitics", "Abortion", "TradMarriage", "SexbMarriage",
          "ChildHome", "MaleJobs", "NativeJobs", "NativeRights", "Religiosity")

# Subset and recode variables
data_slvk_vars <- data_slvk_vars[vars]

# Convert all variables to numeric
data_slvk_vars <- mutate(data_slvk_vars, across(everything(), ~as.numeric(.)))

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
final_data <- merge(data_slvk_vars, data_slvk_questions, by = "id")
write_sav(final_data, "~/projects/AaD_Research/datasets/scrubbed_datasets/slovakia_scrubbed.sav")

rm(list = ls())
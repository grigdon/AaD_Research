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

#-------------------------------
# 1. Data Loading & Initial Setup
#-------------------------------
SKdata <- read_sav("~/projects/AaD_Research/datasets/Slovakia.sav")

# Select endorsement experiment questions and reverse code responses
questions <- c("id", "q10a_control", "q10b_control", "q10c_control", 
               "q10a_experiment", "q10b_experiment", "q10c_experiment")
data_slvk_questions <- SKdata[questions] %>%
  mutate(across(2:7, ~ recode(as.numeric(.), 
                              `1` = 4L, `2` = 3L, `3` = 2L, `4` = 1L, 
                              .default = NA_integer_)))

#-------------------------------
# 2. Variable Preparation & Recoding
#-------------------------------
data_slvk_vars <- SKdata %>%
  mutate(
    id           = id,
    gender       = r1,
    age          = r2pov,
    education    = r4,
    is_capital   = recode(as.integer(r13), `1` = 1L, .default = 0L),
    ideology     = recode(as.integer(y1), `1` = 1L, `2` = 2L, `3` = 3L, 
                          `4` = 4L, `5` = 5L, `9` = 99L),
    income       = recode(as.integer(r11b), `0` = 0L, `1` = 1L, `2` = 2L,
                          `3` = 3L, `4` = 4L, `5` = 5L, `99` = 99L),
    DemPolGrievence    = q4a,
    PolicyPolGrievence = q4b,
    EconGrievenceRetro = q5a,
    EconGrievenceProspInd = q5b,
    EconGrievenceProspAgg = q5c,
    NatPride     = q7,
    NativeRights = q9a,
    NativeJobs   = q9m,
    LawOrder     = q9e,
    Chauvinism   = q9d,
    ChristianSchool = q9c,
    GayNeighbor  = q8c,
    GayPartner   = q8d,
    ForNeighbor  = q8e,
    ForPartner   = q8f,
    Ukraine      = q8g,
    DemonstrateTrad = q12a   # Excluding DemonstrateNational (q12b)
  )

# Keep selected variables
vars <- c("id", "gender", "age", "education", "is_capital", "income", 
          "ideology", "DemPolGrievence", "PolicyPolGrievence",
          "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg",
          "NatPride", "NativeRights", "NativeJobs", "LawOrder", "Chauvinism",
          "ChristianSchool", "GayNeighbor", "GayPartner", "ForNeighbor",
          "ForPartner", "Ukraine")
data_slvk_vars <- data_slvk_vars[vars]

# Debug: Print unique values for columns 14-23 (NativeRights to Ukraine)
print("Unique values in columns 14-23:")
print(sapply(data_slvk_vars[14:23], unique))

# Reverse recode values in columns 14-23 and convert all variables to numeric
data_slvk_vars <- data_slvk_vars %>%
  mutate(across(14:23, ~ recode(as.numeric(.),
                                `1` = 4L, `2` = 3L, `3` = 2L, `4` = 1L,
                                .default = NA_integer_))) %>%
  mutate(across(everything(), as.numeric))

cat("Number of columns in data_slvk_vars:", ncol(data_slvk_vars), "\n")

# Replace values 99 and 9 with NA (all except id)
n_cols <- ncol(data_slvk_vars)
data_slvk_vars <- data_slvk_vars %>%
  mutate(across(2:n_cols, ~ ifelse(. %in% c(99, 9), NA, .)))

#-------------------------------
# 3. Missing Data Imputation & Final Transformations
#-------------------------------
data_slvk_vars <- data_slvk_vars %>%
  mutate(across(c(ideology, income), as.factor))
imputed <- missForest(as.data.frame(data_slvk_vars))
data_slvk_vars <- as_tibble(imputed$ximp)

# Recode gender (1 → 0, 2 → 1) and bin age into 5 categories
data_slvk_vars <- data_slvk_vars %>%
  mutate(
    gender = recode(gender, `1` = 0, `2` = 1),
    age = as.integer(cut(age,
                         breaks = c(18, 32, 45, 55, 65, 90),
                         labels = 1:5,
                         include.lowest = TRUE,
                         right = TRUE))
  )

#-------------------------------
# 4. Merge & Export Clean Data
#-------------------------------
final_data <- merge(data_slvk_vars, data_slvk_questions, by = "id")
write_sav(final_data, "~/projects/AaD_Research/datasets/scrubbed_datasets/slvk_scrubbed.sav")

rm(list = ls())
# Data scrubbing script which produces a clean data file ready to be ran through the endorsement experiment

# Load required packages
library(haven)
library(ggpubr)
library(missForest)
library(forcats)
library(reshape2)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

#============================================
# 1. Data Loading and Initial Processing
#============================================

# Load the survey data
SKdata <- read_sav("/home/grigdon/projects/AaD_Research/datasets/Slovakia.sav")

# Define questions for the endorsement experiment
questions <- c("id", "q10a_control", "q10b_control", "q10c_control", 
               "q10a_experiment", "q10b_experiment", "q10c_experiment")

# Select relevant columns for endorsement analysis
data_slvk_questions <- SKdata[questions]

# Reverse code responses (1 ↔ 4, 2 ↔ 3)
data_slvk_questions <- data_slvk_questions %>%
  mutate(across(2:7, ~ recode(as.numeric(.), 
                              `1` = 4L, 
                              `2` = 3L, 
                              `3` = 2L, 
                              `4` = 1L, 
                              .default = NA_integer_)))

#============================================
# 2. Variable Preparation and Recoding
#============================================

# Prepare and relabel covariates
data_slvk_vars <- mutate(SKdata,
                         id = id,
                         gender = r1,
                         age = r2pov,
                         education = r4,
                         is_capital = recode(as.integer(r13),
                                             `1` = 1L,
                                             .default = 0L),
                         ideology = recode(as.integer(y1),
                                           `1` = 1L, `2` = 2L, `3` = 3L, 
                                           `4` = 4L, `5` = 5L, `9` = 99L),
                         income = recode(as.integer(r11b),
                                         `0` = 0L, `1` = 1L, `2` = 2L,
                                         `3` = 3L, `4` = 4L, `5` = 5L,
                                         `99` = 99L),
                         DemPolGrievence = q4a,
                         PolicyPolGrievence = q4b,
                         EconGrievenceRetro = q5a,
                         EconGrievenceProspInd = q5b,
                         EconGrievenceProspAgg = q5c,
                         NatPride = q7,
                         NativeRights = q9a,
                         NativeJobs = q9m,
                         LawOrder = q9e,
                         Chauvinism = q9d,
                         ChristianSchool = q9c,
                         GayNeighbor = q8c,
                         GayPartner = q8d,
                         ForNeighbor = q8e,
                         ForPartner = q8f,
                         Ukraine = q8g,
                         DemonstrateNational = q12b,
                         DemonstrateTrad = q12a
)

# Excluding "DemonstrateNational"; there is not enough data for this variable

# Define variables to keep
vars <- c("id", "gender", "age", "education", "is_capital", "income", 
          "ideology", "DemPolGrievence", "PolicyPolGrievence",
          "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg",
          "NatPride", "NativeRights", "NativeJobs", "LawOrder", "Chauvinism",
          "ChristianSchool", "GayNeighbor", "GayPartner", "ForNeighbor",
          "ForPartner", "Ukraine")

# Subset and recode variables
data_slvk_vars <- data_slvk_vars[vars]

# First, let's check what values we actually have in these columns
value_check <- sapply(data_slvk_vars[14:23], function(x) unique(x))
print("Unique values in columns 14-24:")
print(value_check)

# Recode specific variables with a default value for unspecified cases
data_slvk_vars <- mutate(data_slvk_vars, 
                         across(14:23, ~ recode(as.numeric(.),
                                                `1` = 4L,
                                                `2` = 3L,
                                                `3` = 2L,
                                                `4` = 1L,
                                                .default = NA_integer_)))

# Convert all variables to numeric
data_slvk_vars <- mutate(data_slvk_vars, across(everything(), ~as.numeric(.)))

# Check the number of columns before applying the missing value treatment
n_cols <- ncol(data_slvk_vars)
print(paste("Number of columns in data_slvk_vars:", n_cols))

# Handle missing values for all columns except ID (adjust range based on actual number of columns)
data_slvk_vars <- mutate(data_slvk_vars, 
                         across(2:n_cols, ~ifelse(. %in% c(99, 9), NA, .)))
#============================================
# 3. Missing Data Imputation
#============================================

# Check if missing values are properly handled
sum(is.na(data_slvk_vars))

data_slvk_vars <- data_slvk_vars %>%
  mutate(across(c(ideology, income), as.factor))

# Impute missing values using Random Forest
data_slvk_vars_imputed <- missForest(as.data.frame(data_slvk_vars))

# Convert back to tibble
data_slvk_vars_imputed <- as_tibble(data_slvk_vars_imputed$ximp)

sum(is.na(data_slvk_vars_imputed))

# First, let's see what columns we have
print("Column names in data_slvk_vars:")
print(names(data_slvk_vars))

# Convert imputed data to tibble
data_slvk_vars <- as_tibble(data_slvk_imp$ximp)

# Get the number of columns (excluding the ID column which is column 1)
n_cols <- ncol(data_slvk_vars)

# Verify the standardization worked
print("Summary of standardized variables:")
print(summary(data_slvk_vars))

# Load data
df <- as_tibble(data_slvk_imp$ximp)

# Define columns to process
cols_to_round <- c("income", "ideology",
                   grep("Grievence|Pride|Jobs|LawOrder|Chauvinism|School|Neighbor|Partner", 
                        names(df), value = TRUE))

# Transformation pipeline
df_standardized <- df %>%
  mutate(
    # Convert gender to 0/1
    gender = recode(gender, `1` = 0, `2` = 1),

    # Bin age into 5 categories
    age = as.integer(as.character(cut(
      age,
      breaks = c(18, 31, 45, 59, 73, 88),
      labels = 1:5,
      include.lowest = TRUE,
      right = TRUE
    ))),

    # Round and clamp ordinal variables
    across(all_of(cols_to_round), ~ pmin(pmax(round(.), 1), 5)),

    # Shift demonstration variables
    across(c(Ukraine, DemonstrateNational, DemonstrateTrad), ~ . + 1)
  )


# Save standardized data
write_csv(df_standardized, "data_standardized.csv")

data_slvk_gaber <- as_tibble(merge(df_standardized, data_slvk_questions, by = "id"))


gaber_endorse <- endorse(Y = Y, data = data_slvk_gaber, identical.lambda = FALSE, covariates = TRUE, formula.indiv = formula( ~ age + gender + education + is_capital + ideology + income + DemPolGrievence + PolicyPolGrievence + EconGrievenceRetro + EconGrievenceProspInd + EconGrievenceProspAgg + NatPride + NativeRights + NativeJobs + DemonstrateNational + LawOrder + Chauvinism + ChristianSchool + DemonstrateTrad + GayNeighbor+ GayPartner+ ForNeighbor + ForPartner + Ukraine), hierarchical = FALSE)

df <- data.frame(as.matrix(gaber_endorse$delta))
ci <- data.frame(variables = colnames(df[2:25]), mean = apply(df[2:25], 2, mean), sd = apply(df[2:25], 2, sd))
ci <- mutate(ci, max = mean + 1.96 * sd, min = mean - 1.96 * sd)
ci$variables <- c("Age", "Gender", "Education", "Capital", "Ideology", "Income", "DemPolGrievence", "PolicyPolGrievence", "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg", "NatPride", "NativeRights", "NativeJobs", "DemonstrateNational", "LawOrder", "Chauvinism", "ChristianSchool", "DemonstrateTrad", "GayNeighbor", "GayPartner", "ForNeighbor", "ForPartner", "Ukraine")
ci <- mutate(ci, variables= fct_reorder(variables, mean))


#plot results with 95 ci

ggplot(ci, aes(x = variables, y = mean)) + geom_point() + geom_errorbar(aes(ymin = min, ymax = max)) + coord_flip() + geom_abline(slope = 0, intercept = 0, color = "red") + ggtitle("Model 2: Full Model") + theme(plot.title = element_text(hjust = 0.5)) 

#end
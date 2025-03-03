#load packages

library(endorse)
library(tidyverse)
library(haven)
library(ggpubr)
library(missForest)
library(forcats)
library(reshape2)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr) # For pivot_longer()

#set wd
#setwd("~/cloud/project/ReceivedScripts/")

#load data
SKdata <- read_sav("/cloud/project/ReceivedScripts/Slovakia.sav")

#view data
View(SKdata)

##attach

attach(SKdata)



###prepare the data

questions <- c("id", "q10a_control", "q10b_control", "q10c_control", 
               "q10a_experiment", "q10b_experiment", "q10c_experiment")


###summarize 
summary(SKdata)


# Select relevant columns
data_slvk_questions <- SKdata[questions]

# Reverse code responses (1 ↔ 4, 2 ↔ 3)
data_slvk_questions <- data_slvk_questions %>%mutate(across(2:7, ~ recode(as.numeric(.), 
                    `1` = 4L, 
                    `2` = 3L, 
                    `3` = 2L, 
                    `4` = 1L, 
                    .default = NA_integer_))) # Keeps NA values intact

# Check if the recoding worked
summary(data_slvk_questions)


#calculate and display treatment effects

# Define experimental and control condition columns
control_questions <- c("q10a_control", "q10b_control", "q10c_control")
experiment_questions <- c("q10a_experiment", "q10b_experiment", "q10c_experiment")



# Compute mean responses for control and experimental groups separately
control_means <- data_slvk_questions %>%
  summarise(across(all_of(control_questions), \(x) mean(x, na.rm = TRUE), .names = "control_{.col}"))

experiment_means <- data_slvk_questions %>%
  summarise(across(all_of(experiment_questions), \(x) mean(x, na.rm = TRUE), .names = "experiment_{.col}"))

# Merge the two summaries into one data frame
treatment_effects <- bind_cols(control_means, experiment_means) %>%
  mutate(
        diff_q10a = experiment_q10a_experiment - control_q10a_control,
        diff_q10b = experiment_q10b_experiment - control_q10b_control,
        diff_q10c = experiment_q10c_experiment - control_q10c_control
  )

# Display results
print(treatment_effects)

# Reshape data for easier visualization
treatment_effects_long <- treatment_effects %>%
    pivot_longer(cols = starts_with("diff_"), names_to = "question", values_to = "treatment_effect")

# Plot the treatment effects
ggplot(treatment_effects_long, aes(x = question, y = treatment_effect, fill = treatment_effect > 0)) +
    geom_col() +
    coord_flip() +
    labs(title = "Treatment Effects: Experimental vs. Control Condition",
            x = "Survey Question", y = "Mean Difference (Experimental - Control)") +
    theme_minimal()


#relabel covariates:

data_slvk_vars <- mutate(SKdata,
                id = id,
                gender = r1,
                age = r2pov,
                education = r4,
                is_capital = recode(as.integer(r13),
                           `1` = 1L,
                          .default = 0L),
                ideology = recode(as.integer(y1),
                          `1` = 1L,
                          `2` = 2L,
                          `3` = 3L,
                          `4` = 4L,
                          `5` = 5L,
                          `9` = 99L),
                income = recode(as.integer(r11b),
                          `0` = 0L,
                          `1` = 1L,
                          `2` = 2L,
                          `3` = 3L,
                          `4` = 4L,
                          `5` = 5L,
                          `99` = 99L),
                           DemPolGrievence = q4a,
                          PolicyPolGrievence = q4b,
                          EUPolGrievence = q4c,
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


#collect new variable labels
vars <- c("id", "gender", "age", "education", "is_capital", "income", "ideology", "DemPolGrievence","PolicyPolGrievence","EUPolGrievence","EconGrievenceRetro","EconGrievenceProspInd","EconGrievenceProspAgg","NatPride","NativeRights", 
"NativeJobs","LawOrder","Chauvinism","ChristianSchool","GayNeighbor","GayPartner","ForNeighbor","ForPartner","Ukraine", "DemonstrateNational", "DemonstrateTrad")

#subset and recode covariates
data_slvk_vars <- data_slvk_vars[vars]

data_slvk_vars <- mutate(data_slvk_vars, across(14:24, ~ recode(as.numeric(.),
                  `1` = 4L,
                  `2` = 3L,
                  `3` = 2L,
                  `4` = 1L)
                  )
)


data_slvk_vars <- mutate(data_slvk_vars, across(everything(), ~as.numeric(.)))
data_slvk_vars <- mutate(data_slvk_vars, across(6:26, ~ifelse(. %in% c(99, 9), NA, .)))


#look at missing
sapply(data_slvk_vars, function(x) sum(is.na(x)))

#shows only income and ideology have NAs

#impute using RF

data_slvk_imp <- missForest(as.data.frame(data_slvk_vars))


#Extract the imputed dataset (ximp) from data_slvk_imp and convert it into a tibble (a modern version of a data frame).
#ximp likely contains imputed values for missing variables in the dataset.

data_slvk_vars <- as_tibble(data_slvk_imp$ximp)

#This standardizes variables from columns 2 to 26 by applying scale().
#scale(.) centers the data (mean = 0) and scales it (SD = 1), making the variables comparable.
#as.vector(scale(.)) ensures the result remains a simple numeric vector.
#mutate(across(...)) applies this transformation across multiple columns.

#look again at missing

sapply(data_slvk_vars, function(x) sum(is.na(x)))
#now no NAs

# TEST







library(tidyverse)

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


# END TEST


#The following section may be entirely wrong! Wierdly standardized values without regard to type

data_slvk_vars <- mutate(data_slvk_vars, across(2:26, ~ as.vector(scale(.))))

#This merges data_slvk_vars (standardized imputed data) with data_slvk_questions (endorsement experiment responses) by the “id” column.
#merge(..., by = "id") ensures that the standardized variables and endorsement experiment responses are properly aligned for analysis. #This ensures that all predictors are on a comparable scale, preventing variables with different units (e.g., income vs. ideology) from dominating the model
#as_tibble() ensures the result remains a tibble for better handling in tidyverse.

data_slvk <- as_tibble(merge(data_slvk_vars, data_slvk_questions, by = "id"))


#create named list (Y) for the response questions, where each element represents a pair of control and experimental conditions from the endorsement experiment.
#Each key (Q1, Q2, Q3) corresponds to a survey question, mapping its control and experimental versions.

Y <- list(Q1 = c("q10a_control", "q10a_experiment"), Q2 = c("q10b_control", "q10b_experiment"), Q3 = c("q10c_control", "q10c_experiment"))


#relabel basic covariates - sociodemos



SKdata <- SKdata %>%
    rename(
          Gender = r1, # Make sure `gender` exists in SKdata
          education = r4, # Make sure `education` exists
          is_capital = r13, # Make sure `is_capital` exists
          ideology = y1, # Make sure `ideology` exists
          income = r11b, # Make sure `income` exists
          age = r2pov # Make sure `age` exists
    )




#Estimate endorse model

endorse1 <- endorse(Y = Y, data = SKdata, identical.lambda = FALSE, covariates = TRUE, formula.indiv = formula( ~ r1 ), hierarchical = FALSE)


endorse1 <- endorse(Y = Y, data = SKdata, identical.lambda = FALSE, covariates = TRUE, formula.indiv = formula( ~ age + Gender + education + is_capital + ideology + income), hierarchical = FALSE)



# This converts the delta column from the endorse1 data frame into a matrix and then into a new data frame df. The as.matrix() function converts endorse1$delta into a matrix, and the data.frame() function converts it into a data frame. This step is useful if endorse1$delta was not already a matrix or if you wanted to ensure it is structured as a data frame.

df <- data.frame(as.matrix(endorse1$delta))

###This line calculates the mean and standard deviation for columns 2 to 7 of df and stores them in a new data frame ci. #df[2:7]: Selects columns 2 through 7 of df. #colnames(df[2:7]): Extracts the column names for the selected columns and assigns them to the variables column of the new ci data frame. #apply(df[2:7], 2, mean): Computes the mean of each column (specified by the 2 in the second argument, which tells apply() to work across columns). #apply(df[2:7], 2, sd): Computes the standard deviation for each column in the same way.

ci <- data.frame(variables = colnames(df[2:7]), mean = apply(df[2:7], 2, mean), sd = apply(df[2:7], 2, sd))

##This adds two new columns (max and min) to the ci data frame that represent the confidence intervals for the means. #mutate(): This function from the dplyr package adds new columns or modifies existing ones. #max = mean + 1.96 * sd: Calculates the upper bound of the confidence interval (mean + 1.96 * standard deviation). #min = mean - 1.96 * sd: Calculates the lower bound of the confidence interval (mean - 1.96 * standard deviation).

ci <- mutate(ci, max = mean + 1.96 * sd, min = mean - 1.96 * sd)

#This assigns descriptive labels to the variables column of the ci data frame. #The variables column, which was previously filled with the column names from df[2:7], is now replaced with the human-readable labels provided in the c() vector.

ci$variables <- c("Age", "Gender", "Education", "Capital", "Ideology", "Income")

#the result of the above is a data frame ci with the following structure:
#	•	variables: Descriptive labels for the variables (e.g., “Age”, “Gender”, etc.).
#	•	mean: The mean value of each variable (from columns 2 to 7 in df).
#	•	sd: The standard deviation for each variable.
#	•	max: The upper bound of the 95% confidence interval for each mean.
#	•	min: The lower bound of the 95% confidence interval for each mean.


###
ci <- mutate(ci, variables= fct_reorder(variables, mean))



###coefplot

ggplot(ci, aes(x = variables, y = mean)) + geom_point() + geom_errorbar(aes(ymin = min, ymax = max)) + coord_flip() + geom_abline(slope = 0, intercept = 0, color = "red") + ggtitle("Model 1: Background Characterstics") + theme(plot.title = element_text(hjust = 0.5)) + ylim(-0.5, 0.5)


##full model

endorseFULL <- endorse(Y = Y, data = data_slvk, identical.lambda = FALSE, covariates = TRUE, formula.indiv = formula( ~ age + gender + education + is_capital + ideology + income + DemPolGrievence + PolicyPolGrievence + EUPolGrievence + EconGrievenceRetro + EconGrievenceProspInd + EconGrievenceProspAgg + NatPride + NativeRights + NativeJobs + DemonstrateNational + LawOrder + Chauvinism + ChristianSchool + DemonstrateTrad + GayNeighbor+ GayPartner+ ForNeighbor + ForPartner + Ukraine), hierarchical = FALSE)

#same procedure as above but with more covariates with 95% confidence intervals, 1.96

df <- data.frame(as.matrix(endorseFULL$delta))
ci <- data.frame(variables = colnames(df[2:26]), mean = apply(df[2:26], 2, mean), sd = apply(df[2:26], 2, sd))
ci <- mutate(ci, max = mean + 1.96 * sd, min = mean - 1.96 * sd)
ci$variables <- c("Age", "Gender", "Education", "Capital", "Ideology", "Income", "DemPolGrievence", "PolicyPolGrievence", "EUPolGrievence", "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg", "NatPride", "NativeRights", "NativeJobs", "DemonstrateNational", "LawOrder", "Chauvinism", "ChristianSchool", "DemonstrateTrad", "GayNeighbor", "GayPartner", "ForNeighbor", "ForPartner", "Ukraine")
ci <- mutate(ci, variables= fct_reorder(variables, mean))


#plot results with 95 ci

ggplot(ci, aes(x = variables, y = mean)) + geom_point() + geom_errorbar(aes(ymin = min, ymax = max)) + coord_flip() + geom_abline(slope = 0, intercept = 0, color = "red") + ggtitle("Model 2: Full Model") + theme(plot.title = element_text(hjust = 0.5)) 




#same as above but with 90% confidence intervals, 1.65
df <- data.frame(as.matrix(endorseFULL$delta))
ci <- data.frame(variables = colnames(df[2:26]), mean = apply(df[2:26], 2, mean), sd = apply(df[2:26], 2, sd))
ci <- mutate(ci, max = mean + 1.65 * sd, min = mean - 1.65 * sd)
ci$variables <- c("Age", "Gender", "Education", "Capital", "Ideology", "Income", "DemPolGrievence", "PolicyPolGrievence", "EUPolGrievence", "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg", "NatPride", "NativeRights", "NativeJobs", "DemonstrateNational", "LawOrder", "Chauvinism", "ChristianSchool", "DemonstrateTrad", "GayNeighbor", "GayPartner", "ForNeighbor", "ForPartner", "Ukraine")
ci <- mutate(ci, variables= fct_reorder(variables, mean))

#plot results with 90% ci

ggplot(ci, aes(x = variables, y = mean)) + geom_point() + geom_errorbar(aes(ymin = min, ymax = max)) + coord_flip() + geom_abline(slope = 0, intercept = 0, color = "red") + ggtitle("Model 2: Full Model") + theme(plot.title = element_text(hjust = 0.5)) 


#FINAL MODEL: same procedure as above with 95% confidence intervals, 1.96 (minus EU pol grievance variable)

endorseFULL1 <- endorse(Y = Y, data = data_slvk, identical.lambda = FALSE, covariates = TRUE, formula.indiv = formula( ~ age + gender + education + is_capital + ideology + income + DemPolGrievence + PolicyPolGrievence + EconGrievenceRetro + EconGrievenceProspInd + EconGrievenceProspAgg + NatPride + NativeRights + NativeJobs + DemonstrateNational + LawOrder + Chauvinism + ChristianSchool + DemonstrateTrad + GayNeighbor+ GayPartner+ ForNeighbor + ForPartner + Ukraine), hierarchical = FALSE)

#This is a test that should be removed. Start here:

  # First, run the two models with different identical.lambda settings

      # Varying lambda values

      endorseFULL_FALSE <- endorse(Y = Y, data = data_slvk, identical.lambda = FALSE, 
                                   covariates = TRUE, 
                                   formula.indiv = formula( ~ age + gender + education + is_capital + 
                                                              ideology + income + DemPolGrievence + 
                                                              PolicyPolGrievence + EconGrievenceRetro + 
                                                              EconGrievenceProspInd + EconGrievenceProspAgg + 
                                                              NatPride + NativeRights + NativeJobs + 
                                                              DemonstrateNational + LawOrder + Chauvinism + 
                                                              ChristianSchool + DemonstrateTrad + GayNeighbor + 
                                                              GayPartner + ForNeighbor + ForPartner + Ukraine), 
                                   hierarchical = FALSE)
      
      # Identical lambda values
      
      endorseFULL_TRUE <- endorse(Y = Y, data = data_slvk, identical.lambda = TRUE, 
                                  covariates = TRUE, 
                                  formula.indiv = formula( ~ age + gender + education + is_capital + 
                                                             ideology + income + DemPolGrievence + 
                                                             PolicyPolGrievence + EconGrievenceRetro + 
                                                             EconGrievenceProspInd + EconGrievenceProspAgg + 
                                                             NatPride + NativeRights + NativeJobs + 
                                                             DemonstrateNational + LawOrder + Chauvinism + 
                                                             ChristianSchool + DemonstrateTrad + GayNeighbor + 
                                                             GayPartner + ForNeighbor + ForPartner + Ukraine), 
                                  hierarchical = FALSE)

      # Compare lambda parameters
      
      lambda_FALSE <- endorseFULL_FALSE$lambda
      lambda_TRUE <- endorseFULL_TRUE$lambda
      
      
          # Calculate variance and spread measures
          lambda_sd <- sd(as.numeric(lambda_FALSE))
          lambda_mean <- mean(as.numeric(lambda_FALSE))
          lambda_cv <- lambda_sd/lambda_mean
          lambda_range <- range(as.numeric(lambda_FALSE))
          
          cat("Varying Lambda summary statistics:\n")
          cat("- Mean:", lambda_mean, "\n")
          cat("- Standard deviation:", lambda_sd, "\n") 
          cat("- Coefficient of variation:", lambda_cv, "\n")
          cat("- Range:", lambda_range[1], "to", lambda_range[2], "\n")
          
          # Calculate variance and spread measures
          lambda_sd <- sd(as.numeric(lambda_TRUE))
          lambda_mean <- mean(as.numeric(lambda_TRUE))
          lambda_cv <- lambda_sd/lambda_mean
          lambda_range <- range(as.numeric(lambda_TRUE))
          
          cat("Identical Lambda summary statistics:\n")
          cat("- Mean:", lambda_mean, "\n")
          cat("- Standard deviation:", lambda_sd, "\n") 
          cat("- Coefficient of variation:", lambda_cv, "\n")
          cat("- Range:", lambda_range[1], "to", lambda_range[2], "\n")
          
        lambda_FALSE <- endorseFULL_FALSE$omega2
        lambda_TRUE <- endorseFULL_TRUE$omega2
        
    
# End test

df <- data.frame(as.matrix(endorseFULL1$delta))
ci <- data.frame(variables = colnames(df[2:25]), mean = apply(df[2:25], 2, mean), sd = apply(df[2:25], 2, sd))
ci <- mutate(ci, max = mean + 1.96 * sd, min = mean - 1.96 * sd)
ci$variables <- c("Age", "Gender", "Education", "Capital", "Ideology", "Income", "DemPolGrievence", "PolicyPolGrievence", "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg", "NatPride", "NativeRights", "NativeJobs", "DemonstrateNational", "LawOrder", "Chauvinism", "ChristianSchool", "DemonstrateTrad", "GayNeighbor", "GayPartner", "ForNeighbor", "ForPartner", "Ukraine")
ci <- mutate(ci, variables= fct_reorder(variables, mean))


#plot results with 95 ci

ggplot(ci, aes(x = variables, y = mean)) + geom_point() + geom_errorbar(aes(ymin = min, ymax = max)) + coord_flip() + geom_abline(slope = 0, intercept = 0, color = "red") + ggtitle("Model 2: Full Model") + theme(plot.title = element_text(hjust = 0.5)) 


#end

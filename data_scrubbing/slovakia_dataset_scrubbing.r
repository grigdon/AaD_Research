
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


gaber_endorse <- endorse(Y = Y, data = data_slvk_gaber, identical.lambda = FALSE, covariates = TRUE, formula.indiv = formula( ~ age + gender + education + is_capital + ideology + income + DemPolGrievence + PolicyPolGrievence + EconGrievenceRetro + EconGrievenceProspInd + EconGrievenceProspAgg + NatPride + NativeRights + NativeJobs + DemonstrateNational + LawOrder + Chauvinism + ChristianSchool + DemonstrateTrad + GayNeighbor+ GayPartner+ ForNeighbor + ForPartner + Ukraine), hierarchical = FALSE)

df <- data.frame(as.matrix(gaber_endorse$delta))
ci <- data.frame(variables = colnames(df[2:25]), mean = apply(df[2:25], 2, mean), sd = apply(df[2:25], 2, sd))
ci <- mutate(ci, max = mean + 1.96 * sd, min = mean - 1.96 * sd)
ci$variables <- c("Age", "Gender", "Education", "Capital", "Ideology", "Income", "DemPolGrievence", "PolicyPolGrievence", "EconGrievenceRetro", "EconGrievenceProspInd", "EconGrievenceProspAgg", "NatPride", "NativeRights", "NativeJobs", "DemonstrateNational", "LawOrder", "Chauvinism", "ChristianSchool", "DemonstrateTrad", "GayNeighbor", "GayPartner", "ForNeighbor", "ForPartner", "Ukraine")
ci <- mutate(ci, variables= fct_reorder(variables, mean))


#plot results with 95 ci

ggplot(ci, aes(x = variables, y = mean)) + geom_point() + geom_errorbar(aes(ymin = min, ymax = max)) + coord_flip() + geom_abline(slope = 0, intercept = 0, color = "red") + ggtitle("Model 2: Full Model") + theme(plot.title = element_text(hjust = 0.5)) 

#end
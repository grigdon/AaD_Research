# Load required libraries
library(corrplot)
library(haven)
library(dplyr)

# Read dataset 
data_slvk <- read_sav("~/projects/AaD_Research/datasets/scrubbed_datasets/slovakia_scrubbed.sav")

selected_vars <- c("age", "male", "educ", "capital", "ideology", "income", "DemPolGrievance", "PolicyPolGrievance",
                   "EconGrievenceRetro",  "EconGrievenceProspInd", "EconGrievenceProspAgg", "NativeRights",
                   "NativeJobs", "NatPride", "DemonstrateNational",  "SlovakNationality", "GayNeighbor",  "LawOrder",
                   "MaleChauvinism", "ChristianSchool", "GayFamily", "ForNeighbor", "DemonstrateTrad", 
                   "ForPartner", "Ukraine", "VoteFarRight", "Nationalist",
                   "FAMincome", "Religiosity")

# Select only explanatory variables
data_slvk <- data_slvk[selected_vars]

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

# Calculate Spearman correlation coefficients
cor_matrix_slvk <- cor(data_slvk, method = "spearman", use = "pairwise.complete.obs")

# Get original column names to verify length
original_names <- colnames(cor_matrix_slvk)

# Ensure custom labels match the number of variables
if(length(custom_labels) != length(original_names)) {
  stop("Number of custom labels doesn't match number of variables!")
}

# Set custom labels as the column/row names
colnames(cor_matrix_slvk) <- custom_labels
rownames(cor_matrix_slvk) <- custom_labels

# Create a correlation heatmap and define output directory

output_dir = "~/projects/AaD_Research/output/plots/slovakia/corr"

pdf(file = file.path(output_dir, "slovakia_corr.pdf"), width = 8, height = 6)
corr_plot <- corrplot(cor_matrix_slvk, 
                      method = "color",           # Color tiles
                      type = "upper",             # Show only upper triangle
                      order = "hclust",           # Hierarchical clustering order
                      hclust.method = "complete", # Clustering method
                      tl.cex = 0.7,               # Text label size
                      tl.col = "black",           # Text label color
                      tl.srt = 60,                # Text label rotation
                      number.cex = 0.5,           # Coefficient text size
                      col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200), # Blue-white-orange palette
                      diag = FALSE,               # Hide diagonal
                      mar = c(0,0,2,0),           # Margins
                      title = "Interdependencies Between Slovak Explanatory Factors: Spearman Correlation",
                      cl.ratio = 0.2,             # Color key size
                      cl.align = "r",             # Color key alignment
)
dev.off()

# clears environment variables
rm(list = ls())
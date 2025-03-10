# Load required libraries
library(corrplot)
library(haven)
library(dplyr)

# Read dataset 
data_pol <- read_sav("~/projects/AaD_Research/datasets/scrubbed_datasets/poland_scrubbed.sav")

# Select only explanatory variables
data_pol <- data_pol[, 2:24]

# Define custom labels for variables
custom_labels <- c(
  "Age" = "Age", 
  "Male" = "Male", 
  "Education" = "Education", 
  "Capital" = "Capital", 
  "IdeologyLR" = "Political Ideology", 
  "Income" = "Personal Income", 
  "DemPolGrievance" = "Political Grievance (Democracy)", 
  "PolicyPolGrievance" = "Policy Grievance", 
  "EconGrievanceRetro" = "Economic Grievance (Retro)", 
  "EconGrievanceProspInd" = "Economic Grievance (Prospective-Ind)", 
  "EconGrievanceProspAgg" = "Economic Grievance (Prospective-Agg)", 
  "EconGrievanceProspMostFams" = "Economic Grievance (ProspMostFams)", 
  "DemonstrateNational" = "Demontrates Nationalism", 
  "NativeRights" = "Native Rights", 
  "NativeJobs" = "Native Jobs", 
  "VoteFarRight" = "Far Right Voter", 
  "Religiosity" = "Religiosity", 
  "GayNeighbor" = "Anti-Gay Neighbor", 
  "GayFamily" = "Anti-Gay Family", 
  "ForNeighbor" = "Anti-Foreigner Neighbor", 
  "ForPartner" = "Anti-Foreigner Neighbor", 
  "Ukraine" = "Anti-Ukrainian Refugee",
  "GayLesRights" = "Gay Lesbian Rights" 
)

# Calculate Spearman correlation coefficients
cor_matrix_pol <- cor(data_pol, method = "spearman", use = "pairwise.complete.obs")

# Get original column names to verify length
original_names <- colnames(cor_matrix_pol)

# Ensure custom labels match the number of variables
if(length(custom_labels) != length(original_names)) {
  stop("Number of custom labels doesn't match number of variables!")
}

# Set custom labels as the column/row names
colnames(cor_matrix_pol) <- custom_labels
rownames(cor_matrix_pol) <- custom_labels

# Create a correlation heatmap and define output directory

output_dir = "~/projects/AaD_Research/output/plots/poland/corr"

pdf(file = file.path(output_dir, "poland_corr.pdf"), width = 8, height = 6)
corr_plot <- corrplot(cor_matrix_pol, 
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
                      title = "Interdependencies Between Polish Explanatory Factors: Spearman Correlation",
                      cl.ratio = 0.2,             # Color key size
                      cl.align = "r",             # Color key alignment
)
dev.off()

# clears environment variables
rm(list = ls())
# 'Armed and Dangerous' Codebase 

## Overview
This repository contains scripts and data for analyzing survey data related to the AaD Research paper.

## Author
Gabe Rigdon

## Project Structure
- `datasets/` - Datasets
  - `scrubbed_datasets/` - Cleaned datasets ready for analysis
  - `raw_datasets/` - Original datasets
- `encodings/` - Contains the pre-processing .do files used to create 'raw_datasets'
- `output/` - Generated results
  - `metro/` - Metropolis acceptance ratios organized by country
  - `plots/` - Visualizations organized by country
  - `all/` - Visualizations for all countries
  - `poland/` - Poland output plots
    - `coef/` - Delta coefficient plots w/ covariates
    - `corr/` - Correlation heatmap of covariates
    - `covar/` - Covariate-speciic support probability graphs
    - `dist/` - Distribution analysis of questions and multi-lambda support coefficients
- `scripts/` - Analysis scripts organized by type
  - `all/` - Analysis scripts that are run on every country in the dataset
  - `poland/`
    - `poland_corr.R` - Produces correlation heatmap for the list of covariates; outputs to output folder
    - `poland_dist.R` - Produces plots for the survey distribution; outputs to output folder
    - `poland_endorsement.R` - Produces endorse effects; outputs to output folder
    - `poland_macro` - Produces question-by-question support levels; outputs to output folder
- `scrubbing/` - Scripts that use raw_datasets to produce scrubbed_datasets

- It is recommended that you clone this repository in ~/projects/AaD_Research/...

## Analysis Pipeline

### 1. Data Preparation
Run the data scrubbing scripts to clean and prepare the datasets:
```
Rscript scrubbing/poland_dataset_scrubbing.R
# Add other countries as needed
```

### 2. Distribution Analysis
Run the distribution scripts to understand data patterns:
```
Rscript scripts/poland/poland_dist.R
Rscript scripts/polanad/poland_macro.R
# Add other countries as needed
```

These scripts generate distribution analysis and comparison in the `output/plots/[country]/dist/` directories.

### 3. Endorsement Analysis
Run the endorsement analysis scripts:
```
Rscript scripts/poland/poland_endorsement.R
# Add other countries as needed
```

### 4. View Results
All generated plots and analysis outputs will be available in the `output/` directory, organized by country and analysis type.

## Dependencies
This project requires the following R packages:
- forcats
- reshape2
- ggpubr
- ggplot2
- haven
- dplyr
- endorse
- readxl
- tidyr
- Cairo
- missForest

Install them using:
```R
install.packages(c("forcats", "reshape2", "ggpubr", "ggplot2", "haven", "dplyr", "endorse", "readxl", "tidyr", "Cairo", "missForest"))
```

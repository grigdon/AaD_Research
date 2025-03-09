# Respository for the Armed and Dangerous Research Paper 

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
    - `poland/`
    - `slovakia/`
    - etc.
- `scripts/` - Analysis scripts organized by type
  - `poland/`
    - `poland_dist.R` - Produces plots for the survey distribution; outputs to output folder
    - `poland_endorse.R` - Produces endorse effects; outputs to output folder
  - `slovakia/`
    - etc.
  - `dist/` - Distribution analysis scripts
  - `endorsement/` - Endorsement analysis scripts
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
# Add other countries as needed
```

These scripts generate stacked bar graphs and comparative visualizations in the `output/plots/[country]/dist/` directories.

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

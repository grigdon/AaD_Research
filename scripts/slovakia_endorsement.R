# Complete Analysis Script for Militia Support in Slovakia
# This script processes survey data, fits endorsement models, and analyzes marginal effects
# for understanding factors that influence militia support

# Load required packages
library(endorse)
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
# 1. Data Loading and Initial Processing for Slovakia
#============================================

SKdata <- read_sav("~/projects/AaD_Research/datasets/scrubbed_datasets/slovakia_scrubbed_dataset.sav")

# Define questions for the endorsement experiment
questions <- c("id", "q10a_control", "q10b_control", "q10c_control", 
               "q10a_experiment", "q10b_experiment", "q10c_experiment")

# Select relevant columns for endorsement analysis
data_slvk_questions <- SKdata[questions]

# Define variables to keep
vars <- c("id", "male", "age", "educ", "capital", "ideology", "income", "DemPolGrievance", "PolicyPolGrievance",
      "DemonstrateTrad", "DemonstrateNational", "PetitionSameSex", "VoteFarRight", "VotePrevFarRight",
      "ideologyLC", "SocialMediaUse", "InternetUse", "SlovakNationality", "FAMincome", "Nationalist",
      "EconGrievanceRetro", "EconGrievanceProspInd", "EconGrievanceProspAgg", "EconGrievanceProspMostFams",
      "NatPride", "RomaPartner", "RomaNeighbor", "GayNeighbor", "GayFamily", "ForNeighbor", "ForPartner", "Ukraine",
      "ChristianSchool", "MaleChauvinism", "LawOrder", "ChurchPolitics", "Abortion", "TradMarriage", "SexbMarriage",
      "ChildHome", "MaleJobs", "NativeJobs", "NativeRights", "Religiosity")

# Subset and recode variables
data_slvk_vars <- SKdata[vars]



















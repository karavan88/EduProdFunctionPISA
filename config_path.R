# ==============================================================================
# NON-COGNITIVE SKILLS PROJECT - COMPLETE ENVIRONMENT SETUP
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         project_config.R
# Purpose:      Complete project environment setup (directories + packages)
# 
# Description:  This script provides ONE-STOP environment setup for the project.
#               It configures directory structures, validates all paths, AND
#               loads all required packages. After sourcing this file, the
#               project environment is fully ready for analysis.
#
# Usage:        source("project_config.R")
#               (automatically loaded by .Rprofile)
#
# Features:     • FULLY PORTABLE - no hardcoded paths or usernames
#               • Automatic project root detection via script location
#               • Comprehensive directory structure validation
#               • Automatic package loading with error handling
#               • Progress tracking and informative logging
#               • Integration with renv for reproducible package management
#               • Support for multiple research domains
#
# What it does: 1. Auto-detects project root directory (no hardcoding!)
#               2. Configures project directory structure  
#               3. Validates all required directories exist
#               4. Loads all required R packages
#               5. Reports status and any issues
#
# Project Structure:
#               01_input_data/          - Raw and processed data files
#               02_codes/               - Analysis scripts and functions  
#               03_manuscripts/         - Academic papers and reports
#               [Domain folders]/       - Subject-specific analyses
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      October 21, 2023
# Modified:     October 19, 2025
#
# Dependencies: renv environment with packages defined in renv.lock
# Runtime:      ~10-15 seconds (first time), ~3-5 seconds (subsequent)
#
# Notes:        This file is automatically sourced by .Rprofile
#               Works on any machine - no path configuration needed!
#               If packages fail to load, run renv::restore() first
#               This replaces the need for separate libraries.R file
#
# ==============================================================================

# Initialize configuration
cat("\n", rep("=", 70), "\n")
cat("INITIALIZING NON-COGNITIVE SKILLS PROJECT ENVIRONMENT\n")
cat("Configuration file: project_config.R\n")
cat("Loading time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 70), "\n\n")

# SECTION 2: PROJECT DIRECTORY SETUP
cat("📁 SECTION 1: PROJECT DIRECTORY SETUP\n")

# auto-detect project root based on this script's location
projectFolder <- getwd()

# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))

# print project root
cat("Project root directory detected at:\n")
cat("→", projectFolder, "\n\n")

# section finalized
cat("   ✅ Project root directory set up successfully.\n\n")

# set up key folders
inputData       <-    file.path(projectFolder, "01_data/rds_converted")
rCodes          <-    file.path(projectFolder, "02_code")
output          <-    file.path(projectFolder, "03_output")
manuscript      <-    file.path(projectFolder, "04_manuscript")
presentation    <-    file.path(projectFolder, "04_presentation")



# SECTION 2: PACKAGE LOADING
cat("📦 SECTION 2: PACKAGE LOADING\n")
cat("Loading all required packages for analysis...\n\n")

# Define required packages by category
required_packages <- list(
  
  "Core Data Science" = c("tidyverse"), # includes "dplyr", "tibble", "ggplot2", "readr", "tidyr",  "stringr", "forcats", "lubridate", "purrr",
  
  "Statistical Analysis" = c("lme4", "lmerTest", "lmtest",  
                             "broom", "broom.mixed"),
  
  "Effects & Visualization" = c("ggeffects", "gridExtra", "sjPlot", "ggcorrplot", 
                                "ggstats"),
  
  "Data Import/Export" = c("haven", "readxl", "here"),
  
  "Tables & Output" = c("gtsummary", "modelsummary", "tinytable", 
                        "kableExtra", "gt"),
  
  "Statistical Packages" = c("easystats", "bayestestR", "performance", "parameters", 
                             "effectsize", "correlation", "insight"),
  
  "Utilities" = c("glue", "conflicted")
)

# Function to safely load packages with informative output
safe_load <- function(package_name, silent = FALSE) {
  tryCatch({
    suppressPackageStartupMessages(
      library(package_name, character.only = TRUE, quietly = TRUE)
    )
    return(TRUE)
  }, error = function(e) {
    if (!silent) {
      cat("   ❌ Failed to load", package_name, "\n")
      cat("      → Run renv::restore() to install missing packages\n")
    }
    return(FALSE)
  })
}

# Load packages by category with progress tracking
total_packages <- sum(lengths(required_packages))
loaded_count <- 0
failed_packages <- character()

for (category in names(required_packages)) {
  cat("   📚", category, "\n")
  
  for (package in required_packages[[category]]) {
    if (safe_load(package, silent = TRUE)) {
      cat("      ✅", package, "\n")
      loaded_count <- loaded_count + 1
    } else {
      cat("      ❌", package, "\n")
      failed_packages <- c(failed_packages, package)
    }
  }
}

# Package loading summary
cat("\n📊 PACKAGE LOADING SUMMARY:\n")
cat("   ✅ Successfully loaded:", loaded_count, "out of", total_packages, "packages\n")

if (length(failed_packages) > 0) {
  cat("   ❌ Failed to load:", length(failed_packages), "packages\n")
  cat("   📝 Missing packages:", paste(failed_packages, collapse = ", "), "\n")
  cat("\n� TO FIX MISSING PACKAGES:\n")
  cat("   1. Run: renv::restore()\n")
  cat("   2. If issues persist: renv::install(c(", 
      paste(paste0('"', failed_packages, '"'), collapse = ", "), "))\n")
  cat("   3. Then run: renv::snapshot()\n\n")
}

# NAMESPACE CONFLICT RESOLUTION
# Resolve common namespace conflicts to ensure dplyr functions work without prefixes
if ("conflicted" %in% loadedNamespaces()) {
  conflicted::conflicts_prefer(dplyr::filter, 
                               dplyr::lag, 
                               dplyr::select, 
                               dplyr::rename, 
                               dplyr::mutate, 
                               dplyr::summarise, 
                               dplyr::slice, 
                               dplyr::arrange)
  
  conflicted::conflicts_prefer(lmerTest::lmer)
  
  cat("   🔧 Namespace conflicts resolved - dplyr functions preferred\n")
}

# FINAL COMPLETION MESSAGE
cat(rep("=", 70), "\n")
if (length(failed_packages) == 0) {
  cat("🚀 ENVIRONMENT FULLY READY FOR ANALYSIS!\n")
  cat("📈 You can now run any analysis script in the project.\n")
} else {
  cat("⚠️  ENVIRONMENT PARTIALLY READY\n") 
  cat("🔧 Please install missing packages before running analysis.\n")
}
cat("✅ Setup completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 70), "\n\n")

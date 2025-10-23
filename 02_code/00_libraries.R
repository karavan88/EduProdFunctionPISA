#-------------------------------------------------------------------
# Project: Education Production Function
# Script: Upload Libraries
# Author: Garen Avanesian
# Date: 16 September 2024
#-------------------------------------------------------------------

required_packages <- c("tidyverse",
                       "haven",
                       "lme4",
                       "lmerTest",
                       "sjPlot",
                       "ggcorrplot",
                       "ggeffects",
                       "gt",
                       "gtsummary",
                       "broom",
                       "broom.mixed",
                       "glue",
                       "psych",
                       "modelsummary",
                       "colorspace",
                       "performance")

# Function to check and install packages
check_and_install_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
}


# Call the function with the list of required packages
check_and_install_packages(required_packages)
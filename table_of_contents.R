# EWE Table of Contents

# Use renv::restore() to restore for lockfile. 
# Run housekeeping before any other script.


# Workflow ----------------------------------------------------------------


# Convenience functions, conflict management 
source('4_scripts/housekeeping.R')

# Clean datasets
source('4_scripts/cleaning.R')

# Exploratory factor analysis
source('4_scripts/efa.R')

# missForest imputation and Rasch modeling
source('4_scripts/impute_rasch.R')

# Product indicators for UPI moderation method
source('4_scripts/prep_sem_dat.R')

# Structural Equation Modeling
source('4_scripts/sem.R')

# Reporting results for paper
source('4_scripts/reporting.R')


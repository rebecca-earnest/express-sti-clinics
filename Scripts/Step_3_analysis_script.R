# Purpose: Calculate epidemiological and economic outcomes for express screening manuscript. 

# STEP 1: Load Libraries & Set Master File Paths --------------------------

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(knitr)
library(kableExtra)
library(scales)
library(viridis)
library(bayestestR)
library(readr)

files_path <- paste0(getwd(), "/")

# STEP 2: Economic Model ----------------------------------------------

costs <- read_csv(paste0(files_path, "Data/", "express_clinic_costs.csv"))

# Note: can generate these large files in Step_1 and Step_2
load(paste0(files_path, "Data/", "sim.no55.paper2.baseline.061420.rda"))
trad_sim <- sim$epi

load(paste0(files_path, "Data/", "sim.no55.paper2.express.061420.rda"))
xpr_sim <- sim$epi

source(paste0(files_path, "Scripts/", "economic_model.R"))

# STEP 3: Outcomes Model ----------------------------------------------

source(paste0(files_path, "Scripts/", "outcomes_model.R"))

# STEP 4: Economic / Outcome Calculations -------------------------------

load(paste0(files_path, "Output/Economic_Model/", "cumul_cost_diff_disc.rda")) # cumulative cost differences
load(paste0(files_path, "Output/Outcomes_Model/", "cumul_num_inf_averted_disc.rda")) # num inf averted
load(paste0(files_path, "Output/Outcomes_Model/", "cumul_num_case_averted_disc.rda")) # num cases averted

source(paste0(files_path, "Scripts/", "economic_outcomes_calcs.R"))

# STEP 5: Sensitivity Analysis --------------------------------------------

load(paste0(files_path, "Output/Economic_Model/", "total_costs_disc_trad.rda")) 
load(paste0(files_path, "Output/Economic_Model/", "total_costs_disc_xpr.rda")) 
load(paste0(files_path, "Output/Outcomes_Model/", "cumul_num_inf_averted_disc.rda")) 
load(paste0(files_path, "Output/Outcomes_Model/", "cumul_num_case_averted_disc.rda")) 

source(paste0(files_path, "Scripts/", "sens_analysis_all_vary.R"))

# STEP 6: Plot ------------------------------------------------------------

# Costs
output_path <- paste0(files_path, "Output/Economic_Model/")

file_names <- c(
  "scr_costs_disc_trad",
  "scr_costs_disc_xpr",
  "test_costs_disc_trad",
  "test_costs_disc_xpr",
  "tx_costs_disc_trad",
  "tx_costs_disc_xpr",
  "cumul_cost_diff_disc",
  "scr_costs_wk_combo", 
  "test_costs_wk_combo",
  "tx_costs_wk_combo"
)

for (file_name in file_names) {
  load(paste0(output_path, file_name, ".rda"))
}

# Outcomes
output_path <- paste0(files_path, "Output/Outcomes_Model/")

file_names <- c(
  "ir100_full_yr_trad",
  "ir100_full_yr_xpr",
  "ir100_week_combo",
  "prev_midpt_by_yr_trad",
  "prev_midpt_by_yr_xpr",
  "prev_all_weeks_both_scenario",
  "cumul_num_inf_averted_disc",
  "cumul_num_case_averted_disc"
)

for (file_name in file_names) {
  load(paste0(output_path, file_name, ".rda"))
}

# ICER
output_path <- paste0(files_path, "Output/Economic_Outcomes_Model/")

file_names <- list(
  "mean_CI_bootstrap_icer_results"
)

for (file_name in file_names) {
  load(paste0(output_path, file_name, ".rda"))
}

# Sensitivity analysis
output_path <- paste0(files_path, "Output/Sensitivity_Analysis/")

file_names <- list(
  "mean_CI_bootstrap_icer_results_sens_analysis"
  # "cumul_costs_trad_sens_analysis",
  # "cumul_costs_xpr_sens_analysis"
)

for (file_name in file_names) {
  load(paste0(output_path, file_name, ".rda"))
}

# Plotting
file_path_input <- paste0(files_path, "Scripts/plot_markdown.Rmd/")
file_path_output <- paste0(files_path, "Output/Plots/")

rmarkdown::render(input = file_path_input,
                  output_file = paste(Sys.Date(), ".pdf"),
                  output_dir = file_path_output)

# To compare every level of operational cost for baseline versus express models.

# STEP 1: Calculate Totaly Yearly Costs with Operational Cost Levels Included  ##########

# 2a - write function

calc_ops_costs <- function(total_yearly_costs_disc, cost_levels) {
  
  vary_costs_level_all <- NULL
  
  for (level in cost_levels) {
    
    yearly_list <- list()
    
    for (year in names(total_yearly_costs_disc)) {
      
      dat <- total_yearly_costs_disc[[year]]
      num_level <- as.numeric(level)
      dat <- dat + (dat * num_level)
      yearly_list[[year]] <- dat
      
    }
    
    vary_cost_level_single <- data.frame(do.call(rbind, yearly_list))
    vary_cost_level_single$Year <- rownames(vary_cost_level_single)
    vary_cost_level_single$Cost_Level <- level
    vary_costs_level_all <- rbind(vary_costs_level_all, vary_cost_level_single)
    
  }
  
  vary_costs_level_all
  
}

# 2b - apply to costs
cost_levels <- as.character(seq(from = 0.01, to = 0.10, by = 0.01))
costs_op_inc_base <- calc_ops_costs(total_costs_disc_trad, cost_levels)
costs_op_inc_comp <- calc_ops_costs(total_costs_disc_xpr, cost_levels)

# STEP 2: Calculate Cumulative Costs Under Each Operational Cost Level  ##########

# 3a - write function
drop_column <- function(data_file, col_name_vec) {
  
  drop_col <- col_name_vec
  data_file[ , !(names(data_file) %in% drop_col)]
  
}

accumul_icer_components <- function(yearly_costs_ops_inc) {
  
  acc_list_by_yr_level <- NULL

  for (level in unique(costs_op_inc_base$Cost_Level)) {
    
    acc_list_by_yr <- list()
    num_elements <- length(names(yearly_costs_ops_inc)) - 2 # for Year and Threshold
    current_values <- rep(0, num_elements)
    
    dat <- yearly_costs_ops_inc[which(yearly_costs_ops_inc$Cost_Level == level), ]  
    
    for (year in unique(costs_op_inc_base$Year)) {
      
      this_yr_values <- dat[which(dat$Year == year), ]
      this_yr_values <- drop_column(this_yr_values, c("Year", "Cost_Level"))
      
      current_values <- current_values + this_yr_values
      acc_list_by_yr[[year]] <- current_values
      
    }
    
    acc_list_by_yr_rbind <- do.call(rbind, acc_list_by_yr)
    acc_list_by_yr_rbind$Year <- rownames(acc_list_by_yr_rbind)
    acc_list_by_yr_rbind$Cost_Level <- level
    acc_list_by_yr_level <- rbind(acc_list_by_yr_level, acc_list_by_yr_rbind)
    
  }
  acc_list_by_yr_level
}

# 3b - apply to costs
cumul_costs_ops_inc_base <- accumul_icer_components(costs_op_inc_base)
cumul_costs_ops_inc_comp <- accumul_icer_components(costs_op_inc_comp)

# STEP 3: Calculate Cumulative Cost Difference for Each Base and Comp Combination of Operational Costs  ##########

# 4a - write function
calc_cumul_cost_diff_ops_inc <- function(cumul_costs_base, cumul_costs_comp) {
  
  all_base_fixed_levels <- NULL 
  
  for (fixed_level in unique(cumul_costs_base$Cost_Level)) {
    
    # fix baseline level
    base_level_fixed <- cumul_costs_base[which(cumul_costs_base$Cost_Level == fixed_level), ]
    cost_diff_all_comp_level <- NULL
    
    for (varied_level in unique(cumul_costs_base$Cost_Level)) {
      
      comp_level_fixed <- cumul_costs_comp[which(cumul_costs_comp$Cost_Level == varied_level), ]
      cost_diff_single_years_list <- list()
      cost_diff_single_comp_level <- NULL
      
      for (year in unique(cumul_costs_base$Year)) {
        
        base_level_fixed_single_yr <- base_level_fixed[which(base_level_fixed$Year == year), ]
        comp_level_fixed_single_yr <- comp_level_fixed[which(comp_level_fixed$Year == year), ]
        
        base_level_fixed_single_yr <- drop_column(base_level_fixed_single_yr, c("Year", "Threshold"))
        comp_level_fixed_single_yr <- drop_column(comp_level_fixed_single_yr, c("Year", "Threshold"))
        
        base_level_fixed_single_yr <- as.numeric(base_level_fixed_single_yr)
        comp_level_fixed_single_yr <- as.numeric(comp_level_fixed_single_yr)
        
        cost_diff_single_years_list[[year]] <- comp_level_fixed_single_yr - mean(base_level_fixed_single_yr)
        
      }
      
      cost_diff_single_comp_level <- data.frame(do.call(rbind, cost_diff_single_years_list))
      cost_diff_single_comp_level$Year <- rownames(cost_diff_single_comp_level)
      
      cost_diff_single_comp_level$Base_Fixed_Level <- fixed_level
      cost_diff_single_comp_level$Comp_Varied_Level <- varied_level
      
      cost_diff_all_comp_level <- rbind(cost_diff_all_comp_level, cost_diff_single_comp_level)
      
    }
    
    all_base_fixed_levels <- rbind(all_base_fixed_levels, cost_diff_all_comp_level)
    
  }
  
  all_base_fixed_levels 
  
}
    
# 4b - apply to costs
cost_diff_all_level_combos <- calc_cumul_cost_diff_ops_inc(cumul_costs_ops_inc_base, cumul_costs_ops_inc_comp)

# 4c - check whether worked
unique(cost_diff_all_level_combos$Base_Fixed_Level)
unique(cost_diff_all_level_combos$Comp_Varied_Level)

value <- 0.10
test <- cost_diff_all_level_combos[which(cost_diff_all_level_combos$Base_Fixed_Level == value), ]
test[, c("Year", "Base_Fixed_Level", "Comp_Varied_Level")]

nrow(cost_diff_all_level_combos)

# STEP 4: Calculate ICER Each Base and Comp Combination of Operational Costs Using Bootstrap Resampling ##########

# 4a - write function
# Note: Bootstrap Sample 100x from 1) Cumulative Discounted Cost Differences and 2) Cumulative Discounted Averted a) Infections and b) Cases #################

rbind_and_add_col_name <- function(data_file, column_to_add) {
  
  data_file <- do.call(rbind, data_file)
  data_file[, column_to_add] <- column_to_add
}

rbind_take_mean_CI <- function(data_file) {
  
  mean_se(do.call(rbind, data_file), mult = 1.96)
  
}

bootstrap_icer <- function(cost_diff_file, inf_avert_file, case_avert_file) {
  
  mean_cost_diff_rbind <- list()
  mean_inf_avert_rbind <- list()
  mean_case_avert_rbind <- list()
  
  mean_icer_inf_rbind <- list()
  mean_icer_case_rbind <- list()
  
  mean_CI_boostrap_icer <- data.frame()
  
  year_start <- 1
  year_end <- 5
  
  for (row in 1:nrow(cost_diff_file)) {
    
    mean_cost_diff <- list()
    mean_inf_avert <- list()
    mean_case_avert <- list()
    mean_icer_inf <- list()
    mean_icer_case <- list()
    
    cost_diff_single_row <- cost_diff_file[row, ]
    row_year <- cost_diff_single_row[, "Year"]
    
    inf_avert_by_yr <- inf_avert_file[[row_year]]
    case_avert_by_yr <- case_avert_file[[row_year]]
    
    costs_only <- as.numeric(drop_column(cost_diff_single_row, c("Year", "Base_Fixed_Level", "Comp_Varied_Level")))
    
    for (k in 1:100) {
      
      samples_cost_diff <- sample(costs_only, size = 128, replace = TRUE)
      samples_inf_avert <- sample(inf_avert_by_yr, size = 128, replace = TRUE)
      samples_case_avert <- sample(case_avert_by_yr, size = 128, replace = TRUE)
      
      mean_cost_diff[[k]] <- mean(samples_cost_diff)
      mean_inf_avert[[k]] <- mean(samples_inf_avert)
      mean_case_avert[[k]] <- mean(samples_case_avert)
      
      mean_icer_inf[[k]] <- mean_cost_diff[[k]] / mean_inf_avert[[k]] 
      mean_icer_case[[k]] <- mean_cost_diff[[k]] / mean_case_avert[[k]] 
      
    }
    
    # rbind and take mean and 95% over 100 bootstrap resamples by row
    mean_cost_diff_rbind[[row]] <- cbind(rbind_take_mean_CI(mean_cost_diff), 
                                         cost_diff_single_row$Year,
                                         cost_diff_single_row$Base_Fixed_Level, 
                                         cost_diff_single_row$Comp_Varied_Level)
    
    mean_inf_avert_rbind[[row]] <- cbind(rbind_take_mean_CI(mean_inf_avert), 
                                         cost_diff_single_row$Year,
                                         cost_diff_single_row$Base_Fixed_Level, 
                                         cost_diff_single_row$Comp_Varied_Level)
    
    mean_case_avert_rbind[[row]] <- cbind(rbind_take_mean_CI(mean_case_avert), 
                                          cost_diff_single_row$Year,
                                          cost_diff_single_row$Base_Fixed_Level, 
                                          cost_diff_single_row$Comp_Varied_Level)
    
    mean_icer_inf_rbind[[row]] <-  cbind(rbind_take_mean_CI(mean_icer_inf), 
                                         cost_diff_single_row$Year,
                                         cost_diff_single_row$Base_Fixed_Level, 
                                         cost_diff_single_row$Comp_Varied_Level)
    
    mean_icer_case_rbind[[row]] <-  cbind(rbind_take_mean_CI(mean_icer_case), 
                                          cost_diff_single_row$Year,
                                          cost_diff_single_row$Base_Fixed_Level, 
                                          cost_diff_single_row$Comp_Varied_Level)
    
  }
  
  # rbind all the years
  mean_cost_diff_rbind_rows <- do.call(rbind, mean_cost_diff_rbind)
  mean_inf_avert_rbind_rows <- do.call(rbind, mean_inf_avert_rbind)
  mean_case_avert_rbind_rows <- do.call(rbind, mean_case_avert_rbind)
  
  mean_icer_inf_rbind_rows <- do.call(rbind, mean_icer_inf_rbind)
  mean_icer_case_rbind_rows <- do.call(rbind, mean_icer_case_rbind)
  
  # add column for data type
  mean_cost_diff_rbind_rows$Type <- "cost_diff"
  mean_inf_avert_rbind_rows$Type <- "inf_avert"
  mean_case_avert_rbind_rows$Type <- "case_avert"
  
  mean_icer_inf_rbind_rows$Type <- "inf_icer"
  mean_icer_case_rbind_rows$Type <- "case_icer"
  
  mean_CI_boostrap_icer_rows <- rbind(
    mean_cost_diff_rbind_rows,
    mean_inf_avert_rbind_rows,
    mean_case_avert_rbind_rows,
    mean_icer_inf_rbind_rows,
    mean_icer_case_rbind_rows, stringsAsFactors = FALSE)
  
  mean_CI_boostrap_icer_rows
  
}

# 4b - apply to data
mean_CI_bootstrap_icer_results_sens_analysis <- bootstrap_icer(cost_diff_all_level_combos, cumul_num_inf_averted_disc, cumul_num_case_averted_disc)

# 4c - check if looks correct
nrow(mean_CI_bootstrap_icer_results_sens_analysis) # should be 2500 - 500 cost differences * 5 values outputted
test <- mean_CI_bootstrap_icer_results_sens_analysis[which(mean_CI_bootstrap_icer_results_sens_analysis$`cost_diff_single_row$Base_Fixed_Level` == "0.01"), ]
test

# STEP 5: Save Down Files #################
output_path <- paste0(files_path, "Output/Sensitivity_Analysis/")
save(mean_CI_bootstrap_icer_results_sens_analysis, file = paste0(output_path, "mean_CI_bootstrap_icer_results_sens_analysis", ".rda"))


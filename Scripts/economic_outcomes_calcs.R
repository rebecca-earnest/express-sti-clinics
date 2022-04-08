# Calculate 
# Input = economic model output (screening, testing, treatment cot=sts), outcomes model output (cases and infections averted)

# Note:
## Infection = each new site-specific infection
## Case = each new infection in a totally uninfected person (so triple-site infection counts 1x)

# STEP 1: Load libraries & files #################

# 1st - load economic and outcomes model output

# STEP 2: Bootstrap Sample 100x from 1) Cumulative Discounted Cost Differences and 2) Cumulative Discounted Averted a) Infections and b) Cases #################

bootstrap_icer <- function(cost_diff_file, inf_avert_file, case_avert_file) {
  
  mean_cost_diff_rbind <- list()
  mean_inf_avert_rbind <- list()
  mean_case_avert_rbind <- list()
  
  mean_icer_inf_rbind <- list()
  mean_icer_case_rbind <- list()
  
  mean_CI_boostrap_icer <- list()
  
  for (year in names(cost_diff_file)) {
    
    mean_cost_diff <- list()
    mean_inf_avert <- list()
    mean_case_avert <- list()
    
    mean_icer_inf <- list()
    mean_icer_case <- list()
    
    for (k in 1:100) {
      
      samples_cost_diff <- sample(cost_diff_file[[year]], size = 128, replace = TRUE)
      samples_inf_avert <- sample(inf_avert_file[[year]], size = 128, replace = TRUE)
      samples_case_avert <- sample(case_avert_file[[year]], size = 128, replace = TRUE)
      
      mean_cost_diff[[k]] <- mean(samples_cost_diff)
      mean_inf_avert[[k]] <- mean(samples_inf_avert)
      mean_case_avert[[k]] <- mean(samples_case_avert)
      
      mean_icer_inf[[k]] <- mean_cost_diff[[k]] / mean_inf_avert[[k]] 
      mean_icer_case[[k]] <- mean_cost_diff[[k]] / mean_case_avert[[k]] 
      
    }
    
    mean_cost_diff_rbind[[year]] <- mean_se(do.call(rbind, mean_cost_diff), mult = 1.96)
    mean_inf_avert_rbind[[year]] <-  mean_se(do.call(rbind, mean_inf_avert), mult = 1.96)
    mean_case_avert_rbind[[year]] <-  mean_se(do.call(rbind, mean_case_avert), mult = 1.96)

    mean_icer_inf_rbind[[year]] <-  mean_se(do.call(rbind, mean_icer_inf), mult = 1.96)
    mean_icer_case_rbind[[year]] <-  mean_se(do.call(rbind, mean_icer_case), mult = 1.96)
  
    mean_CI_boostrap_icer[["cost_diff"]] <- do.call(rbind, mean_cost_diff_rbind)
    mean_CI_boostrap_icer[["inf_avert"]] <- do.call(rbind, mean_inf_avert_rbind)
    mean_CI_boostrap_icer[["case_avert"]] <- do.call(rbind, mean_case_avert_rbind)
    
    mean_CI_boostrap_icer[["inf_icer"]] <- do.call(rbind, mean_icer_inf_rbind)
    mean_CI_boostrap_icer[["case_icer"]] <- do.call(rbind, mean_icer_case_rbind)
    

  }

  mean_CI_boostrap_icer
  
}

mean_CI_bootstrap_icer_results <- bootstrap_icer(cumul_cost_diff_disc, cumul_num_inf_averted_disc, cumul_num_case_averted_disc)


# STEP 4: Save Down Files #################

output_path <- paste0(files_path, "Output/Economic_Outcomes_Model/", "mean_CI_bootstrap_icer_results.rda")

save(mean_CI_bootstrap_icer_results, file = output_path)
  





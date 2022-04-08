# Input = baseline and express model volumes

# Note:
## Infection = each new site-specific infection
## Case = each new infection in a totally uninfected person (so triple-site infection counts 1x)

# Note: infections calculations example
## Prevalence: prev.ugc
## IR100: (incid.ugc / (num - i.num.ugc)) * (52*100)

# Note: case calculations example
## Prevalence: prev.gc
## IR100: # sum over cases and person time at risk: multiply by 52 to convert weeks into years, 52 p-w/1 p-y, and by 100 to get per 100 person-years
 ## (incid.gc.indiv / total.susc.pop) * (52*100)

keep_vec_ir100_inf_numer <- c(
  "incid.ugc",
  "incid.rgc",
  "incid.ogc",
  "incid.gc")

keep_vec_ir100_inf_denom <- c(
  "num",
  "i.num.ugc",
  "i.num.rgc",
  "i.num.ogc"
)

keep_vec_ir100_case_numer <- c(
  "incid.gc.indiv"
)

keep_vec_ir100_case_denom <- c(
  "total.susc.pop" 
)

keep_vec_prev <- c(

  "prev.ugc",
  "prev.rgc", 
  "prev.ogc", 
  "prev.gc" 
  
)

select_df <- function(keep_vec, sim) {
  
  sub_list <- list()
  
  for (vec in keep_vec) {
    
    my_df <- sim[[vec]]
    my_df$time_step <- seq(from = 1, to = nrow(my_df))
    my_df_time_keep <- my_df[which(my_df$time_step > 3120), ]
    drop_col <- "time_step"
    my_df_time_keep <- my_df_time_keep[, !(names(my_df_time_keep) %in% drop_col)]
    sub_list[[vec]] <- my_df_time_keep
    
  }
  sub_list
}

trad_select_ir100_inf_numer <- select_df(keep_vec_ir100_inf_numer, trad_sim)
xpr_select_ir100_inf_numer <- select_df(keep_vec_ir100_inf_numer, xpr_sim)

trad_select_ir100_inf_denom <- select_df(keep_vec_ir100_inf_denom, trad_sim)
xpr_select_ir100_inf_denom <- select_df(keep_vec_ir100_inf_denom, xpr_sim)

trad_select_ir100_case_numer <- select_df(keep_vec_ir100_case_numer, trad_sim)
xpr_select_ir100_case_numer <- select_df(keep_vec_ir100_case_numer, xpr_sim)

trad_select_ir100_case_denom <- select_df(keep_vec_ir100_case_denom, trad_sim)
xpr_select_ir100_case_denom <- select_df(keep_vec_ir100_case_denom, xpr_sim)

trad_select_prev <- select_df(keep_vec_prev, trad_sim)
xpr_select_prev <- select_df(keep_vec_prev, xpr_sim)


# STEP 1: Calculate Incidence per 100 Person-Years and Prevalence by Year #################

# Note: Incidence per 100 PY will be calculated for each simulation as the average of the 3 months.
# Note: Prevalence will be calculated as the final weekly value in each year. 

split_out_site <- function(s) {
  
  # split string `s` on the character "."
  # we add \\ because we want strsplit to treat is as a literal not a regular expression
  parts <- strsplit(s, "\\.")[[1]]
  num_parts <- length(parts)
  
  # turn the string parts into a prefix again by "pasting" with "." in between
  prefix <- paste0(parts[1:(num_parts - 1)], collapse=".")
  site <- parts[num_parts]
  
  # return both the prefix and the race
  c(prefix, site)
}

clean_df_time_years <- function(dat) {
  
  dat <- dat[1:260, ]
  weeks <- 1:nrow(dat)
  weeks_with_year <- floor((weeks -1) / 52) + 1
  dat$Year <- weeks_with_year
  dat_melt <- melt(dat, id.vars = "Year", variable.name = "Simulation", value.name = "Value")
  
}

ir100_inf_full_year <- function(ir100_inf_numer, ir100_inf_denom) {
  
  ir100_by_df <- list()
  
  for (numer in names(ir100_inf_numer)) {
    
    my_numer <- ir100_inf_numer[[numer]]
    my_numer_melt <- clean_df_time_years(my_numer)
    
    for (denom in names(ir100_inf_denom)) {
      
      split_result_denom <- split_out_site(denom)[[2]]
      split_result_numer <- split_out_site(numer)[[2]]
      
      if (split_result_denom == split_result_numer) {
        
        denom_keep <- ir100_inf_denom[[denom]]
        denom_num <- ir100_inf_denom[["num"]]
        
        df_columns <-  colnames(denom_num)
        my_denom <- denom_num[ , df_columns] - denom_keep[, df_columns]
        my_denom_melt <- clean_df_time_years(my_denom)
        
      }
    }
    
    sum_numer <- ddply(my_numer_melt, .(Simulation, Year), summarize, Value = sum(Value))
    sum_denom <- ddply(my_denom_melt, .(Simulation, Year), summarize, Value = sum(Value))
    sum_combo <- left_join(sum_numer, sum_denom, by = c("Simulation", "Year"))
    sum_combo_ir100 <- ddply(sum_combo, .(Simulation, Year), summarize, Value = (Value.x/Value.y) * 5200)
    ir100_by_df[[numer]] <- sum_combo_ir100
    
  }
  ir100_by_df
}

ir100_inf_full_yr_trad <- ir100_inf_full_year(trad_select_ir100_inf_numer, trad_select_ir100_inf_denom)
ir100_inf_full_yr_xpr <- ir100_inf_full_year(xpr_select_ir100_inf_numer, xpr_select_ir100_inf_denom)

ir100_case_full_year <- function(ir100_case_numer, ir100_case_denom) {
  
  for (name in names(ir100_case_numer)) {
    
    my_numer <- ir100_case_numer[[name]]
    my_numer_melt <- clean_df_time_years(my_numer)
    sum_numer <- ddply(my_numer_melt, .(Simulation, Year), summarize, Value = sum(Value))
    
  }
  
  for (name in names(ir100_case_denom)) {
    
    my_denom <- ir100_case_denom[[name]]
    my_denom_melt <- clean_df_time_years(my_denom)
    sum_denom <- ddply(my_denom_melt, .(Simulation, Year), summarize, Value = sum(Value))
    
  }
  
  sum_combo <- left_join(sum_numer, sum_denom, by = c("Simulation", "Year"))
  sum_combo_ir100 <- ddply(sum_combo, .(Simulation, Year), summarize, Value = (Value.x/Value.y) * 5200)
  sum_combo_ir100
  
}

ir100_case_full_yr_trad <- ir100_case_full_year(trad_select_ir100_case_numer, trad_select_ir100_case_denom)
ir100_case_full_yr_xpr <- ir100_case_full_year(xpr_select_ir100_case_numer, xpr_select_ir100_case_denom)

#
clean_df_time_weeks <- function(dat) {
  
  dat <- dat[1:260, ]
  dat$Week <- 1:nrow(dat)
  dat_melt <- melt(dat, id.vars = "Week", variable.name = "Simulation", value.name = "Value")
  dat_melt
}

ir100_inf_each_week <- function(ir100_inf_numer, ir100_inf_denom) {
  
  ir100_by_df <- list()
  
  for (numer in names(ir100_inf_numer)) {
    
    my_numer <- ir100_inf_numer[[numer]]
    my_numer_melt <- clean_df_time_weeks(my_numer)
    
    for (denom in names(ir100_inf_denom)) {
      
      split_result_denom <- split_out_site(denom)[[2]]
      split_result_numer <- split_out_site(numer)[[2]]
      
      if (split_result_denom == split_result_numer) {
        
        denom_keep <- ir100_inf_denom[[denom]]
        denom_num <- ir100_inf_denom[["num"]]
        
        df_columns <-  colnames(denom_num)
        my_denom <- denom_num[ , df_columns] - denom_keep[, df_columns]
        my_denom_melt <- clean_df_time_weeks(my_denom)
        my_denom_melt$Type <- split_result_denom
      }
    }
    
    my_numer_melt$Type <- split_result_numer
    
    # sum_numer <- ddply(my_numer_melt, .(Simulation, Week), summarize, Value = sum(Value))
    # sum_denom <- ddply(my_denom_melt, .(Simulation, Week), summarize, Value = sum(Value))
    sum_combo <- left_join(my_numer_melt, my_denom_melt, by = c("Simulation", "Week", "Type"))
    sum_combo_ir100 <- ddply(sum_combo, .(Simulation, Week, Type), summarize, Value = (Value.x/Value.y) * 5200)
    ir100_by_df[[numer]] <- sum_combo_ir100
    
  }
  ir100_by_df
}

# TODO - change name
ir100_inf_week_trad <- ir100_inf_each_week(trad_select_ir100_inf_numer, trad_select_ir100_inf_denom)
ir100_inf_week_xpr <- ir100_inf_each_week(xpr_select_ir100_inf_numer, xpr_select_ir100_inf_denom)

ir100_inf_week_trad_rbind <- do.call(rbind, ir100_inf_week_trad)
ir100_inf_week_xpr_rbind <- do.call(rbind, ir100_inf_week_xpr)

ir100_inf_week_trad_rbind$Scenario <- "Traditional"
ir100_inf_week_xpr_rbind$Scenario <- "Express"

ir100_inf_week_combo <- rbind(ir100_inf_week_trad_rbind, ir100_inf_week_xpr_rbind)
ir100_inf_week_combo <- ir100_inf_week_combo[which(ir100_inf_week_combo$Type != "gc"), ]

ir100_case_each_week <- function(ir100_case_numer, ir100_case_denom) {
  
  for (name in names(ir100_case_numer)) {
    
    my_numer <- ir100_case_numer[[name]]
    my_numer_melt <- clean_df_time_weeks(my_numer)
    # sum_numer <- ddply(my_numer_melt, .(Simulation, Week), summarize, Value = sum(Value))
    
    split_result_numer <- split_out_site(name)[[2]]
    my_numer_melt$Type <- split_result_numer
    
  }
  
  for (name in names(ir100_case_denom)) {
    
    my_denom <- ir100_case_denom[[name]]
    my_denom_melt <- clean_df_time_weeks(my_denom)
    # sum_denom <- ddply(my_denom_melt, .(Simulation, Year), summarize, Value = sum(Value))
    
    split_result_denom <- split_out_site(name)[[2]]
    my_denom_melt$Type <- split_result_denom
    
  }
  
  
  sum_combo <- left_join(my_numer_melt, my_denom_melt, by = c("Simulation", "Week"))
  sum_combo_ir100 <- ddply(sum_combo, .(Simulation, Week), summarize, Value = (Value.x/Value.y) * 5200)
  sum_combo_ir100
  
}

ir100_case_week_yr_trad <- ir100_case_each_week(trad_select_ir100_case_numer, trad_select_ir100_case_denom)
ir100_case_week_yr_xpr <- ir100_case_each_week(xpr_select_ir100_case_numer, xpr_select_ir100_case_denom)

ir100_case_week_yr_trad$Scenario <- "Traditional"
ir100_case_week_yr_xpr$Scenario <- "Express"

ir100_case_week_combo <- rbind(ir100_case_week_yr_trad, ir100_case_week_yr_xpr)
ir100_case_week_combo$Type <- "gc"

ir100_week_combo <- rbind(ir100_inf_week_combo, ir100_case_week_combo)

#

prev_midpt_each_year <- function(prev_file) {
  
  prev_by_df <- list()
  
  for (df in names(prev_file)) {
    
    my_df <- prev_file[[df]]
    my_df <- my_df[1:260, ]
    weeks <- 1:nrow(my_df)
    weeks_with_year <- floor((weeks -1) / 52) + 1
    my_df$Year <- weeks_with_year
    my_df$Week <- 3121:3380
    
    my_df_melt <- melt(my_df, id.vars = c("Year", "Week"), variable.name = "Simulation", value.name = "Value")
    midpt_row <- ddply(my_df_melt, .(Simulation, Year), function(x) x[floor(median(1:nrow(x))), ])
    prev_by_df[[df]] <- midpt_row
  }
  prev_by_df
}

prev_midpt_by_yr_trad <- prev_midpt_each_year(trad_select_prev)
prev_midpt_by_yr_xpr <- prev_midpt_each_year(xpr_select_prev)

prev_all_weeks <- function(prev_file) {
  
  prev_by_df <- list()
  
  for (df in names(prev_file)) {
    
    my_df <- prev_file[[df]]
    my_df <- my_df[1:260, ]
    weeks <- 1:nrow(my_df)
    weeks_with_year <- floor((weeks -1) / 52) + 1
    my_df$Year <- weeks_with_year
    my_df$Week <- 1:nrow(my_df)

    my_df_melt <- melt(my_df, id.vars = c("Year", "Week"), variable.name = "Simulation", value.name = "Value")
    my_df_melt$Type <- df
    prev_by_df[[df]] <- my_df_melt
  }
  prev_by_df
}

trad_select_prev_transform <- prev_all_weeks(trad_select_prev)
xpr_select_prev_transform <- prev_all_weeks(xpr_select_prev)

trad_select_prev_transform_rbind <- do.call(rbind, trad_select_prev_transform)
xpr_select_prev_transform_rbind <- do.call(rbind, xpr_select_prev_transform)

trad_select_prev_transform_rbind$Scenario <- "Traditional"
xpr_select_prev_transform_rbind$Scenario <- "Express"

prev_all_weeks_both_scenario <- rbind(trad_select_prev_transform_rbind, xpr_select_prev_transform_rbind)

# STEP 2: Calculate Cumulative Infections and Cases Averted by Year #################ß

# 1st - sum incidence infections by year over simulations

sum_by_year_within_sim <- function(sim) {
  
  colsum_list <- list()
  
  for (df in names(sim)) {
    
    my_df <- sim[[df]]
    my_df <- my_df[1:260, ]
    weeks <- 1:nrow(my_df) 
    weeks_with_year <- floor((weeks -1) / 52) + 1
    my_df$Year <- weeks_with_year
    
    my_df_melt <- melt(my_df, id.vars = "Year", variable.name = "Simulation", value.name = "Count")
    colsum_list[[df]] <- ddply(my_df_melt, .(Simulation, Year), summarize, Value = sum(Count))
  }
  colsum_list
}

incid_inf_sum_by_yr_trad <- sum_by_year_within_sim(trad_select_ir100_inf_numer)
incid_inf_sum_by_yr_xpr <- sum_by_year_within_sim(xpr_select_ir100_inf_numer)

incid_case_sum_by_yr_trad <- sum_by_year_within_sim(trad_select_ir100_case_numer)
incid_case_sum_by_yr_xpr <- sum_by_year_within_sim(xpr_select_ir100_case_numer)

# 2nd - grab part of total infections and cases averted per year overall (using discounted values)

# Note: total cases does not equal ugc cases + rgc cases + ogc cases because a case is one OR MORE infection is a totally susceptible person (i.e. uninfected at all three anatomic sites). Therefore, a person could be totally susceptible and be infected at two sites at once, which would count as 1 in the total cases but as 1 each under each site-specific case count.  

# Note: incid.gc = (num UGC infections) + (num RGC infections) + (num OGC infections)
gc_inf_incid_sum_by_yr_trad <- incid_inf_sum_by_yr_trad$incid.gc
gc_case_incid_sum_by_yr_trad <- incid_case_sum_by_yr_trad$incid.gc.indiv

gc_inf_incid_sum_by_yr_xpr <- incid_inf_sum_by_yr_xpr$incid.gc
gc_case_incid_sum_by_yr_xpr <- incid_case_sum_by_yr_xpr$incid.gc.indiv

# 3rd calculate cumulative infections and cases by year over simulations

accumul_icer_components <- function(yearly_values) {
  
  n_years <- 5
  num_elements <- nrow(yearly_values) / n_years
  current_values <- rep(0, num_elements)
  
  years <- 1:5
  accumul_list_by_yr <- list()
  
  for (year in years) {
    
    this_yr_values <- yearly_values[which(yearly_values$Year == year), "Value"]
    current_values <- current_values + this_yr_values
    list_name <- paste0("year", year)
    accumul_list_by_yr[[list_name]] <- current_values
    
  }
  accumul_list_by_yr
}

cumul_gc_inf_incid_sum_by_yr_trad <- accumul_icer_components(gc_inf_incid_sum_by_yr_trad)
cumul_gc_case_incid_sum_by_yr_trad <- accumul_icer_components(gc_case_incid_sum_by_yr_trad)

cumul_gc_inf_incid_sum_by_yr_xpr <- accumul_icer_components(gc_inf_incid_sum_by_yr_xpr)
cumul_gc_case_incid_sum_by_yr_xpr <- accumul_icer_components(gc_case_incid_sum_by_yr_xpr)

# STEP 3: Apply benefit discounting #################

# Note: midpoint method using 3% benefit discounting: Benefit / (1 + Discount Rate) ^ ((Year - Current Year) - 0.5)

# 1st - write discounting function

apply_benefit_discounting <- function(benefits) {
  
  current_year <- 2020
  year_dates <- list(
    
    year1 = current_year + 1,
    year2 = current_year + 2,
    year3 = current_year + 3,
    year4 = current_year + 4,
    year5 = current_year + 5
    
  )
  
  discount_rate <- 0.03
  disc_benefits <- list()
  
  for (year in names(benefits)) {
    
    benefits_year <- benefits[[year]]
    year_date <- year_dates[[year]]
    year_name <- year
    
    disc_benefits[[year_name]] <- benefits_year / ((1 + discount_rate)^((year_date - current_year) - 0.5))
    
  }
  disc_benefits
}

# 2nd - apply discounting function to benefits

cumul_gc_inf_incid_sum_by_yr_disc_trad <- apply_benefit_discounting(cumul_gc_inf_incid_sum_by_yr_trad)
cumul_gc_case_incid_sum_by_yr_disc_trad <- apply_benefit_discounting(cumul_gc_case_incid_sum_by_yr_trad)
cumul_gc_inf_incid_sum_by_yr_disc_xpr <- apply_benefit_discounting(cumul_gc_inf_incid_sum_by_yr_xpr)
cumul_gc_case_incid_sum_by_yr_disc_xpr <- apply_benefit_discounting(cumul_gc_case_incid_sum_by_yr_xpr)

# STEP 4: Calculate Cumulative Cases and Infections Averted By Site For Each Simulation #################

# Note: calculated as mean(base_model) - comp_model; this ensures that always comparing to same value

calc_averted <- function(base_sim, comp_sim) {
  
  avert_list <- list()
  
  for (year in names(base_sim)) {
    
    mean_num_avert_by_yr <- mean(base_sim[[year]]) - comp_sim[[year]]
    avert_list[[year]] <- mean_num_avert_by_yr
    
  }
  avert_list
}

cumul_num_inf_averted_disc <- calc_averted(cumul_gc_inf_incid_sum_by_yr_disc_trad, cumul_gc_inf_incid_sum_by_yr_disc_xpr)
cumul_num_case_averted_disc <- calc_averted(cumul_gc_case_incid_sum_by_yr_disc_trad, cumul_gc_case_incid_sum_by_yr_disc_xpr)


# STEP 4: Save Data #################

# combine lists
ir100_full_yr_trad <- list()

ir100_full_yr_trad[["incid.ugc"]] <- ir100_inf_full_yr_trad$incid.ugc
ir100_full_yr_trad[["incid.rgc"]] <- ir100_inf_full_yr_trad$incid.rgc
ir100_full_yr_trad[["incid.ogc"]] <- ir100_inf_full_yr_trad$incid.ogc
ir100_full_yr_trad[["incid.gc"]] <- ir100_case_full_yr_trad # this is case incidence, but name this way for plot file

ir100_full_yr_xpr <- list()

ir100_full_yr_xpr[["incid.ugc"]] <- ir100_inf_full_yr_xpr$incid.ugc
ir100_full_yr_xpr[["incid.rgc"]] <- ir100_inf_full_yr_xpr$incid.rgc
ir100_full_yr_xpr[["incid.ogc"]] <- ir100_inf_full_yr_xpr$incid.ogc
ir100_full_yr_xpr[["incid.gc"]] <- ir100_case_full_yr_xpr # this is 

file_names <- list(
  "ir100_full_yr_trad" = ir100_full_yr_trad,
  "ir100_full_yr_xpr" = ir100_full_yr_xpr,
  "ir100_week_combo" = ir100_week_combo,
  "prev_midpt_by_yr_trad" = prev_midpt_by_yr_trad,
  "prev_midpt_by_yr_xpr" = prev_midpt_by_yr_xpr,
  "prev_all_weeks_both_scenario" = prev_all_weeks_both_scenario,
  "cumul_num_inf_averted_disc" = cumul_num_inf_averted_disc,
  "cumul_num_case_averted_disc" = cumul_num_case_averted_disc
  
)

output_path <- paste0(files_path, "/Output/Outcomes_Model/")

for (file_name in names(file_names)) {
  
  data_object <- file_names[[file_name]]
  save(data_object, list = c(file_name), file = paste0(output_path, file_name, ".rda"))
  
}



  

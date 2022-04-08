# Calculate the total costs for each pathway in years 1 - 5 after the express pathway is implemented.
# Input = 2020 US costs; baseline and express model volumes

# STEP 1: Load libraries & files #################

trad_sim_names <- c(
  "num.dx.asympt.ugc.only.num.inf",
  "num.dx.asympt.ugc.only",
  "num.dx.asympt.ugc.rgc.num.inf.ugc",
  "num.dx.asympt.ugc.rgc.num.inf.rgc",
  "num.dx.asympt.ugc.rgc.num.inf.both",
  "num.dx.asympt.ugc.rgc.num.inf.none",
  "num.dx.asympt.ugc.ogc.num.inf.ugc",
  "num.dx.asympt.ugc.ogc.num.inf.ogc",
  "num.dx.asympt.ugc.ogc.num.inf.both",
  "num.dx.asympt.ugc.ogc.num.inf.none",
  "num.dx.asympt.triple.site.num.inf.ugc",
  "num.dx.asympt.triple.site.num.inf.rgc",
  "num.dx.asympt.triple.site.num.inf.ogc",
  "num.dx.asympt.triple.site.num.inf.ugc.rgc",
  "num.dx.asympt.triple.site.num.inf.ugc.ogc",
  "num.dx.asympt.triple.site.num.inf.rgc.ogc",
  "num.dx.asympt.triple.site.num.inf.triple.site",
  "num.dx.asympt.triple.site.num.inf.none",
  "num.trad.tx",
  "num.dx.sympt.ugc.only.num.inf",
  "num.dx.sympt.rgc.only.num.inf",
  "num.dx.sympt.ogc.only.num.inf",
  "num.dx.sympt.ugc.rgc",
  "num.dx.sympt.ugc.ogc",
  "num.dx.sympt.rgc.ogc",
  "num.dx.sympt.triple.site"
)

xpr_sim_names <- c(
  "num.dx.asympt.triple.site.num.inf.ugc",
  "num.dx.asympt.triple.site.num.inf.rgc",
  "num.dx.asympt.triple.site.num.inf.ogc",
  "num.dx.asympt.triple.site.num.inf.ugc.rgc",
  "num.dx.asympt.triple.site.num.inf.ugc.ogc",
  "num.dx.asympt.triple.site.num.inf.rgc.ogc",
  "num.dx.asympt.triple.site.num.inf.triple.site",
  "num.dx.asympt.triple.site.num.inf.none",
  "num.trad.tx",
  "num.dx.sympt.ugc.only.num.inf",
  "num.dx.sympt.rgc.only.num.inf",
  "num.dx.sympt.ogc.only.num.inf",
  "num.dx.sympt.ugc.rgc",
  "num.dx.sympt.ugc.ogc",
  "num.dx.sympt.rgc.ogc",
  "num.dx.sympt.triple.site"
)

# add race specifier b/c that is how data is stored from models

add_race_groups_to_df <- function(sim_names) {
  my_vec <- c()

  for (name in sim_names) {
    
    race_groups <- c("B", "W") 
    
    for (race in race_groups) {
      
      my_vec <- c(my_vec, paste0(name, ".", race))
      
    }
  } 
  my_vec
}

keep_vec_trad <- add_race_groups_to_df(trad_sim_names)
keep_vec_xpr <- add_race_groups_to_df(xpr_sim_names)

# 5th - drop dataframes and timesteps we don't need from simulations (start at 3120, 60 years into simulation)

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

trad <- select_df(keep_vec_trad, trad_sim)
xpr <- select_df(keep_vec_xpr, xpr_sim)


# 6th - add values for B and W MSM together since not generating race-specific costs
#     - perform colSums within each simulation by year of interest (i.e. adding values over time within each simulation)

# create function split the df name from the race group and put in vector together

split_prefix_and_race <- function(s) {
  
  # split string `s` on the character "."
  # we add \\ because we want strsplit to treat is as a literal not a regular expression
  parts <- strsplit(s, "\\.")[[1]]
  num_parts <- length(parts)
  
  # turn the string parts into a prefix again by "pasting" with "." in between
  prefix <- paste0(parts[1:(num_parts - 1)], collapse=".")
  race <- parts[num_parts]
  
  # return both the prefix and the race
  c(prefix, race)
}

# create function that makes list with B and W df store within the same sub-list under the prefix name
group_prefix_dfs <- function(list_of_dfs) {
  
  grouped_dfs <- list()
  
  for(entry_name in names(list_of_dfs)) {
    
    name_parts <- split_prefix_and_race(entry_name)
    prefix <- name_parts[[1]]
    entry_df <- list_of_dfs[[entry_name]]
    
    existing_groups <- names(grouped_dfs)
    
    if (prefix %in% existing_groups) {
      
      prefix_dfs <- grouped_dfs[[prefix]]
      prefix_dfs[[2]] <- entry_df
      
    } else {
      prefix_dfs <- list(entry_df)
    }
    grouped_dfs[[prefix]] <- prefix_dfs
  }
  grouped_dfs
}

# create function that adds B and W values for each prefix together (e.g., sim 1 time 1 for B gets added to sim 1 time 1 for W)

add_grouped_dfs <- function(list_of_grouped_dfs) {
  
  added_dfs <- list()
  
  for (entry_name in names(list_of_grouped_dfs)) {
    
    entry_dfs <- list_of_grouped_dfs[[entry_name]]
    df_columns <- colnames(entry_dfs[[1]])
    df1 <- entry_dfs[[1]]
    df2 <- entry_dfs[[2]]
    df_combined <- df1[, df_columns] + df2[, df_columns]  
    added_dfs[[entry_name]] <- df_combined
    
  }
  added_dfs
}

# call initial functions - group each race group df under prefix name 

grouped_dfs_trad <- group_prefix_dfs(trad)
grouped_dfs_xpr <- group_prefix_dfs(xpr)

# call initial functions - add race groups under prefix name together
added_dfs_trad <- add_grouped_dfs(grouped_dfs_trad)
added_dfs_xpr <- add_grouped_dfs(grouped_dfs_xpr)

# sum the counts by years of interest within each simulation

sum_by_year_within_sim <- function(added_dfs) {
  
  colsum_list <- list()
  
  for (df in names(added_dfs)) {
    
    my_df <- added_dfs[[df]]
    my_df <- my_df[1:260, ]
    weeks <- 1:nrow(my_df)
    weeks_with_year <- floor((weeks -1) / 52) + 1
    my_df$Year <- weeks_with_year
    
    my_df_melt <- melt(my_df, id.vars = "Year", variable.name = "Simulation", value.name = "Count")
    colsum_list[[df]] <- ddply(my_df_melt, .(Simulation, Year), summarize, Year_Sum = sum(Count))
  }
  colsum_list
}

Trad <- sum_by_year_within_sim(added_dfs_trad)
Xpr <- sum_by_year_within_sim(added_dfs_xpr)

sum_by_week_within_sim <- function(added_dfs) {
  
  colsum_list <- list()
  
  for (df in names(added_dfs)) {
    
    my_df <- added_dfs[[df]]
    my_df <- my_df[1:260, ]
    my_df$Week <- 1:nrow(my_df)
    
    my_df_melt <- melt(my_df, id.vars = "Week", variable.name = "Simulation", value.name = "Count")
    colsum_list[[df]] <- my_df_melt
  }
  colsum_list
}

Trad_wk <- sum_by_week_within_sim(added_dfs_trad)
Xpr_wk <- sum_by_week_within_sim(added_dfs_xpr)

# STEP 2: Calculate costs * volume for each pathway and year #################

# Major Pathways EACH YEAR:
# - Asymptomatic screening for traditional and express (different costs, different volumes)
# - Symptomatic testing for traditional and express (same costs, different volumes)
# - Treatment for traditional and express (same costs, different volumes)

## 1st - calculate screening costs

calc_scr_costs_trad <- function(x) { 
  
  scr_costs <- list()
  years <- 1:5
  
  pathway <- "Trad"
  symptoms <- "A"
  
  for (year in years) {
    
    sites <- "U"
    
    cost_ugc <- 
      (x$num.dx.asympt.ugc.only.num.inf[which(x$num.dx.asympt.ugc.only.num.inf$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      ((x$num.dx.asympt.ugc.only[which(x$num.dx.asympt.ugc.only$Year == year), "Year_Sum"] - x$num.dx.asympt.ugc.only.num.inf[which(x$num.dx.asympt.ugc.only.num.inf$Year == year), "Year_Sum"]) * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "All_Neg")), "US_Value_2020"])
    
    sites <- "UE"
    
    cost_ugc_rgc <-
      (x$num.dx.asympt.ugc.rgc.num.inf.ugc[which(x$num.dx.asympt.ugc.rgc.num.inf.ugc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.ugc.rgc.num.inf.rgc[which(x$num.dx.asympt.ugc.rgc.num.inf.rgc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.ugc.rgc.num.inf.both[which(x$num.dx.asympt.ugc.rgc.num.inf.both$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.ugc.rgc.num.inf.none[which(x$num.dx.asympt.ugc.rgc.num.inf.none$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "All_Neg")), "US_Value_2020"]) 
    
    cost_ugc_ogc <-
      (x$num.dx.asympt.ugc.ogc.num.inf.ugc[which(x$num.dx.asympt.ugc.ogc.num.inf.ugc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.ugc.ogc.num.inf.ogc[which(x$num.dx.asympt.ugc.ogc.num.inf.ogc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.ugc.ogc.num.inf.both[which(x$num.dx.asympt.ugc.ogc.num.inf.both$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.ugc.ogc.num.inf.none[which(x$num.dx.asympt.ugc.ogc.num.inf.none$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "All_Neg")), "US_Value_2020"]) 
    
    sites <- "URO"
    
    cost_triple_site <-
      (x$num.dx.asympt.triple.site.num.inf.ugc[which(x$num.dx.asympt.triple.site.num.inf.ugc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.rgc[which(x$num.dx.asympt.triple.site.num.inf.rgc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.ogc[which(x$num.dx.asympt.triple.site.num.inf.ogc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.ugc.rgc[which(x$num.dx.asympt.triple.site.num.inf.ugc.rgc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.ugc.ogc[which(x$num.dx.asympt.triple.site.num.inf.ugc.ogc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.rgc.ogc[which(x$num.dx.asympt.triple.site.num.inf.rgc.ogc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.triple.site[which(x$num.dx.asympt.triple.site.num.inf.triple.site$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.none[which(x$num.dx.asympt.triple.site.num.inf.none$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "All_Neg")), "US_Value_2020"]) 
    
    cost_total <- cost_ugc + cost_ugc_rgc + cost_ugc_ogc + cost_triple_site
    name <- paste0("year", year)
    scr_costs[[name]] <- cost_total
    
  }
  scr_costs
}

calc_scr_costs_xpr <- function(x) { 
  
  scr_costs <- list()
  years <- 1:5
  
  pathway <- "Xpr"
  sites <- "URO"
  symptoms <- "A"
  
  for (year in years) {
    
    cost_triple_site <-
      (x$num.dx.asympt.triple.site.num.inf.ugc[which(x$num.dx.asympt.triple.site.num.inf.ugc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.rgc[which(x$num.dx.asympt.triple.site.num.inf.rgc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.ogc[which(x$num.dx.asympt.triple.site.num.inf.ogc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.ugc.rgc[which(x$num.dx.asympt.triple.site.num.inf.ugc.rgc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.ugc.ogc[which(x$num.dx.asympt.triple.site.num.inf.ugc.ogc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.rgc.ogc[which(x$num.dx.asympt.triple.site.num.inf.rgc.ogc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.triple.site[which(x$num.dx.asympt.triple.site.num.inf.triple.site$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.none[which(x$num.dx.asympt.triple.site.num.inf.none$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "All_Neg")), "US_Value_2020"]) 
    
    cost_total <- cost_triple_site
    name <- paste0("year", year)
    scr_costs[[name]] <- cost_total
    
  }
  scr_costs
}

scr_costs_trad <- calc_scr_costs_trad(Trad)
scr_costs_xpr <- calc_scr_costs_xpr(Xpr)

# BY WEEK

## 1st - calculate screening costs

rbind_and_add_scenario <- function(base_file, comp_file, base_scenario_name, comp_scenario_name) {
  
  base_file_rbind <- data.frame(do.call(rbind, base_file))
  comp_file_rbind <- data.frame(do.call(rbind, comp_file))
  
  base_file_rbind$Week <- 1:260
  comp_file_rbind$Week <- 1:260
  
  weeks <- 1:260
  weeks_with_year <- floor((weeks -1) / 52) + 1
  base_file_rbind$Year <- weeks_with_year
  comp_file_rbind$Year <- weeks_with_year
  
  base_file_rbind$Scenario <- base_scenario_name
  comp_file_rbind$Scenario <- comp_scenario_name
  
  combo_file_rbind <- rbind(base_file_rbind, comp_file_rbind, stringsAsFactors = FALSE)
  
}

calc_scr_costs_trad_wk <- function(x) { 
  
  scr_costs <- list()
  weeks <- 1:260
  
  pathway <- "Trad"
  symptoms <- "A"
  
  for (week in weeks) {
    
    sites <- "U"
    
    cost_ugc <- 
      (x$num.dx.asympt.ugc.only.num.inf[which(x$num.dx.asympt.ugc.only.num.inf$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      ((x$num.dx.asympt.ugc.only[which(x$num.dx.asympt.ugc.only$Week == week), "Count"] - x$num.dx.asympt.ugc.only.num.inf[which(x$num.dx.asympt.ugc.only.num.inf$Week == week), "Count"]) * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "All_Neg")), "US_Value_2020"])
    
    sites <- "UE"
    
    cost_ugc_rgc <-
      (x$num.dx.asympt.ugc.rgc.num.inf.ugc[which(x$num.dx.asympt.ugc.rgc.num.inf.ugc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.ugc.rgc.num.inf.rgc[which(x$num.dx.asympt.ugc.rgc.num.inf.rgc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.ugc.rgc.num.inf.both[which(x$num.dx.asympt.ugc.rgc.num.inf.both$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.ugc.rgc.num.inf.none[which(x$num.dx.asympt.ugc.rgc.num.inf.none$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "All_Neg")), "US_Value_2020"]) 
    
    cost_ugc_ogc <-
      (x$num.dx.asympt.ugc.ogc.num.inf.ugc[which(x$num.dx.asympt.ugc.ogc.num.inf.ugc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.ugc.ogc.num.inf.ogc[which(x$num.dx.asympt.ugc.ogc.num.inf.ogc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.ugc.ogc.num.inf.both[which(x$num.dx.asympt.ugc.ogc.num.inf.both$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.ugc.ogc.num.inf.none[which(x$num.dx.asympt.ugc.ogc.num.inf.none$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "All_Neg")), "US_Value_2020"]) 
    
    sites <- "URO"
    
    cost_triple_site <-
      (x$num.dx.asympt.triple.site.num.inf.ugc[which(x$num.dx.asympt.triple.site.num.inf.ugc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.rgc[which(x$num.dx.asympt.triple.site.num.inf.rgc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.ogc[which(x$num.dx.asympt.triple.site.num.inf.ogc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.ugc.rgc[which(x$num.dx.asympt.triple.site.num.inf.ugc.rgc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.ugc.ogc[which(x$num.dx.asympt.triple.site.num.inf.ugc.ogc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.rgc.ogc[which(x$num.dx.asympt.triple.site.num.inf.rgc.ogc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.triple.site[which(x$num.dx.asympt.triple.site.num.inf.triple.site$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.none[which(x$num.dx.asympt.triple.site.num.inf.none$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "All_Neg")), "US_Value_2020"]) 
    
    cost_total <- cost_ugc + cost_ugc_rgc + cost_ugc_ogc + cost_triple_site
    scr_costs[[week]] <- cost_total
    
  }
  scr_costs
}

calc_scr_costs_xpr_wk <- function(x) { 
  
  scr_costs <- list()
  weeks <- 1:260

  
  pathway <- "Xpr"
  sites <- "URO"
  symptoms <- "A"
  
  for (week in weeks) {
    
    cost_triple_site <-
      (x$num.dx.asympt.triple.site.num.inf.ugc[which(x$num.dx.asympt.triple.site.num.inf.ugc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.rgc[which(x$num.dx.asympt.triple.site.num.inf.rgc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.ogc[which(x$num.dx.asympt.triple.site.num.inf.ogc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.ugc.rgc[which(x$num.dx.asympt.triple.site.num.inf.ugc.rgc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.ugc.ogc[which(x$num.dx.asympt.triple.site.num.inf.ugc.ogc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.rgc.ogc[which(x$num.dx.asympt.triple.site.num.inf.rgc.ogc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.triple.site[which(x$num.dx.asympt.triple.site.num.inf.triple.site$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) +
      
      (x$num.dx.asympt.triple.site.num.inf.none[which(x$num.dx.asympt.triple.site.num.inf.none$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "All_Neg")), "US_Value_2020"]) 
    
    cost_total <- cost_triple_site
    scr_costs[[week]] <- cost_total
    
  }
  scr_costs
}

scr_costs_trad_wk <- calc_scr_costs_trad_wk(Trad_wk)
scr_costs_xpr_wk <- calc_scr_costs_xpr_wk(Xpr_wk)

scr_costs_wk_combo <- rbind_and_add_scenario(scr_costs_trad_wk, scr_costs_xpr_wk, "Traditional", "Express")

#

## 2nd - calculate testing costs
# Note: Xpr pathway also uses traditional costs for symptomatic testing, so can use the same fxn 

calc_test_costs_trad <- function(x) { 
  
  test_costs <- list()
  years <- 1:5
  
  pathway <- "Trad"
  symptoms <- "S"
  
  for (year in years) {
    
    sites <- "U"
    
    cost_ugc <-
      (x$num.dx.sympt.ugc.only.num.inf[which(x$num.dx.sympt.ugc.only.num.inf$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    sites <- "R"
    
    cost_rgc <-
      (x$num.dx.sympt.rgc.only.num.inf[which(x$num.dx.sympt.rgc.only.num.inf$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    sites <- "O"
    
    cost_ogc <-
      (x$num.dx.sympt.ogc.only.num.inf[which(x$num.dx.sympt.ogc.only.num.inf$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    sites <- "UE"
    
    cost_ugc_rgc <-
      (x$num.dx.sympt.ugc.rgc[which(x$num.dx.sympt.ugc.rgc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    cost_ugc_ogc <-
      (x$num.dx.sympt.ugc.ogc[which(x$num.dx.sympt.ugc.ogc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    sites <- "RO"
    
    cost_rgc_ogc <-
      (x$num.dx.sympt.rgc.ogc[which(x$num.dx.sympt.rgc.ogc$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    sites <- "URO"
    
    cost_triple_site <-
      (x$num.dx.sympt.triple.site[which(x$num.dx.sympt.triple.site$Year == year), "Year_Sum"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    cost_total <- cost_ugc + cost_rgc + cost_ogc + cost_ugc_rgc + cost_ugc_ogc + cost_ugc_rgc + cost_triple_site
    name <- paste0("year", year)
    test_costs[[name]] <- cost_total
    
  }
  test_costs
}

test_costs_trad <- calc_test_costs_trad(Trad)
test_costs_xpr <- calc_test_costs_trad(Xpr)

# BY WEEK
## 2nd - calculate testing costs
# Note: Xpr pathway also uses traditional costs for symptomatic testing, so can use the same fxn 

calc_test_costs_trad_wk <- function(x) { 
  
  test_costs <- list()
  weeks <- 1:260
  
  pathway <- "Trad"
  symptoms <- "S"
  
  for (week in weeks) {
    
    sites <- "U"
    
    cost_ugc <-
      (x$num.dx.sympt.ugc.only.num.inf[which(x$num.dx.sympt.ugc.only.num.inf$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    sites <- "R"
    
    cost_rgc <-
      (x$num.dx.sympt.rgc.only.num.inf[which(x$num.dx.sympt.rgc.only.num.inf$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    sites <- "O"
    
    cost_ogc <-
      (x$num.dx.sympt.ogc.only.num.inf[which(x$num.dx.sympt.ogc.only.num.inf$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    sites <- "UE"
    
    cost_ugc_rgc <-
      (x$num.dx.sympt.ugc.rgc[which(x$num.dx.sympt.ugc.rgc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    cost_ugc_ogc <-
      (x$num.dx.sympt.ugc.ogc[which(x$num.dx.sympt.ugc.ogc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    sites <- "RO"
    
    cost_rgc_ogc <-
      (x$num.dx.sympt.rgc.ogc[which(x$num.dx.sympt.rgc.ogc$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    sites <- "URO"
    
    cost_triple_site <-
      (x$num.dx.sympt.triple.site[which(x$num.dx.sympt.triple.site$Week == week), "Count"] * 
         costs[which((costs$Pathway == pathway) & (costs$Sites == sites) & (costs$Symptoms == symptoms) & (costs$Result == "Any_Pos")), "US_Value_2020"]) 
    
    cost_total <- cost_ugc + cost_rgc + cost_ogc + cost_ugc_rgc + cost_ugc_ogc + cost_ugc_rgc + cost_triple_site
    test_costs[[week]] <- cost_total
    
  }
  test_costs
}

test_costs_trad_wk <- calc_test_costs_trad_wk(Trad_wk)
test_costs_xpr_wk <- calc_test_costs_trad_wk(Xpr_wk)

test_costs_wk_combo <- rbind_and_add_scenario(test_costs_trad_wk, test_costs_xpr_wk, "Traditional", "Express")

## 3rd - calculate treatment costs
# Note: Xpr pathway also uses traditional costs for treatment, so can use the same fxn 

calc_tx_costs_trad <- function(x) { 
  
  tx_costs <- list()
  years <- 1:5
  
  pathway <- "Tx"
  
  for (year in years) {
    
    cost_total <-
      (x$num.trad.tx[which(x$num.trad.tx$Year == years), "Year_Sum"] * 
         costs[which(costs$Pathway == pathway), "US_Value_2020"]) 
    
    name <- paste0("year", year)
    tx_costs[[name]] <- cost_total
    
  }
  tx_costs
}

tx_costs_trad <- calc_tx_costs_trad(Trad)
tx_costs_xpr <- calc_tx_costs_trad(Xpr)


# BY WEEK

## 3rd - calculate treatment costs
# Note: Xpr pathway also uses traditional costs for treatment, so can use the same fxn 

calc_tx_costs_trad_wk <- function(x) { 
  
  tx_costs <- list()
  weeks <- 1:260
  
  pathway <- "Tx"

  for (week in weeks) {
    
    cost_total <-
      (x$num.trad.tx[which(x$num.trad.tx$Week == week), "Count"] * 
         costs[which(costs$Pathway == pathway), "US_Value_2020"]) 
    
    tx_costs[[week]] <- cost_total
    
  }
  tx_costs
}

tx_costs_trad_wk <- calc_tx_costs_trad_wk(Trad_wk)
tx_costs_xpr_wk <- calc_tx_costs_trad_wk(Xpr_wk)

# 4th - calculate total costs per year
# Note: combining screening, testing, and treatment costs for each simulation

calc_total_cost_per_yr <- function(scr_cost, test_cost, tx_cost) {
  
  years <- paste0("year", seq(1,5))
  total_costs <- list()
  
  for (year in years) {
    
    total <- scr_cost[[year]] + test_cost[[year]] + tx_cost[[year]]
    total_costs[[year]] <- total
  }
  total_costs
}

total_costs_trad <- calc_total_cost_per_yr(scr_costs_trad, test_costs_trad, tx_costs_trad)
total_costs_xpr <- calc_total_cost_per_yr(scr_costs_xpr, test_costs_xpr, tx_costs_xpr)

# BY WEEK
## 3rd - calculate treatment costs
# Note: Xpr pathway also uses traditional costs for treatment, so can use the same fxn 

calc_tx_costs_trad_wk <- function(x) { 
  
  tx_costs <- list()
  weeks <- 1:260
  
  pathway <- "Tx"
  
  for (week in weeks) {
    
    cost_total <-
      (x$num.trad.tx[which(x$num.trad.tx$Week == week), "Count"] * 
         costs[which(costs$Pathway == pathway), "US_Value_2020"]) 
    
    tx_costs[[week]] <- cost_total
    
  }
  tx_costs
}

tx_costs_trad_wk <- calc_tx_costs_trad_wk(Trad_wk)
tx_costs_xpr_wk <- calc_tx_costs_trad_wk(Xpr_wk)

tx_costs_wk_combo <- rbind_and_add_scenario(tx_costs_trad_wk, tx_costs_xpr_wk, "Traditional", "Express")


# 4th - calculate total costs per year
# Note: combining screening, testing, and treatment costs for each simulation

calc_total_cost_per_yr <- function(scr_cost, test_cost, tx_cost) {
  
  years <- paste0("year", seq(1,5))
  total_costs <- list()
  
  for (year in years) {
    
    total <- scr_cost[[year]] + test_cost[[year]] + tx_cost[[year]]
    total_costs[[year]] <- total
  }
  total_costs
}

total_costs_trad <- calc_total_cost_per_yr(scr_costs_trad, test_costs_trad, tx_costs_trad)
total_costs_xpr <- calc_total_cost_per_yr(scr_costs_xpr, test_costs_xpr, tx_costs_xpr)

# STEP 3: Calculate Cumulative Total Costs per Year (Using Discounted Values) #################


accumul_icer_components <- function(yearly_values) {
  
  accumul_list_by_yr <- list()
  num_elements <- length(yearly_values[[names(yearly_values)[[1]]]])
  current_values <- rep(0, num_elements)
  
  for (year in names(yearly_values)) {
    
    this_yr_values <- yearly_values[[year]]
    current_values <- current_values + this_yr_values
    accumul_list_by_yr[[year]] <- current_values
  }
  accumul_list_by_yr
}

cumul_costs_trad <- accumul_icer_components(total_costs_trad)
cumul_costs_xpr <- accumul_icer_components(total_costs_xpr)

# STEP 4: Apply Discounting  #################
# Note: midpoint method using 3% cost discounting: Cost / (1 + Discount Rate) ^ ((Year - Current Year) - 0.5)

# 1st - write discounting function

apply_cost_discounting <- function(costs) {
  
  current_year <- 2020
  year_dates <- list(
    
    year1 = current_year + 1,
    year2 = current_year + 2,
    year3 = current_year + 3,
    year4 = current_year + 4,
    year5 = current_year + 5
    
  )
  
  discount_rate <- 0.03
  disc_costs <- list()
  
  for (year_name in names(costs)) {
    
    raw_costs <- costs[[year_name]]
    year_date <- year_dates[[year_name]]
    
    disc_costs[[year_name]] <- raw_costs / ((1 + discount_rate)^((year_date - current_year) - 0.5))
  }
  
  disc_costs
}

# 2nd - apply discounting function to all costs

scr_costs_disc_trad <- apply_cost_discounting(scr_costs_trad)
scr_costs_disc_xpr <- apply_cost_discounting(scr_costs_xpr)

test_costs_disc_trad <- apply_cost_discounting(test_costs_trad)
test_costs_disc_xpr <- apply_cost_discounting(test_costs_xpr)

tx_costs_disc_trad <- apply_cost_discounting(tx_costs_trad)
tx_costs_disc_xpr <- apply_cost_discounting(tx_costs_xpr)

tx_costs_disc_trad <- apply_cost_discounting(tx_costs_trad)
tx_costs_disc_xpr <- apply_cost_discounting(tx_costs_xpr)

total_costs_disc_trad <- apply_cost_discounting(total_costs_trad)
total_costs_disc_xpr <- apply_cost_discounting(total_costs_xpr)

cumul_costs_disc_trad <- apply_cost_discounting(cumul_costs_trad)
cumul_costs_disc_xpr <- apply_cost_discounting(cumul_costs_xpr)

# apply cost discounting to weekly costs

# disc_function <- function(input_file) {
# 
#  dat <-  input_file$Costs / ((1 + discount_rate)^((year_date - current_year) - 0.5))
#  dat
# 
# }
# 
# run_discounting_on_wk_costs <- function(cost_file) {
# 
#   current_year <- 2020
# 
#   year_dates <- list(
# 
#     year0 = current_year + 1,
#     year1 = current_year + 2,
#     year2 = current_year + 3,
#     year3 = current_year + 4,
#     year4 = current_year + 5
# 
#   )
# 
#   discount_rate <- 0.03
#   disc_costs <- list()
# 
#   cost_file_melt <- melt(cost_file, id.vars = c("Year", "Week", "Scenario"), variable.name = "Simulation", value.name = "Cost")
# 
#   for (year in unique(cost_file_melt$Year)) {
# 
#     this_year <- paste0("year", year)
#     year_date <- year_dates[[this_year]]
# 
#     cost_file_single_year <- cost_file_melt[which(cost_file_melt$Year == year), ]
#     costs <- ddply(cost_file_single_year, id.vars = c("Scenario", "Week", "Year"), transform, disc_function(x))
#     raw_costs <- costs[[year_name]]
#     year_date <- year_dates[[year_name]]
# 
#       disc_costs[[year_name]] <- raw_costs /
#     }
# 
#     disc_costs
# 
#   }
# 
# 
#   cost_file_list[[year]]
#   cost_file_costs_only_disc <- apply_cost_discounting(cost_file_costs_only)
#   cost_file_combo_disc <- cbind(cost_file_costs_only_disc, drop_col, stringsAsFactors = FALSE)
# 
# }
# 
# scr_costs_wk_combo_disc <- run_discounting_on_wk_costs(scr_costs_wk_combo)
# test_costs_wk_combo_disc <- run_discounting_on_wk_costs(test_costs_wk_combo)
# tx_costs_wk_combo_disc <- run_discounting_on_wk_costs(tx_costs_wk_combo)

# STEP 5: Calculate cumulative cost difference

calc_cumul_cost_diff <- function(cumul_costs_base, cumul_costs_comp) {
  
  cumul_cost_diff_list <- list()
  
  for (year in names(cumul_costs_base)) {
    
    cumul_cost_diff_list[[year]] <- cumul_costs_comp[[year]] - mean(cumul_costs_base[[year]]) 
    
  }
  
  cumul_cost_diff_list
  
}

cumul_cost_diff_disc <- calc_cumul_cost_diff(cumul_costs_disc_trad, cumul_costs_disc_xpr)


# STEP 6: Save Data #################

file_names <- list(
  
  "scr_costs_trad" = scr_costs_trad,
  "scr_costs_xpr" = scr_costs_xpr, 
  "test_costs_trad" = test_costs_trad,
  "test_costs_xpr" = test_costs_xpr,
  "tx_costs_trad" = tx_costs_trad,
  "tx_costs_xpr" = tx_costs_xpr,
  "scr_costs_disc_trad" = scr_costs_disc_trad,
  "scr_costs_disc_xpr" = scr_costs_disc_xpr, 
  "test_costs_disc_trad" = test_costs_disc_trad,
  "test_costs_disc_xpr" = test_costs_disc_xpr,
  "tx_costs_disc_trad" = tx_costs_disc_trad,
  "tx_costs_disc_xpr" = tx_costs_disc_xpr,
  "total_costs_disc_trad" = total_costs_disc_trad,
  "total_costs_disc_xpr" = total_costs_disc_xpr,
  "cumul_costs_disc_trad" = cumul_costs_disc_trad,
  "cumul_costs_disc_xpr" = cumul_costs_disc_xpr,
  "cumul_cost_diff_disc" = cumul_cost_diff_disc
  # "scr_costs_wk_combo_disc" = scr_costs_wk_combo_disc,
  # "test_costs_wk_combo_disc" = test_costs_wk_combo_disc,
  # "tx_costs_wk_combo_disc" = tx_costs_wk_combo_disc
  
  )

output_path <- paste0(files_path, "/Output/Economic_Model/")

for (file_name in names(file_names)) {
  
  data_object <- file_names[[file_name]]
  
  save(data_object, list = c(file_name), file = paste0(output_path, file_name, ".rda"))
  
}







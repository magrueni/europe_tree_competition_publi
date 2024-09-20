################################################################################
### 1. script to process dnn predictions ---------------------------------------
### Marc Grünig --- 17.09.2024 -------------------------------------------------
################################################################################


# load libraries
library(arrow)
library(tidyverse)
library(dplyr)
library(reshape)
library(ggplot2)
library(ggdist)
library(data.table)
library(parallel)


# define path
path <- "/.../"

# load functions
source(paste0("functions/calculate_all_indicators_gridcell_rate.R"))
source(paste0("functions/init_state_lists.R"))


# load data
files <- list.files(paste0(path, "/predictions/"), full.names = TRUE)
files <- files[grepl("_final_preds", files)]

# remove the dominance predictions - they are processed in the second part
files <- files[!grepl("dominance", files)]

# define timesteps
timesteps <- c("1981-1990", "1991-2000", "2001-2010", "2071-2080", "2081-2090", "2091-2100")

# define species
species  <- c("Fagus_sylvatica", "Pinus_sylvestris", "Abies_alba", "Picea_abies", 
              "Larix_decidua", "Betula_pendula", "Pinus_halepensis", "Quercus_robur",
              "Quercus_ilex")

# define RCPs
rcps <- c("rcp_8_5", "rcp_4_5", "rcp_2_6")


# define output lists for the two variables
all_out_lai_list <- list()
all_out_hei_list <- list()


# loop over all species to analyse predictions
for(s in 1:length(species)){
  
  # naming of species
  nam <- gsub("_", " ", species[s])
  nam2 <- species[s]
  print(nam2)
  nam_short <- toupper(paste0(substring(strsplit(nam2, "_")[[1]][1], 1, 2), substring(strsplit(nam2, "_")[[1]][2], 1, 2)))

  # special cases
  if(nam == "Fagus sylvatica"){nam2 <- "Fagus_sylvatica_sylvatica"}
  if(nam == "Larix decidua"){nam2 <- "Larix_decidua_decidua"}
  if(nam == "Quercus ilex"){nam2 <- "Quercus_ilex_ilex"}
  
  
  # seperate filenames of the species
  files_sp <- files[grepl(species[s], files)]

  # load the species specific initial states
  init_states <- lookup_table_single[[nam_short]]
 
  # output lists for the species, different RCPs
  out_lai_list <- list()
  out_hei_list <- list()
  
  # counter for rcps
  n <- 0
  
  for(c in rcps){
  
    n <- n+1
    
    # get the rcp specific predicitons
    files_scen <- files_sp[grepl(c, files_sp) | grepl("historical", files_sp)]
    files_scen <- files_scen[grepl(paste(init_states, collapse = "|"), files_scen)]
    
    # create output file for the timesteps
    all_output <- list()
    
    # loop pover all timesteps
    for(t in 1:length(timesteps)){
      
      # get predictions
      print(timesteps[t])
      files_time <- files_scen[grepl(timesteps[t], files_scen)]

      # process the predictions
      output_stats_states_load <- mclapply(files_time, process_dnn_predictions_growth_rate,
                                           timestep = timesteps[t],
                                           mc.cleanup = TRUE, mc.cores = 12)
      
      # collect
      output_stats_states <- do.call(bind_rows, output_stats_states_load) %>% 
        dplyr::rename("init_state" = "state")
      
      # save
      all_output[[t]] <- output_stats_states
      
    } # close timesteps
    
    output <- do.call(bind_rows, all_output)

    # calculate the differences between baseline and future predictions for the three dimensions
    variables <- list("lai_diff" = "LAI", "growth" = "growth")
    output_list <- list()
    
    for(z in seq_along(variables)){
      var_name <- names(variables)[z]
      variable <- variables[[z]]
      
      df <- output %>%
        dplyr::rename(var = !!sym(var_name))
      
      # Calculate the difference between baseline and future timesteps
      baseline <- df %>% 
        filter(timestep %in% c("1981-1990", "1991-2000", "2001-2010", "1981-2010")) %>% 
        group_by(point_id, init_state, scen) %>% 
        summarize(var_base = mean(var, na.rm = TRUE))
      
      fut <- df %>% 
        filter(!timestep %in% c("1981-1990", "1991-2000", "2001-2010", "1981-2010")) %>% 
        group_by(point_id, init_state, scen) %>% 
        summarize(var_fut = mean(var, na.rm = TRUE))
      
      diff_df <- left_join(fut, baseline, by = c("point_id", "init_state", "scen")) %>% 
        mutate(diff = (var_fut - var_base)) %>% 
        group_by(point_id) %>% 
        summarise(net_negative = sum(diff < 0, na.rm = TRUE),
                  net_positive = sum(diff > 0, na.rm = TRUE),
                  diff = mean(diff, na.rm = TRUE),
                  sum = n()) %>% 
        mutate(net_change = (net_positive - net_negative) / sum * 100) %>% 
        mutate(indicator = paste(z),
               rcp = paste(c),
               species = paste0(species[s]))
      
      # save
      output_list[[variable]] <- diff_df
    }
    
    
    # combine the outputs
    out_lai_list[[n]] <- output_list$LAI
    out_hei_list[[n]] <- output_list$growth
    
  } # close rcps
  
  all_out_lai_list[[s]] <- do.call(rbind, out_lai_list)
  all_out_hei_list[[s]] <- do.call(rbind, out_hei_list)
  
} # close sp loop

all_out_lai <- do.call(rbind, all_out_lai_list)
all_out_hei <- do.call(rbind, all_out_hei_list)

# write out
write_csv(all_out_lai, paste0(path, "/results/lai_absolute_change_10year.csv"))
write_csv(all_out_hei, paste0(path, "/results/hei_absolute_change_10year.csv"))



################################################################################
### do the same for the dominance predictions ----------------------------------
################################################################################

# load function
source(paste0(path, "/functions/calculate_all_indicators_gridcell_dominance.R"))
source(paste0(path, "/functions/init_state_lists.R"))

# load data
files <- list.files(paste0(path, "/predictions/"), full.names = TRUE)
files <- files[grepl("_final_preds", files)]

# get only dominance predictions
files <- files[grepl("dominance", files)]

# define timesteps
timesteps <- c("1981-1990", "1991-2000", "2001-2010", "2071-2080", "2081-2090", "2091-2100")

# define species
species  <- c("Fagus_sylvatica", "Pinus_sylvestris", "Abies_alba", "Picea_abies", 
              "Larix_decidua", "Betula_pendula", "Pinus_halepensis", "Quercus_robur",
              "Quercus_ilex")

# define RCPs
rcps <- c("rcp_8_5", "rcp_4_5", "rcp_2_6")

# output list
all_out_dom_list <- list()


# loop over all species to analyse predictions
for(s in 1:length(species)){
  
  # loop over all species to analyse predictions
  nam <- gsub("_", " ", species[s])
  nam2 <- species[s]
  print(nam2)
  nam_short <- toupper(paste0(substring(strsplit(nam2, "_")[[1]][1], 1, 2), substring(strsplit(nam2, "_")[[1]][2], 1, 2)))
  
  if(nam == "Fagus sylvatica"){nam2 <- "Fagus_sylvatica_sylvatica"}
  if(nam == "Larix decidua"){nam2 <- "Larix_decidua_decidua"}
  if(nam == "Quercus ilex"){nam2 <- "Quercus_ilex_ilex"}
  
  # seperate filenames of the species
  files_sp <- files[grepl(species[s], files)]
  
  # define initial states
  init_states <- lookup_table_mixed[[nam_short]]

  # output list
  out_dom_list <- list()

  n <- 0
  
  for(c in rcps){
    
    n <- n+1
    
    files_scen <- files_sp[grepl(c, files_sp) | grepl("historical", files_sp)]
    files_scen <- files_scen[grepl(paste(init_states, collapse = "|"), files_scen)]
    output_stats_timesteps <- list()
    
    if(length(files_scen) == 0){next}
    
    # create output file
    all_output <- list()
    
    # loop pover all timesteps
    for(t in 1:length(timesteps)){
      
      print(timesteps[t])
      files_time <- files_scen[grepl(timesteps[t], files_scen)]
      
      # process the predictions
      processed_preds <- mclapply(files_time, process_dnn_predictions_dominance,
                                           timestep = timesteps[t],
                                           mc.cleanup = TRUE, mc.cores = 12)
      
      processed_preds_bind <- do.call(bind_rows, processed_preds) %>% 
        dplyr::rename("init_state" = "state")
      
      all_output[[t]] <- processed_preds_bind
      
    } # close timesteps
    
    output <- do.call(bind_rows, all_output)
    
    # calculate the differences between baseline and future predictions for the three dimensions
    variables <- list("veg_diff" = "dominance")
    output_list <- list()
    
    for(z in seq_along(variables)){
      var_name <- names(variables)[z]
      variable <- variables[[z]]
      
      df <- output %>%
        dplyr::rename(var = !!sym(var_name))  # Rename the column dynamically
      
      # Calculate the difference between baseline and future timesteps
      baseline <- df %>% 
        filter(timestep %in% c("1981-1990", "1991-2000", "2001-2010", "1981-2010")) %>% 
        group_by(point_id, init_state, scen) %>% 
        summarize(var_base = mean(var, na.rm = TRUE))
      
      fut <- df %>% 
        filter(!timestep %in% c("1981-1990", "1991-2000", "2001-2010", "1981-2010")) %>% 
        group_by(point_id, init_state, scen) %>% 
        summarize(var_fut = mean(var, na.rm = TRUE))
      
      diff_df <- left_join(fut, baseline, by = c("point_id", "init_state", "scen")) %>% 
        mutate(diff = (var_fut - var_base) * -1) %>% # *-1 to render negative change to negative vlaues
        
        group_by(point_id) %>% 
        summarise(net_negative = sum(diff < 0, na.rm = TRUE),
                  net_positive = sum(diff > 0, na.rm = TRUE),
                  diff = mean(diff, na.rm = TRUE),
                  sum = n()) %>% 
        mutate(net_change = (net_positive - net_negative) / sum * 100) %>% 
        mutate(indicator = paste(z),
               rcp = paste(c),
               species = paste0(species[s]))
      
      output_list[[variable]] <- diff_df
    }
    
    # combine the outputs
    out_dom_list[[n]] <- output_list$dominance
    
    
  } # close rcps
  
  all_out_dom_list[[s]] <- do.call(rbind, out_dom_list)
  
} # close sp loop

all_out_dom <- do.call(rbind, all_out_dom_list)

write_csv(all_out_dom, paste0(path, "/results/dom_absolute_change_10year.csv"))






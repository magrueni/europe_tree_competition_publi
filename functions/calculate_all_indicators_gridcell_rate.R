# Define a function to process each file
process_dnn_predictions_growth_rate <- function(file, timestep) {
  
  dat <- read_feather(file) %>% data.table()
  focal_state <- unique(dat$state)
  
  scen <- ifelse(grepl("ICHEC-EC-EARTH", file), "ichec",
                 ifelse(grepl("MPI-M-MPI-ESM-LR", file), "mpi", "ncc"))
  
  # 1. check if the same species is predicted -------------------------
  pred_sp <- dat %>% dplyr::select(point_id, state, pred_state_1, state_proba_1, time_index_1, time_proba_1, 
                                   time_index_2, time_proba_2, time_index_3, time_proba_3) 
  
  pred_sp <- pred_sp %>% 
    mutate(t_time_est = ((time_index_1 * time_proba_1) + (time_index_2 * time_proba_2) +
                           (time_index_3 * time_proba_3)) / (time_proba_1 + time_proba_2 + time_proba_3))
  pred_sp <- pred_sp %>% 
    rowwise() %>% 
    mutate(sp_init = strsplit(state, "_")[[1]][1]) %>% 
    mutate(lai_init = as.numeric(strsplit(state, "_")[[1]][2])) %>% 
    mutate(height_init = as.numeric(strsplit(state, "_")[[1]][4])) %>% 
    mutate(sp_pred = strsplit(pred_state_1, "_")[[1]][1]) %>% 
    mutate(lai_pred = as.numeric(strsplit(pred_state_1, "_")[[1]][2])) %>% 
    mutate(height_pred = as.numeric(strsplit(pred_state_1, "_")[[1]][4])) 
  
  # check if its the same veg or not
  pred_sp <- pred_sp %>% rowwise() %>% 
    mutate(veg_diff = ifelse(grepl(sp_init, sp_pred), 1, 0))
  
  
  # 2. check changes in LAI -------------------------------------------------
  
  pred_sp <- pred_sp %>%
    mutate(lai_diff = lai_pred - lai_init) %>% 
    mutate(lai_diff = ifelse(veg_diff == 0, 0, lai_diff)) %>% 
    mutate(lai_diff = lai_diff / t_time_est)
  
  # 3. height differences -----------------------------------------
  
  pred_sp <- pred_sp %>% 
    mutate(growth = height_pred - height_init) %>% 
    mutate(growth = ifelse(veg_diff == 0, 0, growth)) %>%
    mutate(growth = ifelse(growth > 0, 1, growth)) %>% 
    mutate(growth = ifelse(growth < 0, -1, growth)) %>% 
    mutate(growth = growth / t_time_est)
  
  pred_sp <- pred_sp %>% dplyr::select(point_id, state, veg_diff, lai_diff, growth, state_proba_1)
  
  # outputs -----------------------------------------------------------
  all_outputs <- cbind(timestep = rep(timestep, nrow(pred_sp)),
                       scen = rep(scen, nrow(pred_sp)),
                       id = c(1:nrow(pred_sp)),
                       pred_sp)
  
  # store outputs in lsts collected for all years
  return(all_outputs)
  
}

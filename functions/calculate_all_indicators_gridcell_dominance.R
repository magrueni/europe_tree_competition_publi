
# Define a function to process each file
process_dnn_predictions_dominance <- function(file, timestep) {
  
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
    mutate(sp_dom = substr(sp_init, 0, 4),
           sp_sec = substr(sp_init, 5, 8)) %>% 
    mutate(lai_init = as.numeric(strsplit(state, "_")[[1]][2])) %>% 
    mutate(height_init = as.numeric(strsplit(state, "_")[[1]][4])) %>% 
    mutate(sp_pred = strsplit(pred_state_1, "_")[[1]][1]) %>% 
    mutate(lai_pred = as.numeric(strsplit(pred_state_1, "_")[[1]][2])) %>% 
    mutate(height_pred = as.numeric(strsplit(pred_state_1, "_")[[1]][4])) 
  
  
  # filter out predictions with completely different vegetation (i.e. wierd transitions)
  pred_sp <- pred_sp %>% rowwise() %>% 
    mutate(veg_diff = ifelse(grepl(sp_dom, sp_pred), 0,
                             ifelse(grepl(tolower(sp_dom), sp_pred), 1,
                                    ifelse(grepl(toupper(sp_sec), sp_pred), 1, NA))),
           veg_diff_rate = veg_diff / t_time_est)
  
  
  pred_sp <- pred_sp %>% dplyr::select(point_id, state, pred_state_1, veg_diff, veg_diff_rate, state_proba_1)
  
  all_outputs <- cbind(timestep = rep(timestep, nrow(pred_sp)),
                       scen = rep(scen, nrow(pred_sp)),
                       id = c(1:nrow(pred_sp)),
                       pred_sp)
  
  # store outputs in lsts collected for all years
  return(all_outputs)
  
}

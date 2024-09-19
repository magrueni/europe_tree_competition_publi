##################################################
### 03 --- examples pruning ----------------------
##################################################

# libraries
library(raster)
library(terra)
library(RColorBrewer)
library(sf)
library(dplyr)
library(DBI)
library(stars)
library(ggplot2)
library(exactextractr)
library(arrow)
library(data.table)
library(collapse)
library(future.apply)
library(compiler)
library(parallel)
library(dbplyr)
library(tidyverse)
library(stringr)

path <- "/.../"

### functions -----------------------------------
x_rast <- rast(paste0(path, "/gis_data/reference_grid.tif"))
crs(x_rast) <- "+proj=longlat +datum=WGS84 +no_defs"
ref_tab <- read.csv(paste0(path, "/gis_data/reference_grid_tab.csv"))


get_point_id <- function(x){
  
  x <- as.vector(unlist(x))
  # extract the grid ID
  lon <- as.numeric(x[1])
  lat <- as.numeric(x[2])
  
  coords <- as.data.frame(cbind(lon, lat))
  
  point_extr <- terra::extract(x_rast, coords)[2]
  point_extr <- as.numeric(point_extr)
  
  point_extr <- ifelse(is.na(point_extr), as.numeric(ref_tab[which.min(abs(lon - ref_tab$wgs_x) + abs(lat - ref_tab$wgs_y)), ]), point_extr)
  
  return(point_extr)
  
}


### load data --------------------------------

simulation_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(path, "/forest_simulation_db_v1.1.sqlite"))
tables_con <- dbListTables(simulation_db)
metadata <- dbReadTable(simulation_db, "metadata_all")

# Define function to subsample while maintaining coverage of svd_states
subsample_with_coverage <- function(df) {
  if (nrow(df) > 100000) {
    unique_states <- unique(df$svd_state)
    n_states <- length(unique_states)
    n_sample_per_state <- ceiling(100000 / n_states)
    
    sampled_df <- df %>%
      group_by(svd_state) %>%
      sample_n(n_sample_per_state, replace = T) %>%
      distinct()
    
    return(sampled_df)
  } else {
    return(df)
  }
}



### examples --------------------------------------------------------------

# read examples and check for NAs
examples <- dbReadTable(simulation_db, "examples_2m_scenarios_soil")
examples_nas <- examples %>% 
  filter(grepl("NA", svd_state)) %>% 
  filter()
dim(examples_nas)

examples <- examples %>% 
  left_join(., metadata %>% dplyr::select(uniqueID, simulationID = ID, Lon, Lat), by = c("uniqueID", "simulationID"))


# Group by grid_id, apply subsampling function, and combine results
examples_filt <- examples %>%
  group_by(grid_id, uniqueID, Lat, Lon) %>%
  group_modify(~ subsample_with_coverage(.x))

# Combine subsampled dataframes
examples <- bind_rows(examples_filt)



### augmetnation data -------------------------------------------------------------------
examples_aug <- dbReadTable(simulation_db, "examples_2m_scenarios_soil_augmentation")

# Group by grid_id, apply subsampling function, and combine results
examples_aug <- examples_aug %>% 
  anti_join(., lowLAI, by = c("uniqueID", "simulationID")) #%>% 
dim(examples_aug)

examples_aug <- examples_aug %>% 
  left_join(., metadata %>% dplyr::select(uniqueID, simulationID = ID, Lon, Lat),
            by = c("uniqueID", "simulationID")) %>% 
  group_by(grid_id, uniqueID, Lat, Lon) %>%
  group_modify(~ subsample_with_coverage(.x))

# Combine subsampled dataframes
examples_aug <- bind_rows(examples_aug)

# Print subsampled dataframe
dim(examples_aug)


### filtering of the states according to distance in state space ----------------------------
## in this section we calculate a distance between states to estimate realistic state changes and remove management signals
## then calculate the dist and filter with 0.9

svd_states_examples <- sort(as.character(as.vector(unlist(unique(examples[,"svd_state"])))))
svd_states_aug <- sort(as.character(as.vector(unlist(unique(examples_aug[,"svd_state"])))))

target_states <- sort(as.character(as.vector(unlist(unique(examples[,"target_state"])))))
target_states_aug <- sort(as.character(as.vector(unlist(unique(examples_aug[,"target_state"])))))

hist1 <- sort(as.character(as.vector(unlist(unique(examples[,"hist_state1"])))))
hist2 <- sort(as.character(as.vector(unlist(unique(examples[,"hist_state2"])))))
hist3 <- sort(as.character(as.vector(unlist(unique(examples[,"hist_state3"])))))

hist1_aug <- sort(as.character(as.vector(unlist(unique(examples_aug[,"hist_state1"])))))
hist2_aug <- sort(as.character(as.vector(unlist(unique(examples_aug[,"hist_state2"])))))
hist3_aug <- sort(as.character(as.vector(unlist(unique(examples_aug[,"hist_state3"])))))

uniqueStates <- unique(c(svd_states_examples, svd_states_aug, target_states, target_states_aug, hist1, hist2, hist3, hist1_aug, hist2_aug, hist3_aug)) %>% sort()
uniqueStates <- uniqueStates[uniqueStates != "missing"]

lookup <- as.data.frame(cbind(unique.str = uniqueStates, stateId = c(1:length(uniqueStates))))


# create states europe file:
states_europe <- lookup %>% rowwise() %>% 
  mutate(composition = strsplit(unique.str, "_")[[1]][1],
         structure = strsplit(unique.str, "_")[[1]][3],
         fct = strsplit(unique.str, "_")[[1]][2],
         n = 0) %>% 
  mutate(composition = gsub("(.{4})", "\\1 ", composition, perl = TRUE))
states_europe <- cbind(states_europe, type = rep(NA, nrow(states_europe)))

states_europe <- rbind(c(0, "INVALID", "nope", 0, 0, 0, "nope"), states_europe)

# get all species
species_str <- unique( (states_europe %>% mutate(composition = tolower(composition)))$composition )

# Extract unique 4-character names using regular expressions
unique_names <- unique(unlist(str_extract_all(species_str, "\\b\\w{4}\\b")))
sort(unique_names)

# Print the unique 4-character names
print(unique_names)
species <- "FASY abal piab"

# "calculate" species proportions for a state
# Rules are:
# - single dominant species: 100%
# - single dominant species, and one other: 0.66 / 0.34
# - single dominant species, and two other: 0.66 / 0.17 / 0.17
# - no dominant, two species: 0.5 / 0.5
# - no dominant, three: 0.34 / 0.33 / 0.33
# - no dominant, more species: 1/n
species_props <- function(species) {
  specs <- unique(unlist(str_extract_all(species, "\\b\\w{4}\\b")))
  has_dom <- specs[1] != tolower(specs[1])
  props <- c(1)
  if (length(specs) == 2) {
    if (has_dom) props <- c(0.66, 0.34) else props<-c(0.5, 0.5)
  }
  if (length(specs) == 3) {
    if(has_dom) props <- c(0.66, 0.17, 0.17) else props<- c(0.34, 0.33, 0.33)
  }
  if (length(specs) > 3) {
    props <- rep(1 / length(specs), length(specs))
  }
  
  names(props) <- tolower(specs)
  data.frame(species=names(props), prop=props)
}

props <- species_props("fasy abal lade")

species_long <- data.frame()
for (i in 1:length(states_europe$state)) {
  props <- species_props(states_europe$composition[i])
  props$stateId <- states_europe$state[i]
  species_long <- rbind(species_long, props)
}

species_prop <- species_long %>% pivot_wider(id_cols=stateId, names_from=species, values_from=prop)
species_prop[is.na(species_prop)] <- 0

# all proportions sum up to 1:
summary( rowSums(species_prop %>% dplyr::select(-stateId)) )
states_meta <- states_europe %>%
  mutate(structure = as.numeric(structure),
         fct = as.numeric(fct),
         n = as.numeric(n)) %>% 
  left_join(species_prop, by = c("stateId"))


states_species <- states_meta %>% dplyr::select(abal:colnames(states_meta)[ncol(states_meta)]) 

# calculate similarity
sm_mat <- as.matrix(states_meta %>% dplyr::select(nope:colnames(states_meta)[ncol(states_meta)]) )
res_mat <- matrix(NA, nrow = dim(sm_mat)[1], ncol=dim(sm_mat)[1])
heights_v <- states_meta$structure
lai_v <- states_meta$fct



for (sm in 1:dim(states_meta)[1]) {
  
  single_state <- as.numeric( as.vector(states_meta[sm,] %>% dplyr::select(nope:colnames(states_meta)[ncol(states_meta)]) ) )
  # sum over all species: abs diff per species
  diffs_species <- 1 - rowSums( t( abs(t(sm_mat) - single_state) ) ) / 2
  # difference in height
  delta_h <- heights_v - states_meta$structure[sm]
  # penalty if next state is >1 states away (or gets smaller)
  diffs_h <- 1 - ifelse(delta_h < -2 | delta_h > 2, 1, 0) # original =  delta_h < 0 is filtered but here we allow decreasing height
  weight_h <- 1 - exp(-0.5* states_meta$structure[sm])
  delta_lai <- lai_v - states_meta$fct[sm]
  diffs_lai <- 1- pmin( (abs(delta_lai)/2) ^ 4, 1)
  
  # total difference
  diffs <- (1 - (1-diffs_species)*weight_h) * diffs_h * diffs_lai
  # difference in LAI class
  res_mat[sm,] <- diffs
  
  cat(".")
  if (sm %% 80 == 0) cat(paste("\n", sm, ":"))
}

# every row is a state, such as:
row.names(res_mat) <- states_meta$stateId


## add distance to examples now
examples_dist <- examples %>% 
  left_join(., lookup %>% rename("svd_state" = "unique.str"), by = c("svd_state")) %>% 
  rename("stateFromId" = "stateId") %>% 
  left_join(., lookup %>% rename("target_state" = "unique.str"), by = c("target_state")) %>% 
  rename("stateToId" = "stateId") %>% 
  rename("stateId" = "stateFromId") %>% 
  mutate(stateToId = as.numeric(stateToId),
         stateId = as.numeric(stateId))

examples_dist$dist <- 1 - apply(examples_dist[, c("stateId", "stateToId")], 1, function(x) res_mat[x[1] + 1, x[2] + 1])
filtered_examples <- examples_dist[examples_dist$dist <= 0.9, ]
dim(filtered_examples)

hist(examples_dist$dist)
check_filt <- examples_dist %>% filter(dist > 0.9)


# take examples that occur more than 5 times
examples2 <- filtered_examples %>%
  group_by(svd_state) %>%
  mutate(count_state = n()) %>%
  ungroup() %>%
  group_by(target_state) %>%
  mutate(count_target_state = n()) %>%
  ungroup()

length(unique(examples2$svd_state))

examples3 <- examples2[examples2$count_state > 10 & examples2$count_target_state > 10, ]
length(unique(examples3$svd_state))
length(unique(examples3$target_state))
dim(examples3)



# throw out the histories where the state is not in there anymore
svd_states_examples <- sort(as.character(as.vector(unlist(unique(examples3[,"svd_state"])))))
uniqueStates_examples <- unique(c("missing", svd_states_examples))

filter_dataframe <- function(df, unique_states) {
  df %>%
    filter(
      svd_state %in% unique_states &
        target_state %in% unique_states &
        hist_state1 %in% unique_states &
        hist_state2 %in% unique_states &
        hist_state3 %in% unique_states
    )
}

repeat {
  # Filter the dataframe
  examples4 <- filter_dataframe(examples3, uniqueStates_examples)
  
  # Get new uniqueStates_examples from the filtered dataframe
  new_uniqueStates_examples <- unique(examples4$svd_state)
  
  # Check if uniqueStates_examples has stabilized
  if (setequal(uniqueStates_examples, new_uniqueStates_examples)) {
    break
  } else {
    uniqueStates_examples <- new_uniqueStates_examples
  }
}

# Get the dimensions of the final dataframe
dim(examples4)


# check if there are states in the history which are not available for training
svd_states_examples <- sort(as.character(as.vector(unlist(unique(examples4[,"svd_state"])))))
uniqueStates_examples <- unique(c("missing", svd_states_examples))

target_states <- sort(as.character(as.vector(unlist(unique(examples4[,"target_state"])))))
target_states[!target_states %in% svd_states_examples]
hist1 <- sort(as.character(as.vector(unlist(unique(examples4[,"hist_state1"])))))
hist1[!hist1 %in% svd_states_examples]
hist2 <- sort(as.character(as.vector(unlist(unique(examples4[,"hist_state2"])))))
hist2[!hist2 %in% svd_states_examples]
hist3 <- sort(as.character(as.vector(unlist(unique(examples4[,"hist_state3"])))))
hist3[!hist3 %in% svd_states_examples]


examples_final <- examples4 %>%
  dplyr::select(-c(stateId, stateToId, count_state, count_target_state)) 
dim(examples_final)

# filter those two ids because the resolution is not yearly
examples_final <- examples_final %>% filter(uniqueID != 1005) %>% filter(uniqueID != 1015)
dim(examples_final)

# write out
dbWriteTable(conn = simulation_db, name = "examples_pruned_5_v16", value = examples_final, overwrite = T)


### do the same for the augmetnation data -------------------------------------------------------------------
examples_aug_dist <- examples_aug %>% 
  left_join(., lookup %>% rename("svd_state" = "unique.str"), by = c("svd_state")) %>% 
  rename("stateFromId" = "stateId") %>% 
  left_join(., lookup %>% rename("target_state" = "unique.str"), by = c("target_state")) %>% 
  rename("stateToId" = "stateId") %>% 
  rename("stateId" = "stateFromId") %>% 
  mutate(stateToId = as.numeric(stateToId),
         stateId = as.numeric(stateId))

examples_aug_dist$dist <- 1 - apply(examples_aug_dist[, c("stateId", "stateToId")], 1, function(x) res_mat[x[1] + 1, x[2] + 1])


filtered_examples_aug <- examples_aug_dist[examples_aug_dist$dist <= 0.9, ]
dim(filtered_examples_aug)

# get unique states from examples_final
svd_states_examples <- sort(as.character(as.vector(unlist(unique(examples_final[,"svd_state"])))))
uniqueStates_examples <- unique(c("missing", svd_states_examples))

# sort out the rare states from the augmentation dataset aswell

examples_aug_filt <- filtered_examples_aug %>%
  filter(svd_state %in% uniqueStates_examples &
           target_state %in% uniqueStates_examples &
           hist_state1 %in% uniqueStates_examples &
           hist_state2 %in% uniqueStates_examples &
           hist_state3 %in% uniqueStates_examples)
dim(examples_aug_filt)

# test
svd_state_aug <- sort(as.character(as.vector(unlist(unique(examples_aug_filt[,"svd_state"])))))
svd_state_aug[!svd_state_aug %in% svd_states_examples]
target_states_aug <- sort(as.character(as.vector(unlist(unique(examples_aug_filt[,"target_state"])))))
target_states_aug[!target_states_aug %in% svd_states_examples]
hist1_aug <- sort(as.character(as.vector(unlist(unique(examples_aug_filt[,"hist_state1"])))))
hist1_aug[!hist1_aug %in% svd_states_examples]
hist2_aug <- sort(as.character(as.vector(unlist(unique(examples_aug_filt[,"hist_state2"])))))
hist2_aug[!hist2_aug %in% svd_states_examples]
hist3_aug <- sort(as.character(as.vector(unlist(unique(examples_aug_filt[,"hist_state3"])))))
hist3_aug[!hist3_aug %in% svd_states_examples]


### write out
examples_final_aug <- examples_aug_filt %>%
  
  dplyr::select(-c(stateId, stateToId, dist))
dim(examples_final_aug)


examples_final_aug <- examples_final_aug %>% filter(uniqueID != 1005) %>% filter(uniqueID != 1015)
dim(examples_final_aug)

dbWriteTable(conn = simulation_db, name = "examples_aug_pruned_5_v16", value = examples_final_aug, overwrite = T)


### create lookup table --------------------------------------------------------------------------------------------
svd_states <- sort(as.character(as.vector(unlist(unique(examples_final[,"svd_state"])))))
target_states <- sort(as.character(as.vector(unlist(unique(examples_final[,"target_state"])))))
target_states[!target_states %in% svd_states]
hist1 <- sort(as.character(as.vector(unlist(unique(examples_final[,"hist_state1"])))))
hist1[!hist1 %in% svd_states]
hist2 <- sort(as.character(as.vector(unlist(unique(examples_final[,"hist_state2"])))))
hist2[!hist2 %in% svd_states]
hist3 <- sort(as.character(as.vector(unlist(unique(examples_final[,"hist_state3"])))))
hist3[!hist3 %in% svd_states]

svd_states_aug <- sort(as.character(as.vector(unlist(unique(examples_final[,"svd_state"])))))
svd_states_aug[!svd_states_aug %in% svd_states]
target_states_aug <- sort(as.character(as.vector(unlist(unique(examples_final_aug[,"target_state"])))))
target_states_aug[!target_states_aug %in% svd_states]
hist1_aug <- sort(as.character(as.vector(unlist(unique(examples_final_aug[,"hist_state1"])))))
hist1_aug[!hist1_aug %in% svd_states]
hist2_aug <- sort(as.character(as.vector(unlist(unique(examples_final_aug[,"hist_state2"])))))
hist2_aug[!hist2_aug %in% svd_states]
hist3_aug <- sort(as.character(as.vector(unlist(unique(examples_final_aug[,"hist_state3"])))))
hist3_aug[!hist3_aug %in% svd_states]



uniqueStates <- unique(c(svd_states, target_states, hist1, hist2, hist3,
                         svd_states_aug, target_states_aug, hist1_aug, hist2_aug, hist3_aug)) %>% sort()
uniqueStates <- uniqueStates[uniqueStates != "missing"]
uniqueStates[!uniqueStates %in% svd_states]

lookup_new <- as.data.frame(cbind(state = uniqueStates, stateID = c(1:length(uniqueStates))))

# add a row for the "missing" state
lookup_new <- lookup_new %>% 
  rbind(c("missing", 0)) %>%
  mutate(stateID = as.numeric(stateID)) %>%
  arrange(stateID)

dim(lookup_new)

# check if there are states in the augmentation data that are not in the lookup
examples_final[!examples_final$svd_state %in% lookup_new$state, ]
examples_final[!examples_final$target_state %in% lookup_new$state, ]
examples_final[!examples_final$hist_state1 %in% lookup_new$state, ]
examples_final[!examples_final$hist_state2 %in% lookup_new$state, ]
examples_final[!examples_final$hist_state3 %in% lookup_new$state, ]

examples_final_aug[!examples_final_aug$svd_state %in% lookup_new$state, ]
examples_final_aug[!examples_final_aug$target_state %in% lookup_new$state, ]
examples_final_aug[!examples_final_aug$hist_state1 %in% lookup_new$state, ]
examples_final_aug[!examples_final_aug$hist_state2 %in% lookup_new$state, ]
examples_final_aug[!examples_final_aug$hist_state3 %in% lookup_new$state, ]


# check
head(lookup_new)
dim(lookup_new)


# write out
dbWriteTable(conn = simulation_db, name = "states_lookup_pruned_5_v16", value = lookup_new, overwrite = T)
write_csv(lookup_new, paste0(path, "/.../states_lookup_pruned_5_v16.csv"))


### count how many examples ----
metadata <- dbReadTable(simulation_db, "metadata_all")
lookup <- dbReadTable(simulation_db, "states_lookup_pruned_5_v16")
dim(lookup)

# take examples that occur more than 5 times
examples_much <- examples_final %>%
  group_by(svd_state) %>%
  mutate(count_state = n()) %>%
  ungroup() %>%
  group_by(target_state) %>%
  mutate(count_target_state = n()) %>%
  ungroup()

dim(examples_much)

dbDisconnect(simulation_db)




### the end ----------------------------------------------------------------------------------------------------------------------------


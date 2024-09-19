##################################################
### 02 --- add soil conditions to examples
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
library(collapse)


path <- "/.../"

### look at the simulations -----------------------------------
simulation_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(path, "/forest_simulation_db_v1.1.sqlite"))
tables_con <- dbListTables(simulation_db)

metadata <- dbReadTable(simulation_db, "metadata_all" )
examples <- dbReadTable(simulation_db, "examples_2m" )


examples_with_soil <- examples %>%
  left_join(metadata %>% 
              dplyr::select(uniqueID, ID, WHC, TextureSand, SoilDepth, AvailableNitrogen) %>% 
                                                 mutate(simulationID = ID) %>% 
                                                 dplyr::select(-ID), by = c("uniqueID", "simulationID")) %>% 
    dplyr::select(-flag) %>% 
    mutate(uniqueID = as.integer(uniqueID),
           simulationID = as.integer(simulationID),
           Year = as.integer(Year), 
           residence_time = as.integer(residence_time),
           target_time = as.integer(target_time))

dbWriteTable(conn = simulation_db, name = "examples_2m_soil", value = examples_with_soil, overwrite = T)

dbDisconnect(simulation_db)


### same for augmentation samples -----------------------------------------------------------------------------------

simulation_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(path, "/Projects/Resonate/data_portal/simulation_data/simulation_db_users_v6.sqlite"))
tables_con <- dbListTables(simulation_db)

metadata <- dbReadTable(simulation_db, "metadata_all")

examples_with_soil <- examples %>%
  left_join(metadata %>% dplyr::select(uniqueID, ID, WHC, TextureSand, SoilDepth, AvailableNitrogen) %>% 
                                                 mutate(simulationID = ID) %>% 
                                                 dplyr::select(-ID), by = c("uniqueID", "simulationID")) %>% 
    dplyr::select(-flag) %>% 
    mutate(uniqueID = as.integer(uniqueID),
           simulationID = as.integer(simulationID),
           Year = as.integer(Year), 
           residence_time = as.integer(residence_time),
           target_time = as.integer(target_time))



dbWriteTable(conn = simulation_db, name = "examples_2m_soil_augmentation", value = examples_with_soil, overwrite = T)

dbDisconnect(simulation_db)



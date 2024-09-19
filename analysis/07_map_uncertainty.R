##########################################################################
### 7. mapping uncertainties from DNN predictions ------------------------
##########################################################################


library(arrow)
library(tidyverse)
library(dplyr)
library(reshape)
library(terra)
library(raster)
library(ggplot2)
library(ggdist)
library(ggbeeswarm)
library(tidyterra)
library(sf)
library(RColorBrewer)
library(pals)
library(gridExtra)
library(data.table)
library(grid)
library(parallel)
library(MetBrewer)
library(scico)

library(dplyr, warn.conflicts = FALSE)

# source("process_dnn_predictions_growth_calc.R")
source("/functions/calculate_all_indicators_gridcell_rate.R")
source("/functions/calculate_csi.R")
source(paste0("init_state_lists.R"))

# define path
path <- "/.../"


# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
terraOptions(memfrac=0.5, tempdir = "/tmp/")

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


# load geo data
eu_shp <- vect(paste0(path, "/gis_data/europe_lowres.shp"))
proj_wgs <- "+proj=longlat +datum=WGS84 +no_defs"
proj_leae <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
eu_shp <- terra::project(eu_shp, proj_wgs)
eu_shp_sf <- st_as_sf(eu_shp)
eu_shp_sf_leae <- st_transform(eu_shp_sf, proj_leae)

x_rast <- rast(paste0(path, "/gis_data/reference_grid.tif"))
rast_df <- as.data.frame(x_rast, xy = T)
colnames(rast_df) <- c("x", "y", "point_id")


# loop over all species to analyse predictions
for(s in 1:length(species)){
  
  
  nam <- gsub("_", " ", species[s])
  nam2 <- species[s]
  print(nam2)
  nam_short <- toupper(paste0(substring(strsplit(nam2, "_")[[1]][1], 1, 2), substring(strsplit(nam2, "_")[[1]][2], 1, 2)))
  if(nam == "Pinus pinea"){nam_short <- "PIPN"}
  
  if(nam == "Fagus sylvatica"){nam2 <- "Fagus_sylvatica_sylvatica"}
  if(nam == "Larix decidua"){nam2 <- "Larix_decidua_decidua"}
  if(nam == "Quercus ilex"){nam2 <- "Quercus_ilex_ilex"}
  
  # seperate filenames of the species
  files_sp <- files[grepl(species[s], files)]
  init_states <- lookup_table_single[[nam_short]]
 
  # define rcps
  rcps <- c("rcp_8_5", "rcp_4_5", "rcp_2_6")
  
  # output list
  sp_data_list <- list()
  n <- 0
  
  for(c in rcps){
    
    files_scen <- files_sp[grepl(c, files_sp) | grepl("historical", files_sp)]
    files_scen <- files_scen[grepl(paste(init_states, collapse = "|"), files_scen)]

    # define output lists
    all_output <- list()
    
    # loop pover all timesteps
    for(t in 1:length(timesteps)){
      
      print(timesteps[t])
      files_time <- files_scen[grepl(timesteps[t], files_scen)]
      
      output_stats_states_load <- mclapply(files_time, process_dnn_predictions,
                                           timestep = timesteps[t],
                                           mc.cleanup = TRUE, mc.cores = 12)
      
      output_stats_states <- do.call(bind_rows, output_stats_states_load) %>% 
        dplyr::rename("init_state" = "state")
      
      all_output[[t]] <- output_stats_states
      
    } # close timesteps
    
    all_output <- do.call(bind_rows, all_output)
    
    
    uncertainty_hist <- all_output %>% 
      filter(timestep %in% c("1981-1990", "1991-2000", "2001-2010", "1981-2010")) %>% 
      group_by(point_id) %>% 
      summarise(mean_state_proba = mean(state_proba_1),
                mean_time_proba = mean(time_proba_new))
    
    
    uncertainty_fut <- all_output %>% 
      filter(!timestep %in% c("1981-1990", "1991-2000", "2001-2010", "1981-2010")) %>% 
      group_by(point_id) %>% 
      summarise(mean_state_proba = mean(state_proba_1),
                mean_time_proba = mean(time_proba_new))
    
    # plot growth baseline
    rast_df_ind_hist <- left_join(rast_df, na.omit(uncertainty_hist), by = "point_id")
    rast_hist <- rast(rasterFromXYZ(rast_df_ind_hist[, c("x", "y", "mean_state_proba")]))
    crs(rast_hist) <- crs(x_rast)
    rast_hist <- terra::project(rast_hist, proj_leae)
    
    rast_df_ind <- left_join(rast_df, na.omit(uncertainty_fut), by = "point_id")
    rast <- rast(rasterFromXYZ(rast_df_ind[, c("x", "y", "mean_state_proba")]))
    crs(rast) <- crs(x_rast)
    rast <- terra::project(rast, proj_leae)
    
    
    stack_csi <- c(rast_hist, rast)
    
    writeRaster(stack_csi, paste0(path, "/results/rasters/uncertainty_raster_", species[s], "_", c, ".tif"), overwrite = T)
    
    
    
    # same for the time
    rast_df_ind_hist <- left_join(rast_df, na.omit(uncertainty_hist), by = "point_id")
    rast_hist <- rast(rasterFromXYZ(rast_df_ind_hist[, c("x", "y", "mean_time_proba")]))
    crs(rast_hist) <- crs(x_rast)
    rast_hist <- terra::project(rast_hist, proj_leae)
    
    rast_df_ind <- left_join(rast_df, na.omit(uncertainty_fut), by = "point_id")
    rast <- rast(rasterFromXYZ(rast_df_ind[, c("x", "y", "mean_time_proba")]))
    crs(rast) <- crs(x_rast)
    rast <- terra::project(rast, proj_leae)
    
    
    stack_csi <- c(rast_hist, rast)
    
    writeRaster(stack_csi, paste0(path, "/results/rasters/time_uncertainty_raster_", species[s], "_", c, ".tif"), overwrite = T)
    
    
    
  } # close rcps
  
} # close sp loop


### plotting ----------------------------------------------------------------------------------------------------------

### load data ----
source("/data/public/Projects/Resonate/R_projects/svd/functions/make_hexagon.R")
library("rnaturalearth")
library("rnaturalearthdata")
library(gridExtra)

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, proj_leae)
europe <- st_crop(world, eu_shp_sf_leae)
europe <- europe[!europe$sovereignt %in% c("Tunisia", "Russia", "Algeria", "Turkey", "Cyprus", "Northern Cyprus", "Morocco", "Iceland"), ]
europe <- vect(europe)


rcps <- c("rcp_8_5", "rcp_4_5", "rcp_2_6")

for(c in rcps){
  
  hist_list <- list()
  fut_list <- list()
  
  time_hist_list <- list()
  time_fut_list <- list()
  
  species  <- c("Betula_pendula", "Pinus_sylvestris", "Quercus_robur", "Picea_abies",
                "Fagus_sylvatica", "Abies_alba", "Quercus_ilex",  "Pinus_halepensis", "Larix_decidua")
  
  for(s in 1:length(species)){
    
    
    nam <- gsub("_", " ", species[s])
    nam2 <- species[s]
    
    rasts <- rast(paste0(path, "/results/rasters/uncertainty_raster_", species[s], "_", c, ".tif"))
    r_1 <- rasts[[1]]
    r_2 <- rasts[[2]]
    
    shp <- vect(paste0("/gis_data/sp_range_nat/", species[s], "_shape_nat.shp"))
    shp <- crop(shp, r_2)
    shp <- terra::project(shp, proj_leae)
    
    
    
    cols <- met.brewer("OKeeffe1", 100, "continuous")
    map_1 <- ggplot(eu_shp_sf_leae) +
      geom_spatraster(data = r_1) +
      scale_fill_gradientn(colours = cols, na.value = NA, limits = c(0, 1),
                           breaks = c(0, 1), 
                           guide = guide_colorbar(title.position = "top")) +
      geom_sf(fill = "grey", alpha = 0.1) +
      geom_spatvector(data = shp, fill = "red", alpha = 0.1) +
      geom_spatvector(data = europe, fill = "transparent", col = "black") +
      theme_void() +
      labs(
        fill = paste0("prediction probability"),
        title = paste0(nam)
      ) +
      theme(legend.position = "none")
    
    cols <- met.brewer("OKeeffe1", 100, "continuous")
    map_2 <- ggplot(eu_shp_sf_leae) +
      geom_spatraster(data = r_2) +
      scale_fill_gradientn(colours = cols, na.value = NA, limits = c(0, 1),
                           breaks = c(0, 1), 
                           guide = guide_colorbar(title.position = "top")) +
      geom_sf(fill = "grey", alpha = 0.1) +
      geom_spatvector(data = shp, fill = "red", alpha = 0.1) +
      geom_spatvector(data = europe, fill = "transparent", col = "black") +
      theme_void() +
      labs(
        fill = paste0("prediction probability"),
        title = paste0(nam, " uncertainty")
      ) +
      theme(legend.position = "none")
    map_2
    
    
    hist_list[[s]] <- map_1
    fut_list[[s]] <- map_2
    
    
    # same for time --
    rasts <- rast(paste0(path, "/results/rasters/time_uncertainty_raster_", species[s], "_", c, ".tif"))
    r_1 <- rasts[[1]]
    r_2 <- rasts[[2]]
    
    cols <- met.brewer("OKeeffe1", 100, "continuous")
    map_1 <- ggplot(eu_shp_sf_leae) +
      geom_sf(fill = "grey", alpha = 0.7) +
      geom_spatraster(data = r_1) +
      scale_fill_gradientn(colours = cols, na.value = NA, limits = c(0, 1),
                           breaks = c(0, 1), 
                           guide = guide_colorbar(title.position = "top")) +
      geom_spatvector(data = shp, fill = "red", alpha = 0.1) +
      geom_spatvector(data = europe, fill = "transparent", col = "black") +
      theme_void() +
      labs(
        fill = paste0("prediction probability"),
        title = paste0(nam)
      ) +
      theme(legend.position = "none")
    
    cols <- met.brewer("OKeeffe1", 100, "continuous")
    map_2 <- ggplot(eu_shp_sf_leae) +
      geom_sf(fill = "grey", alpha = 0.7) +
      geom_spatraster(data = r_2) +
      scale_fill_gradientn(colours = cols, na.value = NA, limits = c(0, 1),
                           breaks = c(0, 1), 
                           guide = guide_colorbar(title.position = "top")) +
      
      geom_spatvector(data = shp, fill = "red", alpha = 0.1) +
      geom_spatvector(data = europe, fill = "transparent", col = "black") +
      theme_void() +
      labs(
        fill = paste0("prediction probability"),
        title = paste0(nam, " uncertainty")
      ) +
      theme(legend.position = "none")
    map_2
    
    
    
    time_hist_list[[s]] <- map_1
    time_fut_list[[s]] <- map_2
    
  }  
  
  # state historical
  legend <- cowplot::get_legend(hist_list[[1]] + theme(legend.position = "bottom",
                                                       legend.title = element_text(hjust = 1)))
  p_grid <- cowplot::plot_grid(plotlist = hist_list, ncol = 3)
  hist_plot <- cowplot::plot_grid(p_grid, legend, ncol = 1, rel_heights = c(1, 0.1))
  ggsave(paste0(path, "/results/maps/prediction_uncertainty_", c, "_hist.png"),
         hist_plot, dpi = 300, width = 12, height = 12, units = "in")
  
  # state fut
  legend <- cowplot::get_legend(fut_list[[1]] + theme(legend.position = "bottom",
                                                      legend.title = element_text(hjust = 1)))
  p_grid <- cowplot::plot_grid(plotlist = fut_list, ncol = 3)
  fut_plot <- cowplot::plot_grid(p_grid, legend, ncol = 1, rel_heights = c(1, 0.1))
  ggsave(paste0(path, "/results/maps/prediction_uncertainty_", c, "_fut.png"),
         fut_plot, dpi = 300, width = 12, height = 12, units = "in")
  
  # time historical
  legend <- cowplot::get_legend(time_hist_list[[1]] + theme(legend.position = "bottom",
                                                            legend.title = element_text(hjust = 1)))
  p_grid <- cowplot::plot_grid(plotlist = time_hist_list, ncol = 3)
  hist_plot <- cowplot::plot_grid(p_grid, legend, ncol = 1, rel_heights = c(1, 0.1))
  ggsave(paste0(path, "/results/maps/time_prediction_uncertainty_", c, "_hist.png"),
         hist_plot, dpi = 300, width = 12, height = 12, units = "in")
  
  # time fut
  legend <- cowplot::get_legend(time_fut_list[[1]] + theme(legend.position = "bottom",
                                                           legend.title = element_text(hjust = 1)))
  p_grid <- cowplot::plot_grid(plotlist = time_fut_list, ncol = 3)
  fut_plot <- cowplot::plot_grid(p_grid, legend, ncol = 1, rel_heights = c(1, 0.1))
  ggsave(paste0(path, "/results/maps/time_prediction_uncertainty_", c, "_fut.png"),
         fut_plot, dpi = 300, width = 12, height = 12, units = "in")
  
}


### end ---

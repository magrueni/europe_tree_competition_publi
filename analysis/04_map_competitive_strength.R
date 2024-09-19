########################################################################
### 4. mapping of competitive strength ---------------------------------
########################################################################



### libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(sf)
library(data.table)
library(terra)
library(gridExtra)
library(MetBrewer)


### load functions ----
source("/functions/make_hexagon.R")
library("rnaturalearth")
library("rnaturalearthdata")


### load geo data ----
eu_shp <- vect(paste0(path, "/gis_data/europe_lowres.shp"))
proj_wgs <- "+proj=longlat +datum=WGS84 +no_defs"
proj_leae <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
eu_shp <- terra::project(eu_shp, proj_wgs)
eu_shp_sf <- st_as_sf(eu_shp)
eu_shp_sf_leae <- st_transform(eu_shp_sf, proj_leae)

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, proj_leae)
europe <- st_crop(world, eu_shp_sf_leae)
europe <- europe[!europe$sovereignt %in% c("Tunisia", "Russia", "Algeria", "Turkey", "Cyprus", "Northern Cyprus", "Morocco", "Iceland"), ]
europe <- vect(europe)

species  <- c("Betula_pendula", "Pinus_sylvestris", "Quercus_robur", "Picea_abies",
              "Fagus_sylvatica", "Abies_alba", "Quercus_ilex",  "Pinus_halepensis", "Larix_decidua")



### load pre processed data from script 1 ---
lai <- read_csv(paste0(path, "/results/lai_absolute_change_10year.csv"))
hei <- read_csv(paste0(path, "/results/lai_absolute_change_10year.csv"))


# define rcps
rcps <- c("rcp_8_5", "rcp_4_5", "rcp_2_6")


### loop over rcps to do the maps
for(c in rcps){
  
  
  # define output lists
  plot_list <- list()
  hei_plot_list <- list()
  lai_plot_list <- list()
  
  rast_all <- rast()
  min_max <- list()
  
  
  # loop over the species
  for(s in 1:length(species)){
    
    # naming things
    nam <- gsub("_", " ", species[s])
    print(species[s])
    nam2 <- species[s]
    if(nam == "Fagus sylvatica"){nam2 <- "Fagus_sylvatica_sylvatica"}
    if(nam == "Larix decidua"){nam2 <- "Larix_decidua_decidua"}
    if(nam == "Quercus ilex"){nam2 <- "Quercus_ilex_ilex"}
    
    # load species range
    sp <- species[s]
    shp <- terra::vect(paste0(path, "/chorological_maps_dataset/", nam, "/shapefiles/", nam2, "_plg.shp"))
    shp <- terra::project(shp, proj_leae)
    
    
    # get LAI data for species
    r_1 <- lai %>% 
      filter(species == sp) %>% 
      filter(rcp == c)
    
    # create raster from it
    r_1 <- left_join(rast_df, na.omit(r_1), by = "point_id") 
    r_1 <- rast(rasterFromXYZ(r_1[, c("x", "y", "net_change")]))
    crs(r_1) <- proj_wgs
    r_1 <- terra::project(r_1, proj_leae)

    # get height data for the species
    r_2 <- hei %>% 
      filter(species == sp) %>% 
      filter(rcp == c)
    
    # create raster from it
    r_2 <- left_join(rast_df, na.omit(r_2), by = "point_id") 
    r_2 <- rast(rasterFromXYZ(r_2[, c("x", "y", "net_change")]))
    crs(r_2) <- proj_wgs
    r_2 <- terra::project(r_2, proj_leae)

    # put the rasters together to get the CSI raster
    r_fin <- (r_1 + r_2)/2
    r_fin <- crop(r_fin, europe)
    r_fin_vect <- r_fin
    r_fin_vect[!is.na(r_fin_vect)] <- 1
    r_fin_vect <- as.polygons(r_fin_vect, dissolve = T)
    
    # add to list
    min_max[[s]] <- cbind(min = min(values(r_fin), na.rm = T), max = max(values(r_fin), na.rm = T))
    rast_all <- c(rast_all, r_fin)
    
    
    ###  plotting ---
    cols <- met.brewer("OKeeffe1", 100, "continuous")
    map_diff <- ggplot(eu_shp_sf_leae) +
      geom_sf(fill = "grey", alpha = 0.7) +
      geom_spatraster(data = r_fin) +
      scale_fill_gradientn(colours = cols, na.value = NA, limits = c(-80, 80),
                           breaks = c(-80, 0, 80), 
                           guide = guide_colorbar(title.position = "top")) +
      geom_spatvector(data = europe, fill = "transparent", col = "black") +
      theme_void() +
      labs(
        fill = paste0("Change in competitive strength [%]"),
        title = paste0(nam)
      ) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0, vjust = -10)
      )
    
    map_hei <- ggplot(eu_shp_sf_leae) +
      geom_sf(fill = "grey", alpha = 0.7) +
      geom_spatraster(data = r_1) +
      scale_fill_gradientn(colours = cols, na.value = NA, limits = c(-100, 100),
                           breaks = c(-100, 0, 100), 
                           guide = guide_colorbar(title.position = "top")) +
      geom_spatvector(data = europe, fill = "transparent", col = "black") +
      theme_void() +
      labs(
        fill = paste0("Change in height growth [%]"),
        title = paste0(nam)
      ) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0, vjust = -10)
      )
    
    map_lai <- ggplot(eu_shp_sf_leae) +
      geom_sf(fill = "grey", alpha = 0.7) +
      geom_spatraster(data = r_2) +
      scale_fill_gradientn(colours = cols, na.value = NA, limits = c(-100, 100),
                           breaks = c(-100, 0, 100), 
                           guide = guide_colorbar(title.position = "top")) +
      # geom_spatvector(data = shp, fill = "red", alpha = 0.5) +
      geom_spatvector(data = europe, fill = "transparent", col = "black") +
      theme_void() +
      labs(
        fill = paste0("Change in LAI [%]"),
        title = paste0(nam)
      ) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0, vjust = -10)
      )
    
    # add plots to the lists
    plot_list[[s]] <- map_diff
    hei_plot_list[[s]] <- map_hei
    lai_plot_list[[s]] <- map_lai
    
    
  } # end species loop
  
  
  # make the assembled plots
  legend <- cowplot::get_legend(plot_list[[1]] + theme(legend.position = "bottom",
                                                       legend.title = element_text(hjust = 2)))
  p_grid <- cowplot::plot_grid(plotlist = plot_list, ncol = 3)
  csi_plot <- cowplot::plot_grid(p_grid, ncol = 1, rel_heights = c(1, 0.1))
  ggsave(paste0(path, "/results/maps/prediction_csi_", c, "_all_v46_final.png"),
         csi_plot, dpi = 300, width = 12, height = 12, units = "in")
  
  ggsave(paste0(path, "/results/maps/prediction_csi_", c, "_all_v46_final_legend.png"),
         legend, dpi = 300, width = 12, height = 12, units = "in")
  
  legend <- cowplot::get_legend(hei_plot_list[[1]] + theme(legend.position = "bottom",
                                                           legend.title = element_text(hjust = 1)))
  p_grid <- cowplot::plot_grid(plotlist = hei_plot_list, ncol = 3)
  hei_plot <- cowplot::plot_grid(p_grid, legend, ncol = 1, rel_heights = c(1, 0.1))
  ggsave(paste0(path, "/results/maps/prediction_height_", c, "_v46_final.png"),
         hei_plot, dpi = 300, width = 12, height = 12, units = "in")
  
  legend <- cowplot::get_legend(lai_plot_list[[1]] + theme(legend.position = "bottom",
                                                           legend.title = element_text(hjust = 1)))
  p_grid <- cowplot::plot_grid(plotlist = lai_plot_list, ncol = 3)
  lai_plot <- cowplot::plot_grid(p_grid, legend, ncol = 1, rel_heights = c(1, 0.1))
  ggsave(paste0(path, "/results/maps/prediction_lai_", c, "_v46_final.png"),
         lai_plot, dpi = 300, width = 12, height = 12, units = "in")
  
  
  if(c %in% c("rcp_2_6", "rcp_4_5")){
    
    legend <- cowplot::get_legend(plot_list[[1]] + theme(legend.position = "bottom",
                                                         legend.title = element_text(hjust = 1)))
    p_grid <- cowplot::plot_grid(plotlist = plot_list, ncol = 3)
    csi_plot <- cowplot::plot_grid(p_grid, legend, ncol = 1, rel_heights = c(1, 0.1))
    ggsave(paste0(path, "/results/maps/prediction_csi_", c, "_all_v46_final.png"),
           csi_plot, dpi = 300, width = 12, height = 12, units = "in")
    
    
  }
  
  # write out the raster stack with results
  writeRaster(rast_all, paste0(path, "/results/rasters/csi_rast_new_", c, ".tif"), overwrite = T)

}# close RCPS

### end

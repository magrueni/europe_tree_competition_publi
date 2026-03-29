##########################################################################
### 6. mapping of competitive strength -----------------------------------
##########################################################################


### libraries ---
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(sf)
library(data.table)
library(terra)
library(gridExtra)
library(scico)
library(rnaturalearth)
library(rnaturalearthdata)


### load functions ----
source("/functions/make_hexagon.R")

### define path
path <- "/.../"


### load geo data ---------------------------------------------------------------------------------------
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


forest_cover <- read_csv(paste0(path, "/gis_data/reference_grid_forest_cover.csv"))
forest_cover <- forest_cover %>% mutate(forest_cover = ifelse(is.na(forest_cover), 0, forest_cover))
x_rast <- rast(paste0(path, "/clim_data/reference_grid.tif"))
x_rast_proj <- terra::project(x_rast, proj_leae)
ref_grid <- as.data.frame(x_rast, xy = T)
ref_grid <- ref_grid %>% dplyr::rename("point_id" = "reference_grid")
pixel_area <- cellSize(x_rast, unit = "m")
pixel_area_df <- as.data.frame(pixel_area, xy = T)

area_df <- ref_grid %>% 
  left_join(., forest_cover %>%
              dplyr::select(point_id, forest_cover), by = c("point_id")) %>% 
  left_join(pixel_area_df, by = c("x", "y"))


### load pre processed data from script 1 -------------------------------------------
dom <- read_csv(paste0(path, "/results/dom_absolute_change_10year.csv"))

# filter value for dominance
min_val <- 0.05

# define rcps
rcps <- c("rcp_8_5", "rcp_4_5", "rcp_2_6")


# loop over rcps to do the maps
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
    if(nam == "Larix decidua"){next}
    
    # load species range
    sp <- species[s]
    shp <- vect(paste0(path, "/gis_data/sp_range_nat/", species[s], "_shape_nat.shp"))
    shp <- terra::project(shp, proj_leae)
    
    dom_dist <- read_csv(paste0(path, "/gis_data/range_edges/sp_dom_vals_", species[s], "_v2.csv"))
    dom_dist <- area_df %>%
      left_join(dom_dist %>% dplyr::select(point_id, dom), by = c("point_id")) %>%
      mutate(dom = ifelse(is.na(dom), 0, dom))
    
    dom_range <- read_csv(paste0(path, "/gis_data/range_edges/sp_dom_ids", species[s], ".csv"))
    dom_dist <- dom_range %>%
      dplyr::rename("point_id" = "reference_grid") %>%
      left_join(., dom_dist, by = c("point_id"))
    
    dom_dist <- dom_dist %>% 
      mutate(forest_area = forest_cover * area,
             forest_area_share = dom/100 * forest_area / area) %>% 
      mutate(forest_area_share = ifelse(forest_cover < 0.05, 0, forest_area_share),
             forest_area_share = ifelse(forest_area_share < 0.05, 0, forest_area_share)) %>% 
      mutate(forest_area_share = ifelse(is.na(forest_area_share), 0, forest_area_share),
             forest_area_share = ifelse(is.infinite(forest_area_share), 0, forest_area_share))
    
    dom_dist <- dom_dist %>% dplyr::select(point_id, dom = forest_area_share)
    
    
    # get LAI data for species
    r_1 <- dom %>% 
      filter(species == sp) %>% 
      filter(rcp == c)
    
    # create raster from it
    r_1 <- left_join(ref_grid, dom_dist, by = c("point_id")) %>% 
      left_join(., na.omit(r_1), by = "point_id")
    r_1 <- r_1 %>% 
      # mutate(weighted_change = net_change * dom/100)
      mutate(weighted_change = ifelse(dom < min_val, 0, net_change))
    
    r_1 <- rast(rasterFromXYZ(r_1[, c("x", "y", "weighted_change")]))
    crs(r_1) <- proj_wgs
    r_1 <- terra::project(r_1, proj_leae)
    plot(r_1)
    
    # put the rasters together to get the CSI raster
    r_fin <- r_1
    r_fin <- terra::crop(r_fin, europe)
    r_fin <- terra::mask(r_fin, shp)
    r_fin <- terra::mask(r_fin, europe)
    r_fin <- terra::project(r_fin, x_rast_proj)
    r_fin_vect <- r_fin
    r_fin_vect[!is.na(r_fin_vect)] <- 1
    r_fin_vect <- as.polygons(r_fin_vect, dissolve = T)
    
    # add to list
    rast_all <- c(rast_all, r_fin)
 
  } # end species loop
  
  
  
  # write out the raster stack with results
  writeRaster(rast_all, paste0(path_out, "/results/domstack_", c, ".tif"), overwrite = T)

  
}# close RCPS





# plot for a RCP
c <- "rcp_8_5"

rast_all <- rast(paste0(path_out, "/results/domstack_", c, ".tif"))

num_list <- list() 
den_list <- list() 

species  <- c("Betula_pendula", "Pinus_sylvestris", "Quercus_robur", "Picea_abies",
              "Fagus_sylvatica", "Abies_alba", "Quercus_ilex",  "Pinus_halepensis")


for(s in 1:length(species)){
  
  # if(nam == "Larix decidua"){next}
  
  # read dominance per species
  dom_dist <- read_csv(
    paste0(path, "/svd_dnn/range_edges/sp_dom_vals_", species[s], "_v2.csv")
  )
  dom_dist <- dom_dist %>% mutate(dom = dom/100)
  r_dom <- rast(rasterFromXYZ(dom_dist[, c("x", "y", "dom")]))
  crs(r_dom) <- proj_wgs
  
  # get corresponding values raster
  r <- rast_all[[s]]  
  r_dom <- terra::project(r_dom, r)  # align
  
  # weighted numerator (value * dom)
  r_weighted <- r_dom * r
  plot(r_weighted)
  
  # store
  num_list[[s]] <- r_weighted
  den_list[[s]] <- r_dom
}

# sum across species
num_sum <- rast(num_list)
num_sum <- app(num_sum, "sum", na.rm = T) # total change in dominance

den_sum <- rast(den_list)
den_sum <- app(den_sum, "sum", na.rm = T)
den_sum[den_sum == 0] <- NA

plot(den_sum)

# weighted mean per cell
r_weighted_mean <- num_sum / den_sum # divide by sum of weights to get the mean change in dominance
plot(r_weighted_mean)


r_weighted_mean_crop <- crop(r_weighted_mean, europe)
r_weighted_mean_mask <- mask(r_weighted_mean_crop, europe)

map_diff <- ggplot() +
  # --- white base under everything (so outside Europe is white)
  geom_sf(data = europe, fill = "grey", color = "grey") +
  
  # --- grey base polygons (optional, if you want land shading)
  geom_sf(data = eu_shp_sf_leae, fill = "grey", color = NA) +
  
  # --- raster on top (NAs within Europe will show as grey)
  geom_spatraster(data = r_weighted_mean_mask) +
  
  scale_fill_scico(
    palette = "vik",
    na.value = "transparent",       # grey only where raster is NA (inside Europe)
    limits = c(-100, 100),
    breaks = c(-100, -50, 0, 50, 100),
    direction = -1,
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0
    )
  ) +
  
  # --- borders on top
  geom_spatvector(data = europe, fill = "transparent", col = "black") +
  
  theme_classic() +
  labs(fill = "Shift in dominance [%]") +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, vjust = 0.65, hjust = 0.25),
    axis.title = element_text(size = 12),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 10),
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white", colour = NA),  # white outside
    plot.background = element_rect(fill = "white", colour = NA)
  )

map_diff


ggsave(paste0(path, "/figures/hotspots_dom_", c, "_Figure3.png"),
       map_diff, dpi = 300, width = 7.5, height = 7.5, units = "in")
writeRaster(r_weighted_mean_mask, paste0(path_out, "/figure_data/figure3.tif"))



### calculate the percent area decreasing per RCP
r_weighted_mean_mask

area <- terra::cellSize(r_weighted_mean_mask)
area <- terra::mask(area, r_weighted_mean_mask)
area_df <- as.data.frame(area)
total_area <- sum(area_df$area) # in m2
total_area <- total_area/10000 # to ha
total_area

negative <- r_weighted_mean_mask
negative[negative >=0] <- NA
negative[negative ==0] <- NA
area <- terra::cellSize(negative)
area <- terra::mask(area, negative)
area_df <- as.data.frame(area)
neg_area <- sum(area_df$area) # in m2
neg_area <- neg_area/10000 # to ha
neg_area


posative <- r_weighted_mean_mask
posative[posative <= 0] <- NA
posative[posative == 0] <- NA
area <- terra::cellSize(posative)
area <- terra::mask(area, posative)
area_df <- as.data.frame(area)
pos_area <- sum(area_df$area) # in m2
pos_area <- pos_area/10000 # to ha
pos_area


neg_area/total_area
pos_area/total_area






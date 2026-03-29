#########################################################################################################
### 2. define range sizes ------------------------------------------------------------------------------
### in this script we use the data from Caudullo et al. 2017 (https://doi.org/10.1016/j.dib.2017.05.007)
### to get the ranges of the study species
### -----------------------------------------------------------------------------------------------------



# load libraries
library(terra)
library(raster)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(tidyterra)
library(arrow)
library(collapse)
library(sf)

# define path
path <- "/.../"



# define rcps
rcps <- c("rcp_8_5", "rcp_4_5", "rcp_2_6")
proj_leae <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
proj_wgs <- "+proj=longlat +datum=WGS84 +no_defs"
x_rast <- rast(paste0(path, "/gis_data/reference_grid.tif"))
rast_df <- as.data.frame(x_rast, xy = T)
colnames(rast_df) <- c("x", "y", "point_id")

eu_shp <- shapefile(paste0(path, "/clim_data/europe_lowres.shp"))
hex_ecu <- make_grid(eu_shp, type = "hexagonal", cell_width = 100000, clip = TRUE)

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, proj_leae)
europe <- st_crop(world, eu_shp)
europe <- europe[!europe$sovereignt %in% c("Tunisia", "Russia", "Algeria", "Turkey", "Cyprus", "Northern Cyprus", "Morocco", "Iceland"), ]

europe <- vect(europe)


dom <- read_csv(paste0(path, "/results/dom_absolute_change_10year.csv"))


### from brus maps get the dominant species per grid ---------------------------
dom_veg_all <- list.files(paste0(path, "/gis_data/EU_TreeMap_Brus_etal/"),
                          pattern = ".tif", full.names = T)

dom_names <- c("FagusSpp", "PinusSylvestris", "AbiesSpp", "PiceaSpp", "LarixSpp", "BetulaSpp", "PinesMisc", 
               "QuercusRoburPetraea", "QuercusMisc")

# define species
species  <- c("Fagus sylvatica", "Pinus sylvestris", "Abies alba",
              "Picea abies", "Larix decidua", "Betula pendula",
              "Pinus halepensis", "Quercus robur", "Quercus ilex")

dom_veg <- dom_veg_all[grepl("Dominant", dom_veg_all)]
dom_rast <- rast(dom_veg)

dom_rast_wgs <- terra::project(dom_rast, proj_wgs)


forest_mask <- rast(paste0(path, "/gis_data/forest_mask_agg.tif"))

# define order for different datasets
sp <- c("abal", "algl", "bepe", "cabe", "casa",
        "eugl", "fasy", "frex", "lade", "broa",
        "coni", "pine", "quer", "piab", "pipi",
        "pisy", "potr", "psme", "quro", "rops")

my_species <- list(3, NA, 5, NA, NA,
                   NA, 1, NA, NA, NA,
                   NA, NA, NA, 4, NA,
                   2, NA, NA, 7, NA)

my_sp_dom <- c(7, 16, 1, 14, 9, 3, 12, 19, 13)
dom_rast2 <- dom_rast
dom_rast2[dom_rast2 != c(7, 16, 1, 14, 9, 3, 12, 19, 13)] <- NA

dom_rast3 <- app(dom_rast2, mean, na.rm = T)
dom_rast3 <- aggregate(dom_rast3, fact = 10, fun = "modal", na.rm = T)

dom_rast3 <- terra::project(dom_rast3, dom_rast)
dom_rast3 <- terra::mask(dom_rast3, dom_rast)


rast_ids <- terra::project(x_rast, dom_rast3, method = "mode")
rast_ids <- terra::crop(rast_ids, dom_rast3)
rast_ids <- terra::mask(rast_ids, dom_rast3)

size_list <- list()
dev.off()
par(mfrow = c(1,2))

for(i in 1:length(species)){
  
  focalsp <- species[i]
  dom_nr <- my_sp_dom[i]
  
  nam <- gsub(" ", "_", species[i])
  shp <- vect(paste0(path, "/gis_data/sp_range_nat/", nam, "_shape_nat.shp"))
  shp <- terra::project(shp, proj_leae)
  
  
  masked_r <- dom_rast3
  masked_r <- terra::mask(masked_r, shp)  
  # plot(masked_r, main = focalsp)
  
  masked_dom <- masked_r
  masked_dom[masked_dom != dom_nr] <- NA
  
  
  ids_rast <- terra::mask(rast_ids, masked_dom)
  # plot(ids_rast, main = focalsp)
  
  sp_dom_ids <- as.data.frame(ids_rast, xy = T) %>% drop_na() %>% distinct()
  write_csv(sp_dom_ids, paste0(path_out, "/range_edges/sp_dom_ids", species[i], ".csv"))
  
  dom_sp_f <- dom_veg_all[grepl(paste0(dom_names[i],".tif"), dom_veg_all)]
  dom_sp_rast <- rast(dom_sp_f)
  dom_sp_rast_masked <- terra::mask(dom_sp_rast, shp)
  plot(dom_sp_rast_masked)
  writeRaster(dom_sp_rast_masked, paste0(path_out, "/range_edges/sp_dom_layer_", species[i], ".tif"), overwrite = T)
  
  
  range_sizes <- as.data.frame(cbind(
    species_name = species[i],
    dom_range = nrow(na.omit(as.data.frame(ids_rast))),
    whole_range = nrow(na.omit(as.data.frame(masked_r)))
  ))
  
  print(range_sizes)
  
  size_list[[i]] <- range_sizes
  
}

size_list <- as.data.frame(do.call(rbind, size_list))
size_list %>% mutate(whole_range = as.numeric(whole_range)) %>% arrange(-whole_range)

write_csv(size_list, paste0(path_out, "/range_edges/range_size_all_species.csv"))







### calc percent of dominated forest by conifers ---
library(terra)

dom_veg <- list.files(paste0(path, "/Data/GIS_data/europe/dominant_trees/EU_TreeMap_Brus_etal/"),
                      pattern = ".tif", full.names = T)

dom_veg <- dom_veg[grepl("Dominant", dom_veg)]
dom_rast2 <- rast(dom_veg)
dom_rast2[dom_rast2 != c(1, 9, 14, 16)] <- NA
dom_rast2 <- app(dom_rast2, mean, na.rm = T)

100/nrow(na.omit(as.data.frame(dom_rast)))*nrow(na.omit(as.data.frame(dom_rast2)))

# mask with actual forest mask---
forest_mask <- rast(paste0(path, "/gis_data/forest_mask_agg.tif"))
forest_mask <- terra::project(forest_mask, dom_rast)

dom_rast_masked <- terra::mask(dom_rast, forest_mask)
plot(dom_rast_masked)

dom_rast2_masked <- dom_rast_masked
dom_rast2_masked[dom_rast2_masked != c(1, 9, 14, 16)] <- NA
dom_rast2_masked <- app(dom_rast2_masked, mean, na.rm = T)

nrow(na.omit(as.data.frame(dom_rast2_masked)))*100
nrow(na.omit(as.data.frame(dom_rast2_masked)))*100 * 19/100
100/nrow(na.omit(as.data.frame(dom_rast_masked)))*nrow(na.omit(as.data.frame(dom_rast2_masked)))


# calculate the percentage of area affected by dominance shift
dom_veg <- list.files(paste0(path, "/gis_data/EU_TreeMap_Brus_etal/"),
                      pattern = ".tif", full.names = T)

dom_veg <- dom_veg[grepl("Dominant", dom_veg)]
dom_rast2 <- rast(dom_veg)
dom_rast2[dom_rast2 != c(1, 3, 7, 12, 13, 14, 16, 19)] <- NA
dom_rast2 <- app(dom_rast2, mean, na.rm = T)

100/nrow(na.omit(as.data.frame(dom_rast)))*nrow(na.omit(as.data.frame(dom_rast2)))

# mask with actual forest mask---
forest_mask <- rast(paste0(path, "/gis_data/forest_mask_agg.tif"))
forest_mask <- terra::project(forest_mask, dom_rast)

dom_rast_masked <- terra::mask(dom_rast, forest_mask)
plot(dom_rast_masked)

dom_rast2_masked <- dom_rast_masked
dom_rast2_masked[dom_rast2_masked != c(1, 3, 7, 12, 13, 14, 16, 19)] <- NA
dom_rast2_masked <- app(dom_rast2_masked, mean, na.rm = T)

nrow(na.omit(as.data.frame(dom_rast2_masked)))*100
nrow(na.omit(as.data.frame(dom_rast2_masked)))*100 * 19/100
nrow(na.omit(as.data.frame(dom_rast2_masked)))*100 * 15/10
100/nrow(na.omit(as.data.frame(dom_rast_masked)))*nrow(na.omit(as.data.frame(dom_rast2_masked)))


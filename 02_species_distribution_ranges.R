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

# define species
species  <- c("Fagus sylvatica", "Pinus sylvestris", "Abies alba",
              "Picea abies", "Larix decidua", "Betula pendula",
              "Pinus halepensis", "Quercus robur", "Quercus ilex")


# loop over species and create a shape file with the distribution range
# in some cases it's the naative and the naturalized 


for(i in species){
  
  
  print(i)
  nam <- gsub(" ", "_", i)
  nam2 <- nam
  
  if(i == "Fagus sylvatica"){nam2 <- "Fagus_sylvatica_sylvatica"}
  if(i == "Larix decidua"){nam2 <- "Larix_decidua_decidua"}
  if(i == "Quercus ilex"){nam2 <- "Quercus_ilex_ilex"}
  
  # load range
  fls <- list.files(paste0(path, "/gis_data/chorological_maps_dataset/", i , "/shapefiles/"),
                    pattern = "*plg_clip.shp", full.names = T)
  fls <- fls[grepl(nam2, fls)]
  
  
  if(length(fls) == 0){
    shp <- vect(paste0(path, "/gis_data/chorological_maps_dataset/", i, "/shapefiles/", nam2, "_plg.shp"))
  }else{
  
  if(length(fls) == 2){
    shp1 <- read_sf(fls[1])
    shp2 <- read_sf(fls[2])
    shp <- vect(st_union(shp1, shp2))
    }else{
      shp <- vect(fls[1])
    }
  }

  
  writeVector(shp, paste0("/gis_data/sp_range_nat/", nam, "_shape_nat.shp"), overwrite = T)
  
  gc()
  tmpFiles(remove = T)
  
}



### ----------------------------------------------------------------------------
### in a second step, we can calutlate the range size and
### the size of the domanited area per species 


proj_leae <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
proj_wgs <- "+proj=longlat +datum=WGS84 +no_defs"
x_rast <- rast(paste0(path, "/gis_data/reference_grid.tif"))
rast_df <- as.data.frame(x_rast, xy = T)
colnames(rast_df) <- c("x", "y", "point_id")


# from brus maps get the dominant species per grid ---------------------------
dom_veg <- list.files(paste0(path, "/gis_data/EU_TreeMap_Brus_etal/"),
                      pattern = ".tif", full.names = T)

species  <- c("Fagus_sylvatica", "Pinus_sylvestris", "Abies_alba",
              "Picea_abies", "Larix_decidua", "Betula_pendula", 
              "Pinus_halepensis", "Quercus_robur", "Quercus_ilex")

dom_veg <- dom_veg[grepl("Dominant", dom_veg)]
dom_rast <- rast(dom_veg)

dom_rast_wgs <- terra::project(dom_rast, proj_wgs)

# species from Brus
sp <- c("abal", "algl", "bepe", "cabe", "casa",
        "eugl", "fasy", "frex", "lade", "broa",
        "coni", "pine", "quer", "piab", "pipi",
        "pisy", "potr", "psme", "quro", "rops")

# my species numbers
my_species <- list(3, NA, 5, NA, NA,
                   NA, 1, NA, NA, NA,
                   NA, NA, NA, 4, NA,
                   2, NA, NA, 7, NA)

# isolate the species we have
my_sp_dom <- c(7, 16, 1, 14, 9, 3, 12, 19, 13)
dom_rast2 <- dom_rast
dom_rast2[dom_rast2 != c(7, 16, 1, 14, 9, 3, 12, 19, 13)] <- NA

# use modal to make sure coastlines are covered
dom_rast3 <- app(dom_rast2, mean, na.rm = T)
dom_rast3 <- aggregate(dom_rast3, fact = 10, fun = "modal", na.rm = T)
dom_rast3 <- terra::project(dom_rast3, dom_rast)
dom_rast3 <- terra::mask(dom_rast3, dom_rast)

# project to study resolution
rast_ids <- terra::project(x_rast, dom_rast3, method = "mode")
rast_ids <- terra::crop(rast_ids, dom_rast3)
rast_ids <- terra::mask(rast_ids, dom_rast3)


# create outputlist
size_list <- list()
dev.off()
par(mfrow = c(1,2))

# loop over species
for(i in 1:length(species)){
  
  # focal species name
  focalsp <- species[i]
  dom_nr <- my_sp_dom[i]
  
  # load distribution range
  nam <- gsub(" ", "_", species[i])
  shp <- vect(paste0(path, "/gis_data/sp_range_nat/", nam, "_shape_nat.shp"))
  shp <- terra::project(shp, proj_leae)
  
  # mask the dominat species map with the distribution range of the species
  masked_r <- dom_rast3
  masked_r <- terra::mask(masked_r, shp)  
  
  # isolate the gridcells that are dominated by focal species
  masked_dom <- masked_r
  masked_dom[masked_dom != dom_nr] <- NA
  
  # get the cell ids for this grids
  ids_rast <- terra::mask(rast_ids, masked_dom)
  plot(ids_rast, main = focalsp)
  
  # write out dataframe with the cell ids
  sp_dom_ids <- as.data.frame(ids_rast) %>% drop_na() %>% distinct()
  write_csv(sp_dom_ids, paste0(path, "/gis_data/range_edges/sp_dom_ids", species[i], ".csv"))
  
  # put everything together for the output
  range_sizes <- as.data.frame(cbind(
    species_name = species[i],
    dom_range = nrow(na.omit(as.data.frame(ids_rast))),
    whole_range = nrow(na.omit(as.data.frame(masked_r)))
  ))

  # put in list
  size_list[[i]] <- range_sizes
  
}

# bind together and sort
size_list <- as.data.frame(do.call(rbind, size_list))
size_list %>% mutate(whole_range = as.numeric(whole_range)) %>% arrange(-whole_range)

# write out
write_csv(size_list, paste0(path, "/gis_data/range_edges/range_size_all_species.csv"))


### calc percent of dominated forest by conifers --- ---------------------------
dom_veg <- list.files(paste0(path, "/gis_data/EU_TreeMap_Brus_etal/"),
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

nrow(na.omit(as.data.frame(dom_rast2_masked)))
100/nrow(na.omit(as.data.frame(dom_rast_masked)))*nrow(na.omit(as.data.frame(dom_rast2_masked)))



  
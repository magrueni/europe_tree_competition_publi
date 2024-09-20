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


species  <- c("Fagus_sylvatica", "Pinus_sylvestris", "Abies_alba", "Picea_abies", 
              "Larix_decidua", "Betula_pendula", "Pinus_halepensis", "Quercus_robur",
              "Quercus_ilex")

x_rast <- rast(paste0(path, "/gis_data/reference_grid.tif"))
rast_df <- as.data.frame(x_rast, xy = T)
colnames(rast_df) <- c("x", "y", "point_id")

eu_shp <- shapefile(paste0(path, "/gis_data/europe_lowres.shp"))
hex_ecu <- make_grid(eu_shp, type = "hexagonal", cell_width = 100000, clip = TRUE)




### load pre processed data from script 1 -------------------------------------------
dom <- read_csv(paste0(path, "/results/dom_absolute_change_10year.csv"))


# define rcps
rcps <- c("rcp_8_5", "rcp_4_5", "rcp_2_6")



# from brus maps get the dominant species per grid 
dom_veg <- list.files(paste0(path, "/EU_TreeMap_Brus_etal/"),
                      pattern = ".tif", full.names = T)

species  <- c("Fagus_sylvatica", "Pinus_sylvestris", "Abies_alba", "Picea_abies", 
              "Betula_pendula", "Pinus_halepensis", "Quercus_robur",
              "Quercus_ilex")

species_names  <- c("Fagus_sylvatica", "Pinus_sylvestris", "Abies_alba", "Picea_abies", 
                    "Pinus_halepensis", "Quercus_robur",
                    "Quercus_ilex", "Betula_pendula")


dom_veg <- dom_veg[grepl("Dominant", dom_veg)]
dom_rast <- rast(dom_veg)


# species from brus maps
sp <- c("abal", "algl", "bepe", "cabe", "casa",
        "eugl", "fasy", "frex", "lade", "broa",
        "coni", "pine", "quer", "piab", "pipi",
        "pisy", "potr", "psme", "quro", "rops")

# assign to my species numbers
my_species <- list(3, NA, 5, NA, NA,
                   NA, 1, NA, NA, NA,
                   NA, 6, 8, 4, NA,
                   2, NA, NA, 7, NA)


# remove everything but my species
dom_rast2 <- dom_rast
dom_rast2[dom_rast2 != c(1, 3, 7, 12, 13, 14, 16, 19)] <- NA

# get the mean and fill some NAs 
dom_rast3 <- app(dom_rast2, mean, na.rm = T)
dom_rast3 <- aggregate(dom_rast3, fact = 10, fun = "modal", na.rm = T)
dom_rast3 <- terra::project(dom_rast3, dom_rast)
dom_rast3 <- terra::mask(dom_rast3, dom_rast)


# loop over RCPs
for(c in rcps){
  
  # get rcp specific data
  dom_rcp <- dom %>% filter(rcp == c)
  
  
  # output lists
  stack <- rast()
  names_sp <- c()
  
  # loop over species
  for(i in 1:length(species)){
    
    
    focalsp <- species[i]
    dom_sp <- dom_rcp %>% filter(species == focalsp)
    if(nrow(dom_sp) == 0){next}
    
    names_sp <- c(names_sp, focalsp)
    
    # create raster from it
    r_1 <- left_join(rast_df, na.omit(dom_sp), by = "point_id") %>% 
      mutate(net_change = net_change)
    
    r_1 <- rast(rasterFromXYZ(r_1[, c("x", "y", "net_change")]))
    crs(r_1) <- proj_wgs
    r_1 <- terra::project(r_1, proj_leae)
    stack <- c(stack, r_1)
    
  }
  
  
  names(stack) <- names_sp
  
  # get mean of stack and remove everything that is more than 5% negative
  stack_test <- app(stack, mean, na.rm = T)
  stack_test[stack_test > -5] <- 0

  
  all_rasts <- rast()
  for(i in c(1, 3, 7, 12, 13, 14, 16, 19)){
    
    # get dominance raster
    x <- dom_rast3
    x[x != i] <- NA
    
    # get my species raster
    my_sp <- species[my_species[[i]]]
    my_sp_rast <- stack[[names(stack) == my_sp]]
    my_sp_rast <- app(my_sp_rast, mean)
    
    # project and mask with dominance raster
    x <- terra::project(x, my_sp_rast)
    masked_r <- terra::mask(my_sp_rast, x)

    # store in stack
    all_rasts <- c(all_rasts, masked_r)
    
  }
  
  # calculate the sum and focus on negative shifts
  all_rasts_sum <- app(all_rasts, sum, na.rm = T)
  all_rasts_sum[all_rasts_sum > 0] <- 0
  all_rasts_sum_count <- all_rasts_sum
  all_rasts_sum_count[!is.na(all_rasts_sum_count)] <- 1
  
  # convert to hexagons
  hex_srtm <- raster::extract(raster(all_rasts_sum), hex_ecu, fun = mean, na.rm = TRUE, sp = TRUE)
  hex_srtm_modal <- raster::extract(raster(all_rasts_sum_count), hex_ecu, fun = sum, na.rm = TRUE, sp = TRUE)
  sf_obj <- st_as_sf(hex_srtm)
  
  # plot
  p2 <- ggplot() +
    geom_sf(data = sf_obj, aes(fill = sum)) +
    scale_fill_scico(palette = "lajolla", na.value = "lightgrey",
                     limits = c(-60, 0),
                     breaks = c(-50, -25, 0),
                     direction = -1,
                     guide = guide_colorbar(title.position = "top", 
                                            title.hjust = 0)) + 
    geom_spatvector(data = europe, fill = "transparent", col = "black") +
    theme_classic() +
    labs(fill = paste0("shift in dominance [%]"))+ 
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(size = 12, vjust = 0.65, hjust = 0.25),
      axis.title = element_text(size = 12),
      axis.title.x = element_blank(),
      strip.text = element_text(size = 10),
      legend.position = "bottom")
  
  p2
  ggsave(p2, filename = paste0(path, "/results/maps/loss_comp_all_species_by_dominant_", c, ".png"), 
         dpi = 300, width = 7.5, height = 7.5, units = "in")

  
  # get the numbers
  sf_obj_df <- as.data.frame(sf_obj) %>% dplyr::select(sum)
  numbers <- sf_obj_df %>% 
    mutate(sum = ifelse(sum > -5, 0, sum)) %>%
    mutate(sum = ifelse(is.na(sum), 0, sum)) %>%
    mutate(sum = ifelse(sum != 0, 1, 0)) %>%
    group_by(sum) %>%
    summarize(count = n()) %>% 
    mutate(all = sum(count)) %>% 
    mutate(pct = 100*count/all)
  
  print(numbers)
  
}




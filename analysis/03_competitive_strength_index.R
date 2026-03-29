##############################################################################
### 3. continental scale competitive strength --------------------------------
### in this script we calculate the CSI and plot results ---------------------
##############################################################################


### load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(sf)
library(data.table)

# define path
path <- "/.../"
path_out <- "/.../"


# define proj
proj_leae <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

# get dominance
forest_cover <- read_csv(paste0(path0, "/gis_data/reference_grid_forest_cover.csv"))
forest_cover <- forest_cover %>% 
  mutate(forest_cover = ifelse(is.na(forest_cover), 0, forest_cover)) %>% 
  mutate(forest_cover = ifelse(forest_cover < 0.05, 0, forest_cover))

x_rast <- rast(paste0(path0, "/gis_data/reference_grid.tif"))
x_rast_proj <- terra::project(x_rast, proj_leae)
ref_grid <- as.data.frame(x_rast, xy = T)
pixel_area <- cellSize(x_rast, unit = "m")
pixel_area_df <- as.data.frame(pixel_area, xy = T)

area_df <- ref_grid %>% dplyr::rename("point_id" = "reference_grid") %>% 
  left_join(., forest_cover %>%
              dplyr::select(point_id, forest_cover), by = c("point_id")) %>% 
  left_join(pixel_area_df, by = c("x", "y")) %>% 
  mutate(forest_area = forest_cover * area)

dom_dist <- read_csv(paste0(path, "/range_edges/all_sp_dom_vals.csv"))
dom_dist <- dom_dist %>% dplyr::select(point_id, dom, species) %>%
  left_join(area_df, by = c("point_id")) %>%
  mutate(dom = ifelse(is.na(dom), 0, dom)) 

dom_vals <- dom_dist  %>%
  group_by(point_id, species) %>% 
  summarize(dom_val = mean(dom, na.rm = T),
            forest_area = mean(forest_area, na.rm = T),
            area = mean(area, na.rm = T)) %>% 
  mutate(weighted_dom = (dom_val/100 * area) / forest_area) %>%
  mutate(dom_val_scaled = dom_val / mean(dom_val))


# read in the process predictions
all_lai <- read_csv(paste0(path, "/results/lai_absolute_change_10year.csv"))
all_lai <- all_lai %>% 
  dplyr::select(species, rcp, point_id, diff_lai = diff, net_change_lai = net_change)
  
all_hei <- read_csv(paste0(path, "/results/hei_absolute_change_10year.csv"))
all_hei <- all_hei %>% 
  dplyr::select(species, rcp, point_id, diff_hei = diff, net_change_hei = net_change)


# calculate the number of positive and negative gridcells...
csi_df <- all_lai %>%
  left_join(., all_hei, by = c("species", "rcp", "point_id")) %>%
  left_join(dom_vals, by = c("point_id", "species")) %>% 
  mutate(net_change_lai = net_change_lai * dom_val_scaled,
         net_change_hei = net_change_hei * dom_val_scaled) %>%
  mutate(csi = (net_change_lai + net_change_hei)/2)%>% 
  dplyr::rename("RCP" = rcp) %>% 
  mutate(RCP = ifelse(RCP == "rcp_2_6", "RCP2.6",
                      ifelse(RCP == "rcp_4_5", "RCP4.5", "RCP8.5"))) %>% 
  rowwise() %>% 
  mutate(species = gsub("_", " ", species)) %>% 
  drop_na()


# order the species by CSI
level_order <- csi_df %>% 
  group_by(species) %>% 
  summarise(avg = mean(as.numeric(csi))) %>% 
  arrange(-avg) %>% dplyr::select(species) %>% unique() %>% unlist() %>% as.vector()


# get some numbers
csi_df %>% 
  group_by(species) %>% 
  summarise(avg = mean(as.numeric(csi), na.rm = T)) %>% 
  mutate(sp_group = ifelse(species %in% c("Fagus sylvatica", "Quercus robur", "Betula pendula", "Quercus ilex"), "broadleaved", "coniferous")) %>% 
  group_by(sp_group) %>% 
  summarise(avg = mean(as.numeric(avg), na.rm = T))

  

# calculate how much of the area has negative CSI
csi_df %>% 
  group_by(species) %>% 
  summarise(net_negative = sum(csi < 0, na.rm = TRUE),
            net_positive = sum(csi > 0, na.rm = TRUE),
            sum = n()) %>% 
  mutate(pct_neg = 100/sum*net_negative,
         pct_pos = 100/sum*net_positive)


# get the numbers
csi_df_summarised <- csi_df %>% group_by(species, RCP) %>% 
  summarise(mean_csi = mean(csi, na.rm = T),
            sd_csi = sd(csi, na.rm = T),
            sum = n(),
            se = sd_csi/sqrt(sum)) %>% 
  mutate(upper = mean_csi + 1.96*se,
         lower = mean_csi - 1.96*se)

csi_df_summarised %>% filter(RCP == "RCP8.5")
csi_df_summarised %>% filter(RCP == "RCP4.5")
csi_df_summarised %>% filter(RCP == "RCP2.6")


# do the nice plot
level_order <- csi_df_summarised %>%
  filter(RCP == "RCP8.5") %>% 
  group_by(species) %>%
  summarise(avg = mean(as.numeric(mean_csi))) %>%
  arrange(-avg) %>% dplyr::select(species) %>% unique() %>% unlist() %>% as.vector()


p <- ggplot(csi_df_summarised %>% filter(RCP != "RCP4.5")) +
  geom_hline(yintercept = c(0, 10, -10, -20, 20), linetype = "dashed", alpha = 0.5, color = "grey") + 
  geom_bar(aes(x = factor(species, level = level_order),  y = mean_csi, fill = RCP),
           stat = "identity", position = "dodge") +
  geom_errorbar(aes(x = factor(species, level = level_order),
                    y = mean_csi, ymin = lower, ymax = upper, group = RCP),
                position = position_dodge(width = 0.9),
                width=0.3, colour="black", alpha=0.7, linewidth=0.5) +
  labs(x = "RCP", y = "Values") +
  theme_classic()+
  xlab("Species") + ylab("Change in competitive strength [%]")+
  scale_fill_manual(values = c("RCP8.5" = "#CC3380E6",
                               "RCP4.5" = "#B3801AE6",
                               "RCP2.6" = "#338080E6")) +
  ylim(-25, 10) +
  theme(
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = -45, vjust = 0.65, hjust = 0.25),
    axis.title = element_text(size = 12),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 10)
  )

p

# save
# ggsave(paste0(path, "/svd_dnn_results/figures/Figure1a.png"),
#        p_combined, dpi = 300, width = 7.5, height = 7.5, units = "in")
write_csv(csi_df_summarised, paste0(path_out, "/figure_data/figure1.csv"))

# load the area of the distribution range
size_list <- read_csv(paste0(path, "/gis_data/range_edges/range_size_all_species.csv"))


### all rcps
p <- ggplot(csi_df_summarised) + #%>% filter(RCP != "RCP4.5")) +
  geom_hline(yintercept = c(0, 10, -10, -20, 20), linetype = "dashed", alpha = 0.5, color = "grey") + 
  geom_bar(aes(x = factor(species, level = level_order),  y = mean_csi, fill = RCP),
           stat = "identity", position = "dodge") +
  geom_errorbar(aes(x = factor(species, level = level_order),
                    y = mean_csi, ymin = lower, ymax = upper, group = RCP),
                position = position_dodge(width = 0.9),
                width=0.3, colour="black", alpha=0.7, linewidth=0.5) +
  labs(x = "RCP", y = "Values") +
  theme_classic()+
  xlab("Species") + ylab("Change in competitive strength [%]")+
  scale_fill_manual(values = c("RCP8.5" = "#CC3380E6",
                               "RCP4.5" = "#B3801AE6",
                               "RCP2.6" = "#338080E6")) +
  ylim(-26, 10) +
  theme(
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = -45, vjust = 0.65, hjust = 0.25, face = "italic"),
    axis.title = element_text(size = 12),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 10)
  )

p

# save
ggsave(paste0(path, "/svd_dnn_results/figureS3.png"),
       p, dpi = 300, width = 7.5, height = 7.5, units = "in")
write_csv(csi_df_summarised, paste0(path_out, "/figure_data/figureS3.csv"))



# Add a type column to indicate the plot type
range_df_long <- size_list %>%
  rename("species" = "species_name") %>% 
  mutate(species = gsub("_", " ", species))
write_csv(range_df_long, paste0(path_out, "/figure_data/figure1_ranges.csv"))

combined_df <- left_join(csi_df_summarised, range_df_long, by = c("species"))


# Plot for Range
p_range <- ggplot(range_df_long, aes(x = factor(species, level = level_order), y =  whole_range/100)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3) +
  labs(x = "Species", y =  expression("Range size [km"^2*"]")) +
  scale_fill_manual(values = c("grey")) +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = -45, vjust = 0.65, hjust = 0.25),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 10),
    plot.margin = margin(t = 2, b = 0, l = 10, r = 10)  # Reduce top margin
  )

# Combine plots
library(patchwork)
# Removing the x-axis text from the CSI plot
p_csi <- p +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.title.x = element_blank(),
    plot.margin = margin(t = 2, b = 0, l = 10, r = 10)  # Reduce top margin# Remove x-axis title
  )

# Combine the two plots with patchwork
p_combined <- p_csi / p_range + 
  plot_layout(heights = c(5, 1))  # Adjust height ratios if needed

# Display the combined plot
p_combined

# save
ggsave(paste0(path, "/svd_dnn_results/figures/Figure1.png"),
       p_combined, dpi = 300, width = 7.5, height = 7.5, units = "in")



# same plot with all RCPs for supplement
p <- ggplot(csi_df_summarised) +
  geom_hline(yintercept = c(0, 10, -10, -20, 20), linetype = "dashed", alpha = 0.5, color = "grey") +  # Add dashed horizontal lines
  geom_bar(aes(x = factor(species, level = level_order),  y = mean_csi, fill = RCP), stat = "identity", position = "dodge") +
  geom_errorbar(aes(x = factor(species, level = level_order), y = mean_csi, ymin = lower, ymax = upper, group = RCP), position = position_dodge(width = 0.9),
                width=0.3, colour="black", alpha=0.7, linewidth=0.5) +
  labs(x = "RCP", y = "Values") +
  theme_classic()+
  xlab("Species") + ylab("Change in CSI [%]")+
  scale_fill_manual(values = c("RCP8.5" = "#CC3380E6", "RCP4.5" = "#B3801AE6", "RCP2.6" = "#338080E6")) +
  ylim(-25, 10) +
  theme(
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = -45, vjust = 0.65, hjust = 0.25),
    axis.title = element_text(size = 12),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 10)
  )

p

ggsave(paste0(path, "/svd_dnn_results/figures/FigureS1.png"),
       p, dpi = 300, width = 7.5, height = 7.5, units = "in")
write_csv(csi_df_summarised, paste0(path_out, "/figure_data/figureS1.csv"))




### individual components  -----------------------------------------------------------------

## plot height and LAI change for the supplement

# get numbers
csi_df %>% group_by(species, RCP) %>% 
  summarise(mean_change = mean(net_change_lai, na.rm = T),
            sd_change = sd(net_change_lai, na.rm = T),
            sum = n()) %>% filter(RCP == "RCP8.5")


csi_df_summarised <- csi_df %>% group_by(species, RCP) %>% 
  summarise(mean = mean(net_change_lai, na.rm = T),
            sd = sd(net_change_lai, na.rm = T),
            sum = n(),
            se = sd/sqrt(sum)) %>% 
  mutate(upper = mean + 1.96*se,
         lower = mean - 1.96*se)

csi_df_summarised %>% filter(RCP == "RCP8.5")
csi_df_summarised %>% filter(RCP == "RCP4.5")
csi_df_summarised %>% filter(RCP == "RCP2.6")


# bar plot
level_order <- csi_df_summarised %>%
  filter(RCP == "RCP8.5") %>% 
  group_by(species) %>%
  summarise(avg = mean(as.numeric(mean))) %>%
  arrange(-avg) %>% dplyr::select(species) %>% unique() %>% unlist() %>% as.vector()


p <- ggplot(csi_df_summarised) +
  geom_hline(yintercept = c(0, 10, -10, -20, 20, -40, -60), linetype = "dashed", alpha = 0.5, color = "grey") +  # Add dashed horizontal lines
  geom_bar(aes(x = factor(species, level = level_order),  y = mean, fill = RCP), stat = "identity", position = "dodge") +
  geom_errorbar(aes(x = factor(species, level = level_order), y = mean, ymin = lower, ymax = upper, group = RCP), position = position_dodge(width = 0.9),
                width=0.3, colour="black", alpha=0.7, linewidth=0.5) +
  labs(x = "RCP", y = "Values") +
  theme_classic()+
  xlab("Species") + ylab("Change in LAI [%]")+
  scale_fill_manual(values = c("RCP8.5" = "#CC3380E6", "RCP4.5" = "#B3801AE6", "RCP2.6" = "#338080E6")) +
  ylim(-60, 10) +
  theme(
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = -45, vjust = 0.65, hjust = 0.25),
    axis.title = element_text(size = 12),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 10)
  )

p

ggsave(paste0(path, "/results/figureS1.png"),
       p, dpi = 300, width = 7.5, height = 7.5, units = "in")



### same for height growth ---

#numbers
csi_df %>% group_by(species, RCP) %>% 
  summarise(mean_change = mean(net_change_hei, na.rm = T),
            sd_change = sd(net_change_hei, na.rm = T),
            sum = n()) %>% filter(RCP == "RCP8.5")


# get the numbers
csi_df_summarised <- csi_df %>% group_by(species, RCP) %>% 
  summarise(mean = mean(net_change_hei, na.rm = T),
            sd = sd(net_change_hei, na.rm = T),
            sum = n(),
            se = sd/sqrt(sum)) %>% 
  mutate(upper = mean + 1.96*se,
         lower = mean - 1.96*se)

csi_df_summarised %>% filter(RCP == "RCP8.5")
csi_df_summarised %>% filter(RCP == "RCP4.5")
csi_df_summarised %>% filter(RCP == "RCP2.6")


## bar plot
level_order <- csi_df_summarised %>%
  filter(RCP == "RCP8.5") %>% 
  group_by(species) %>%
  summarise(avg = mean(as.numeric(mean))) %>%
  arrange(-avg) %>% dplyr::select(species) %>% unique() %>% unlist() %>% as.vector()


p <- ggplot(csi_df_summarised) +
  geom_hline(yintercept = c(0, 10, -10, -20, 20), linetype = "dashed", alpha = 0.5, color = "grey") +  # Add dashed horizontal lines
  geom_bar(aes(x = factor(species, level = level_order),  y = mean, fill = RCP), stat = "identity", position = "dodge") +
  geom_errorbar(aes(x = factor(species, level = level_order), y = mean, ymin = lower, ymax = upper, group = RCP), position = position_dodge(width = 0.9),
                width=0.3, colour="black", alpha=0.7, linewidth=0.5) +
  labs(x = "RCP", y = "Values") +
  theme_classic()+
  xlab("Species") + ylab("Change in height growth [%]")+
  scale_fill_manual(values = c("RCP8.5" = "#CC3380E6", "RCP4.5" = "#B3801AE6", "RCP2.6" = "#338080E6")) +
  ylim(-10, 20) +
  theme(
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = -45, vjust = 0.65, hjust = 0.25),
    axis.title = element_text(size = 12),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 10)
  )

p

ggsave(paste0(path, "/figures/figureS2.png"),
       p, dpi = 300, width = 7.5, height = 7.5, units = "in")
write_csv(csi_df_summarised, paste0(path_out, "/figure_data/figureS2.csv"))


### per biomes ------------------------------------------------------------------

# biomes
biomes_lookup <- read_csv(paste0(path, "/gis/ecoregions/point_id_lookup.csv"))


area_df <- ref_grid %>% dplyr::rename("point_id" = "reference_grid") %>% 
  left_join(., forest_cover %>%
              dplyr::select(point_id, forest_cover), by = c("point_id")) %>% 
  left_join(pixel_area_df, by = c("x", "y")) %>% 
  mutate(forest_area = forest_cover * area)


# load dominance cells
dom_dist <- read_csv(paste0(path, "/svd_dnn/range_edges/all_sp_dom_vals_v2.csv"))
dom_dist <- dom_dist %>% dplyr::select(point_id, dom, species) %>%
  left_join(area_df, by = c("point_id")) %>%
  mutate(dom = ifelse(is.na(dom), 0, dom)) 

dom_vals <- dom_dist  %>%
  group_by(point_id, species) %>% 
  summarize(dom_val = mean(dom, na.rm = T),
            forest_area = mean(forest_area, na.rm = T),
            area = mean(area, na.rm = T)) %>% 
  mutate(weighted_dom = (dom_val/100 * area) / forest_area) %>%
  mutate(dom_val_scaled = dom_val / mean(dom_val))



# calculate the number of positive and negative gridcells...
csi_df <- all_lai %>%
  left_join(., all_hei, by = c("species", "rcp", "point_id")) %>%
  left_join(dom_vals, by = c("point_id", "species")) %>% 
  mutate(net_change_lai = net_change_lai * dom_val_scaled,
         net_change_hei = net_change_hei * dom_val_scaled) %>%
  mutate(csi = (net_change_lai + net_change_hei)/2)%>% 
  dplyr::rename("RCP" = rcp) %>% 
  mutate(RCP = ifelse(RCP == "rcp_2_6", "RCP2.6",
                      ifelse(RCP == "rcp_4_5", "RCP4.5", "RCP8.5"))) %>% 
  rowwise() %>% 
  mutate(species = gsub("_", " ", species)) %>% 
  drop_na() %>% 
  left_join(., biomes_lookup, by = "point_id")


# get the numbers
csi_df_summarised <- csi_df %>% group_by(species, RCP, biome) %>% 
  summarise(mean_csi = mean(csi, na.rm = T),
            sd_csi = sd(csi, na.rm = T),
            sum = n(),
            se = sd_csi/sqrt(sum)) %>% 
  mutate(upper = mean_csi + 1.96*se,
         lower = mean_csi - 1.96*se)
csi_df_summarised <- csi_df_summarised %>% mutate(biome = ifelse(biome == "Temperate Broadleaf", "Temperate Broadleaved", biome))

# bar plot
level_order <- csi_df_summarised %>%
  filter(RCP == "RCP8.5") %>% 
  group_by(species) %>%
  summarise(avg = mean(as.numeric(mean_csi))) %>%
  arrange(-avg) %>% dplyr::select(species) %>% unique() %>% unlist() %>% as.vector()


# Define the desired order of biomes
biome_order <- c("Mediterranean", "Temperate Broadleaved",
                 "Temperate Grasslands", "Temperate Coniferous",
                 "Boreal Forests", "Tundra")

# Convert the biome column to a factor with the specified order
csi_df_summarised <- csi_df_summarised %>% filter(biome != "Temperate Grasslands") %>%
  mutate(n_total = sum(sum),
         share = sum/n_total * 100) %>%
  # filter(sum > n_total/20) %>%  # at least 5% need to occur per biome
  ungroup()

csi_df_summarised$biome <- factor(csi_df_summarised$biome, levels = biome_order)


p <- ggplot(csi_df_summarised) +
  geom_hline(yintercept = c(0, -20, 20, -40, 40), linetype = "dashed", alpha = 0.5, color = "grey") +  # Add dashed horizontal lines
  geom_bar(aes(x = factor(species, level = level_order),  y = mean_csi, fill = RCP), stat = "identity", position = "dodge") +
  geom_errorbar(aes(x = factor(species, level = level_order), y = mean_csi, ymin = lower, ymax = upper, group = RCP), position = position_dodge(width = 0.9),
                width=0.3, colour="black", alpha=0.7, linewidth=0.5) +
  labs(x = "RCP", y = "Values") +
  theme_classic()+
  xlab("Species") + ylab("relative change in CSI [%]")+
  scale_fill_manual(values = c("RCP8.5" = "#CC3380E6", "RCP4.5" = "#B3801AE6", "RCP2.6" = "#338080E6")) +
  ylim(-40, 40) +
  theme(
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = -90, vjust = 0.65, hjust = 0.25, face = "italic"),
    axis.title = element_text(size = 12),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 10)
  ) + facet_wrap(~biome)

p

ggsave(paste0(path, "/figures/FigureS4"),
       p, dpi = 300, width = 7.5, height = 7.5, units = "in")

write_csv(csi_df_summarised, paste0(path_out, "/figure_data/figureS4.csv"))


# get the numbers
csi_df_summarised <- csi_df %>% group_by(species, RCP, biome) %>% 
  summarise(mean_csi = mean(csi, na.rm = T),
            sd_csi = sd(csi, na.rm = T),
            sum = n(),
            se = sd_csi/sqrt(sum)) %>% 
  mutate(upper = mean_csi + 1.96*se,
         lower = mean_csi - 1.96*se) %>% 
  mutate(n_total = sum(sum)) %>%
  filter(sum > n_total/20)

csi_df_summarised %>% filter(RCP == "RCP8.5") %>% View()
csi_df_summarised %>% filter(RCP == "RCP4.5")
csi_df_summarised %>% filter(RCP == "RCP2.6")


###  individual components -------------------------
csi_df <- all_lai %>%
  dplyr::rename("RCP" = rcp) %>% 
  mutate(RCP = ifelse(RCP == "rcp_2_6", "RCP2.6",
                      ifelse(RCP == "rcp_4_5", "RCP4.5", "RCP8.5"))) %>% 
  mutate(species = gsub("_", " ", species)) %>% 
  left_join(., biomes_lookup, by = "point_id")

csi_df_summarised <- csi_df %>% group_by(species, RCP, biome) %>% 
  summarise(mean_csi = mean(net_change_lai, na.rm = T),
            sd_csi = sd(net_change_lai, na.rm = T),
            sum = n(),
            se = sd_csi/sqrt(sum)) %>% 
  mutate(upper = mean_csi + 1.96*se,
         lower = mean_csi - 1.96*se) %>% 
  mutate(n_total = sum(sum)) %>%
  filter(sum > n_total/20)

csi_df_summarised %>% filter(RCP == "RCP8.5") %>% View()
csi_df_summarised %>% filter(RCP == "RCP4.5")
csi_df_summarised %>% filter(RCP == "RCP2.6")


csi_df <- all_hei %>%
  dplyr::rename("RCP" = rcp) %>% 
  mutate(RCP = ifelse(RCP == "rcp_2_6", "RCP2.6",
                      ifelse(RCP == "rcp_4_5", "RCP4.5", "RCP8.5"))) %>% 
  mutate(species = gsub("_", " ", species)) %>% 
  left_join(., biomes_lookup, by = "point_id")

csi_df_summarised <- csi_df %>% group_by(species, RCP, biome) %>% 
  summarise(mean_csi = mean(net_change_hei, na.rm = T),
            sd_csi = sd(net_change_hei, na.rm = T),
            sum = n(),
            se = sd_csi/sqrt(sum)) %>% 
  mutate(upper = mean_csi + 1.96*se,
         lower = mean_csi - 1.96*se) %>% 
  mutate(n_total = sum(sum)) %>%
  filter(sum > n_total/20)

csi_df_summarised %>% filter(RCP == "RCP8.5") %>% View()
csi_df_summarised %>% filter(RCP == "RCP4.5")
csi_df_summarised %>% filter(RCP == "RCP2.6")


#### end

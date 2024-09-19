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
  mutate(csi = (net_change_lai + net_change_hei)/2)%>% 
  dplyr::rename("RCP" = rcp) %>% 
  mutate(RCP = ifelse(RCP == "rcp_2_6", "RCP2.6",
                      ifelse(RCP == "rcp_4_5", "RCP4.5", "RCP8.5"))) %>% 
  rowwise() %>% 
  mutate(species = gsub("_", " ", species))


# order the species by CSI
level_order <- csi_df %>% 
  group_by(species) %>% 
  summarise(avg = mean(as.numeric(csi))) %>% 
  arrange(-avg) %>% dplyr::select(species) %>% unique() %>% unlist() %>% as.vector()


# get some numbers
csi_df %>% 
  group_by(species) %>% 
  summarise(avg = mean(as.numeric(csi))) %>% 
  mutate(sp_group = ifelse(species %in% c("Fagus sylvatica", "Quercus robur",
                                          "Betula pendula", "Quercus ilex"),
                           "broadleaved", "coniferous")) %>% 
  group_by(sp_group) %>% 
  summarise(avg = mean(as.numeric(avg)))
  

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
ggsave(paste0(path, "/results/figures/CSI_barplot_vfinal.png"),
       p, dpi = 300, width = 7.5, height = 7.5, units = "in")


# load the area of the distribution range
size_list <- read_csv(paste0(path, "/gis_data/range_edges/range_size_all_species.csv"))


# Add a type column to indicate the plot type
range_df_long <- size_list %>%
  rename("species" = "species_name") %>% 
  mutate(species = gsub("_", " ", species))

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
ggsave(paste0(path, "/results/CSI_barplot_vfinal_rangesize.png"),
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

ggsave(paste0(path, "/results/CSI_barplot_vfinal_allRCPs.png"),
       p, dpi = 300, width = 7.5, height = 7.5, units = "in")




### individual components  -----------------------------------------------------------------

## plot height and LAI change for the supplement

# get numbers
all_lai %>% group_by(species, rcp) %>% 
  summarise(mean_change = mean(net_change_lai, na.rm = T),
            sd_change = sd(net_change_lai, na.rm = T),
            sum = n()) %>% filter(rcp == "rcp_8_5")


# calculate the number of positive and negative gridcells...
csi_df <- all_lai %>%
  dplyr::rename("RCP" = rcp) %>% 
  mutate(RCP = ifelse(RCP == "rcp_2_6", "RCP2.6",
                      ifelse(RCP == "rcp_4_5", "RCP4.5", "RCP8.5"))) %>% 
  mutate(species = gsub("_", " ", species))


## get the numbers
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

ggsave(paste0(path, "/results/LAI_barplot_vfinal.png"),
       p, dpi = 300, width = 7.5, height = 7.5, units = "in")



### same for height growth ---

#numbers
all_hei %>% group_by(species, rcp) %>% 
  summarise(mean_change = mean(net_change_hei, na.rm = T),
            sd_change = sd(net_change_hei, na.rm = T),
            sum = n()) %>% filter(rcp == "rcp_8_5")

# calculate the number of positive and negative gridcells...
csi_df <- all_hei %>%
  dplyr::rename("RCP" = rcp) %>% 
  mutate(RCP = ifelse(RCP == "rcp_2_6", "RCP2.6",
                      ifelse(RCP == "rcp_4_5", "RCP4.5", "RCP8.5"))) %>% 
  mutate(species = gsub("_", " ", species))


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

ggsave(paste0(path, "/results/HEI_barplot_vfinal.png"),
       p, dpi = 300, width = 7.5, height = 7.5, units = "in")


#### end

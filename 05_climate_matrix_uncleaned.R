##################################################################
### 5. climate matrix analysis -----------------------------------
##################################################################


### load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(sf)
library(data.table)


path <- "/data/public/"

### load data ---
all_out_lai <- read_csv(paste0(path, "/results/lai_absolute_change_v46_10year.csv"))
all_out_hei <- read_csv(paste0(path, "/results/hei_absolute_change_v46_10year.csv"))


# define species
species  <- c("Fagus_sylvatica", "Pinus_sylvestris", "Abies_alba", "Picea_abies", 
              "Larix_decidua", "Betula_pendula", "Pinus_halepensis", "Quercus_robur",
              "Quercus_ilex")


# get the MAT and ANP for all species
all_clims <- list()
for(s in 1:length(species)){
  dat <- read.table(paste0(path, "/gis_data/range_edges/range_edges_df_", species[s],"_10pct_new.csv"))
  dat <- dat %>% mutate(species  = species[s])
  all_clims[[s]] <- dat
}

# calculate mean for each edge
all_clims <- do.call(rbind, all_clims)
all_clims <- all_clims %>% group_by(point_id, edge, species) %>% 
  summarise(MAT_avg = mean(MAT_avg, na.rm = T),
            ANP_avg = mean(ANP_avg, na.rm = T))

# check
head(all_clims)


# calculate CSI from LAI and height
csi_df <- all_out_lai %>% left_join(., all_out_hei, by = c("species", "rcp", "point_id")) %>% 
  mutate(csi = (net_change.x + net_change.y)/2) %>% 
  dplyr::select(species, rcp, point_id, csi) %>% 
  left_join(., all_clims, by = c("species", "point_id"))

# check
head(csi_df)


# group into broadleaved and conifers
data <- csi_df %>%
  drop_na() %>% 
  mutate(sp_group = ifelse(species %in% c("Fagus_sylvatica", "Quercus_robur", "Betula_pendula", "Quercus_ilex"), "broadleaved", "coniferous")) %>% 
  group_by(edge, sp_group, rcp) %>% 
  summarize(mean_csi = mean(csi),
            sd_csi = sd(csi),
            sum = n(),
            se = sd_csi/sqrt(sum)) %>% 
  mutate(upper = mean_csi + 1.96*se,
         lower = mean_csi - 1.96*se) 


# Calculate the difference to center for each edge group and sp_group
center_data <- data[data$edge == "center", ]

other_data <- data %>% filter(edge != "center") %>%
  mutate(x = ifelse(grepl("warm", edge), "warm", "cold"),
         y = ifelse(grepl("dry", edge), "dry", "wet"))

data <- left_join(other_data, center_data[, c("sp_group", "mean_csi", "rcp")], by = c("sp_group", "rcp"))
data$diff_to_center <- data$mean_csi.x - data$mean_csi.y

data_wide <- data %>% dplyr::select(edge, sp_group, rcp, x, y, diff_to_center) 


# Create the plot with adjusted zlim and title size
plot <- ggplot(data_wide %>% 
                 filter(rcp == "rcp_8_5"), aes(x = x, y = y, fill = diff_to_center)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = brewer.pal(11, "RdBu"),
                       limits = c(-max(abs(data_wide$diff_to_center)), max(abs(data_wide$diff_to_center)))) +
  labs(x = NULL, y = NULL, fill = "[%]") +
  theme_minimal() + 
  theme(
    axis.text = element_text(size = 18), 
    legend.text = element_text(size = 18), 
    legend.title = element_text(size = 20),
    legend.key.size = unit(2, "lines"),
    plot.title = element_text(size = 24, vjust = 2),
    strip.text = element_text(size = 22)
  ) +
  facet_grid(~sp_group) 

plot


data <- csi_df %>% 
  drop_na() %>% 
  mutate(sp_group = ifelse(species %in% c("Fagus_sylvatica", "Quercus_robur", "Betula_pendula", "Quercus_ilex"), "Broadleaved", "Coniferous")) %>% 
  distinct() %>% 
  group_by(species, sp_group, rcp, edge) %>% 
  summarize(mean_csi = mean(csi, na.rm = T),
            sd_csi = sd(csi, na.rm = T),
            sum = n(),
            se = sd_csi/sqrt(sum)) %>% 
  mutate(upper = mean_csi + 1.96*se,
         lower = mean_csi - 1.96*se)


# data <- data %>%
#   group_by(sp_group, rcp, edge) %>%
#   summarize(mean_csi_group = mean(mean_csi, na.rm  = T),
#             sd_group = sd(mean_csi))


data <- data %>%
  group_by(sp_group, rcp, edge) %>%
  summarize(
    mean_csi_group = mean(mean_csi, na.rm = TRUE),
    sd_group = sqrt(sum((sum - 1) * sd_csi^2, na.rm = TRUE) / (sum(sum, na.rm = TRUE) - 1)), # Pooled SD
    total_sum = sum(sum)  # Total number of observations in each group
  ) %>%
  mutate(
    se_group = sd_group / sqrt(total_sum),
    upper = mean_csi_group + 1.96 * se_group,
    lower = mean_csi_group - 1.96 * se_group
  ) %>%
  ungroup()


# 
# data <- csi_df %>% 
#   drop_na() %>% 
#   mutate(sp_group = ifelse(species %in% c("Fagus_sylvatica", "Quercus_robur", "Betula_pendula", "Quercus_ilex"), "Broadleaved", "Coniferous")) %>% 
#   distinct() %>% 
#   group_by(sp_group, rcp, edge) %>% 
#   summarize(mean_csi = mean(csi, na.rm = T),
#             sd_csi = sd(csi, na.rm = T),
#             sum = n(),
#             se = sd_csi/sqrt(sum)) %>% 
#   mutate(upper = mean_csi + 1.96*se,
#          lower = mean_csi - 1.96*se)
# 


rcp26 <- data %>% group_by(sp_group, rcp, edge) %>% 
  filter(rcp == "rcp_2_6")

rcp45 <- data %>% group_by(sp_group, rcp, edge) %>%
  filter(rcp == "rcp_4_5")

rcp85 <- data %>% group_by(sp_group, rcp, edge) %>%
  filter(rcp == "rcp_8_5")


dat_tab <- rcp26 %>%
  left_join(rcp45, by = c("sp_group", "edge")) %>% 
  left_join(rcp85, by = c("sp_group", "edge")) %>% 
  mutate(edge = gsub("_", "-", edge)) %>% 
  ungroup()



# Load necessary libraries
library(dplyr)
library(gt)


# Pivot the data to the desired format
result <- dat_tab %>%
  dplyr::select(sp_group, edge, mean_csi_group.x) %>%
  spread(key = sp_group, value = mean_csi_group.x) %>%
  rename(RCP2.6_Broadleaved = Broadleaved, RCP2.6_Coniferous = Coniferous) %>%
  left_join(dat_tab %>%
              dplyr::select(sp_group, edge, mean_csi_group.y) %>%
              spread(key = sp_group, value = mean_csi_group.y) %>%
              rename(RCP4.5_Broadleaved = Broadleaved, RCP4.5_Coniferous = Coniferous),
            by = "edge") %>%
  left_join(dat_tab %>%
              dplyr::select(sp_group, edge, mean_csi_group) %>%
              spread(key = sp_group, value = mean_csi_group) %>%
              rename(RCP8.5_Broadleaved = Broadleaved, RCP8.5_Coniferous = Coniferous),
            by = "edge") %>%
  mutate(across(ends_with("Broadleaved"), ~round(., 1)),
         across(ends_with("Coniferous"), ~round(., 1)))

result_se <- dat_tab %>%
  dplyr::select(sp_group, edge, se_group.x) %>%
  spread(key = sp_group, value = se_group.x) %>%
  rename(RCP2.6_Broadleaved_SE = Broadleaved, RCP2.6_Coniferous_SE = Coniferous) %>%
  left_join(dat_tab %>%
              dplyr::select(sp_group, edge, se_group.y) %>%
              spread(key = sp_group, value = se_group.y) %>%
              rename(RCP4.5_Broadleaved_SE = Broadleaved, RCP4.5_Coniferous_SE = Coniferous),
            by = "edge") %>%
  left_join(dat_tab %>%
              dplyr::select(sp_group, edge, se_group) %>%
              spread(key = sp_group, value = se_group) %>%
              rename(RCP8.5_Broadleaved_SE = Broadleaved, RCP8.5_Coniferous_SE = Coniferous),
            by = "edge") %>%
  mutate(across(ends_with("Broadleaved"), ~round(., 1)),
         across(ends_with("Coniferous"), ~round(., 1)))

result <- left_join(result, result_se, by = "edge") %>% 
  dplyr::select(edge, RCP2.6_Broadleaved, RCP2.6_Broadleaved_SE, RCP2.6_Coniferous, RCP2.6_Coniferous_SE,
                RCP4.5_Broadleaved, RCP4.5_Broadleaved_SE, RCP4.5_Coniferous, RCP4.5_Coniferous_SE,
                RCP8.5_Broadleaved, RCP8.5_Broadleaved_SE, RCP8.5_Coniferous, RCP8.5_Coniferous_SE)






result <- result %>%
  mutate(
    RCP2.6_Broadleaved = paste0(sprintf("%.1f", RCP2.6_Broadleaved), " ±", sprintf("%.1f", RCP2.6_Broadleaved_SE)),
    RCP2.6_Coniferous = paste0(sprintf("%.1f", RCP2.6_Coniferous), " ±", sprintf("%.1f", RCP2.6_Coniferous_SE)),
    RCP4.5_Broadleaved = paste0(sprintf("%.1f", RCP4.5_Broadleaved), " ±", sprintf("%.1f", RCP4.5_Broadleaved_SE)),
    RCP4.5_Coniferous = paste0(sprintf("%.1f", RCP4.5_Coniferous), " ±", sprintf("%.1f", RCP4.5_Coniferous_SE)),
    RCP8.5_Broadleaved = paste0(sprintf("%.1f", RCP8.5_Broadleaved), " ±", sprintf("%.1f", RCP8.5_Broadleaved_SE)),
    RCP8.5_Coniferous = paste0(sprintf("%.1f", RCP8.5_Coniferous), " ±", sprintf("%.1f", RCP8.5_Coniferous_SE))
  ) %>% 
  dplyr::select(edge, RCP2.6_Broadleaved, RCP2.6_Coniferous, RCP4.5_Broadleaved, RCP4.5_Coniferous, RCP8.5_Broadleaved, RCP8.5_Coniferous) %>% 
  mutate(edge = ifelse(edge == "center", "Core",
                       ifelse(edge == "cold-dry", "Cold-dry edge",
                              ifelse(edge == "cold-wet", "Cold-wet edge",
                                     ifelse(edge == "warm-dry", "Warm-dry edge",
                                            "Warm-wet edge")))))


library(gt)
library(dplyr)

# Create the gt table
table <- result %>%
  gt() %>%
  cols_label(
    edge = "Niche position",
    RCP8.5_Broadleaved = "Broadleaved",
    RCP8.5_Coniferous = "Coniferous",
    RCP2.6_Broadleaved = "Broadleaved",
    RCP2.6_Coniferous = "Coniferous"
  ) %>%
  tab_spanner(
    label = "RCP2.6",
    columns = vars(RCP2.6_Broadleaved, RCP2.6_Coniferous)
  ) %>%
  tab_spanner(
    label = "RCP8.5",
    columns = vars(RCP8.5_Broadleaved, RCP8.5_Coniferous)
  ) %>%
  # Apply bold text style only to negative values
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(RCP8.5_Broadleaved),
      rows = result$RCP8.5_Broadleaved < 0
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(RCP8.5_Coniferous),
      rows = result$RCP8.5_Coniferous < 0
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(RCP4.5_Broadleaved),
      rows = result$RCP4.5_Broadleaved < 0
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(RCP4.5_Coniferous),
      rows = result$RCP4.5_Coniferous < 0
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(RCP2.6_Broadleaved),
      rows = result$RCP2.6_Broadleaved < 0
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(RCP2.6_Coniferous),
      rows = result$RCP2.6_Coniferous < 0
    )
  ) %>%
  # Add a thicker border line after the row labeled "center"
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "lightgrey",
      weight = px(3)  # Adjust thickness as needed
    ),
    locations = cells_body(
      rows = c(which(result$edge == "center"),
               which(result$edge == "center") + 1)# Apply the border to the row after "center"
    )
  )

# Print the table
table

# Print the table
table




# without RCP45


# Pivot the data to the desired format
result <- dat_tab %>%
  dplyr::select(sp_group, edge, mean_csi_group.x) %>%
  spread(key = sp_group, value = mean_csi_group.x) %>%
  rename(RCP2.6_Broadleaved = Broadleaved, RCP2.6_Coniferous = Coniferous) %>%
  left_join(dat_tab %>%
              dplyr::select(sp_group, edge, mean_csi_group) %>%
              spread(key = sp_group, value = mean_csi_group) %>%
              rename(RCP8.5_Broadleaved = Broadleaved, RCP8.5_Coniferous = Coniferous),
            by = "edge") %>%
  mutate(across(ends_with("Broadleaved"), ~round(., 1)),
         across(ends_with("Coniferous"), ~round(., 1)))

result_se <- dat_tab %>%
  dplyr::select(sp_group, edge, se_group.x) %>%
  spread(key = sp_group, value = se_group.x) %>%
  rename(RCP2.6_Broadleaved_SE = Broadleaved, RCP2.6_Coniferous_SE = Coniferous) %>%
  left_join(dat_tab %>%
              dplyr::select(sp_group, edge, se_group) %>%
              spread(key = sp_group, value = se_group) %>%
              rename(RCP8.5_Broadleaved_SE = Broadleaved, RCP8.5_Coniferous_SE = Coniferous),
            by = "edge") %>%
  mutate(across(ends_with("Broadleaved"), ~round(., 1)),
         across(ends_with("Coniferous"), ~round(., 1)))

result <- left_join(result, result_se, by = "edge") %>% 
  dplyr::select(edge, RCP2.6_Broadleaved, RCP2.6_Broadleaved_SE, RCP2.6_Coniferous, RCP2.6_Coniferous_SE,
                RCP8.5_Broadleaved, RCP8.5_Broadleaved_SE, RCP8.5_Coniferous, RCP8.5_Coniferous_SE)
# Combine mean values with SEs in the result tibble
result <- result %>%
  mutate(
    RCP2.6_Broadleaved = paste0(sprintf("%.1f", RCP2.6_Broadleaved), " ±", sprintf("%.1f", RCP2.6_Broadleaved_SE)),
    RCP2.6_Coniferous = paste0(sprintf("%.1f", RCP2.6_Coniferous), " ±", sprintf("%.1f", RCP2.6_Coniferous_SE)),
    RCP8.5_Broadleaved = paste0(sprintf("%.1f", RCP8.5_Broadleaved), " ±", sprintf("%.1f", RCP8.5_Broadleaved_SE)),
    RCP8.5_Coniferous = paste0(sprintf("%.1f", RCP8.5_Coniferous), " ±", sprintf("%.1f", RCP8.5_Coniferous_SE))
  ) %>% 
  dplyr::select(edge, RCP2.6_Broadleaved, RCP2.6_Coniferous, RCP8.5_Broadleaved, RCP8.5_Coniferous) %>% 
  mutate(edge = ifelse(edge == "center", "Core",
                       ifelse(edge == "cold-dry", "Cold-dry edge",
                              ifelse(edge == "cold-wet", "Cold-wet edge",
                                     ifelse(edge == "warm-dry", "Warm-dry edge",
                                            "Warm-wet edge")))))

library(gt)
library(dplyr)

# Create the gt table
table <- result %>%
  gt() %>%
  cols_label(
    edge = "Niche position",
    RCP8.5_Broadleaved = "Broadleaved",
    RCP8.5_Coniferous = "Coniferous",
    RCP2.6_Broadleaved = "Broadleaved",
    RCP2.6_Coniferous = "Coniferous"
  ) %>%
  tab_spanner(
    label = "RCP2.6",
    columns = vars(RCP2.6_Broadleaved, RCP2.6_Coniferous)
  ) %>%
  tab_spanner(
    label = "RCP8.5",
    columns = vars(RCP8.5_Broadleaved, RCP8.5_Coniferous)
  ) %>%
  # Apply bold text style only to negative values
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(RCP8.5_Broadleaved),
      rows = result$RCP8.5_Broadleaved < 0
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(RCP8.5_Coniferous),
      rows = result$RCP8.5_Coniferous < 0
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(RCP2.6_Broadleaved),
      rows = result$RCP2.6_Broadleaved < 0
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(RCP2.6_Coniferous),
      rows = result$RCP2.6_Coniferous < 0
    )
  ) %>%
  # Add a thicker border line after the row labeled "center"
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "lightgrey",
      weight = px(3)  # Adjust thickness as needed
    ),
    locations = cells_body(
      rows = c(which(result$edge == "center"),
               which(result$edge == "center") + 1)# Apply the border to the row after "center"
    )
  )

# Print the table
table





# Create the boxplot using ggplot2
ggplot(data %>% filter(rcp == "rcp_8_5"), aes(x = factor(sp_group), y = csi, fill = edge, groups = edge)) +
  geom_hline(yintercept = c(0, -20, 20, 40), linetype = "dashed", alpha = 0.5, color = "grey") +  # Add dashed horizontal lines
  geom_boxplot(outlier.shape = NA)+
  theme_classic()+
  xlab("") + ylab("Difference to center niche [%]")+
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 12),     
    axis.title.x = element_blank())










# Calculate the difference to center for each edge group and sp_group
center_data <- data[data$edge == "center", ]  # Filter only the "center" group rows
other_data <- data %>%  filter(edge != "center") 

data <- left_join(other_data, center_data[, c("species", "sp_group", "csi", "rcp")], by = c("species", "sp_group", "rcp"))
data$diff_to_center <- data$csi.x - data$csi.y

data_wide <- data %>% dplyr::select(edge, species, sp_group, rcp, diff_to_center) 
View(data_wide)


# Create the boxplot using ggplot2
ggplot(data %>% filter(rcp == "rcp_2_6"), aes(x = factor(sp_group), y = diff_to_center, fill = edge, groups = edge)) +
  geom_hline(yintercept = c(0, -20, 20, 40), linetype = "dashed", alpha = 0.5, color = "grey") +  # Add dashed horizontal lines
  geom_boxplot(outlier.shape = NA)+
  theme_classic()+
  xlab("") + ylab("Difference to center niche [%]")+
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 12),     
    axis.title.x = element_blank())



### heatmap ---

# Calculate breaks for bins based on integer rounding
mat_diff_breaks <- seq(floor(min(csi_df$mat_diff_rcp85, na.rm = TRUE)), ceiling(max(csi_df$mat_diff_rcp85, na.rm = TRUE)), by = 0.5)

# Calculate breaks for ANP_diff (bins with intervals of 50 units)
anp_diff_min <- floor(min(csi_df$anp_diff_rcp85, na.rm = TRUE)) 
anp_diff_max <- ceiling(max(csi_df$anp_diff_rcp85, na.rm = TRUE)) 
anp_diff_breaks <- seq(anp_diff_min, anp_diff_max, by = 0.5)

# Bin MAT_diff and ANP_diff using case_when
# Assign MAT_diff_bin and ANP_diff_bin directly from breaks
df_plot_fin <- csi_df %>%
  mutate(
    MAT_diff_bin = as.factor(mat_diff_breaks[findInterval(mat_diff_rcp85, mat_diff_breaks)]),
    ANP_diff_bin = as.factor(anp_diff_breaks[findInterval(anp_diff_rcp85, anp_diff_breaks)])
  )


# Calculate mean dist_rate for each combination of MAT_diff_bin and ANP_diff_bin
df_plot_heatmap <- df_plot_fin %>%
  filter(rcp == "rcp_8_5") %>% 
  drop_na() %>% 
  group_by(MAT_diff_bin, ANP_diff_bin) %>%
  mutate(dist_rate = mean(csi, na.rm = TRUE)) %>%
  ungroup() %>%
  unite("combined", MAT_diff_bin, ANP_diff_bin, remove = FALSE) %>%
  mutate(unique_id = as.integer(as.factor(combined))) %>%
  group_by(unique_id) %>% 
  mutate(n = n()) %>% filter(n > 10)



# Create the base heatmap with scatter plot
df_plot_heatmap_sub <- df_plot_heatmap %>%
  mutate(sp_group = ifelse(species %in% c("Fagus_sylvatica", "Quercus_robur", "Betula_pendula", "Quercus_ilex"), "broadleaved", "coniferous")) #%>% 
# filter(sp_group == "broadleaved")


base_plot <- ggplot() +
  geom_tile(data = df_plot_heatmap_sub, aes(x = MAT_diff_bin, y = ANP_diff_bin, fill = dist_rate)) +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(x = "Δ Temperature",
       y = "Δ Precipitation",
       fill = "disturbance rate") +
  theme_classic() +
  theme(legend.position = "none")  # Remove legend for heatmap

base_plot

























species_colors <- rev(RColorBrewer::brewer.pal(9, "Set1"))#rainbow(length(unique(data$species)))
ggplot(data = csi_df %>% drop_na()) +
  # geom_point(aes(x = pca_value, y = mean_change, group = species, col = species, alpha = 0)) +
  geom_smooth(aes(x = MAT_avg, y = csi, group = species, col = species), method = "lm", se = FALSE) +
  scale_color_manual(values = species_colors) +
  ylab("delta CSI [%]") + xlab("MAT")


ggplot(data = csi_df %>% drop_na()) +
  # geom_point(aes(x = pca_value, y = mean_change, group = species, col = species, alpha = 0)) +
  geom_smooth(aes(x = ANP_avg, y = csi, group = species, col = species), method = "lm", se = FALSE) +
  scale_color_manual(values = species_colors) +
  ylab("delta CSI [%]") + xlab("ANP")




species_colors <- c("green", "darkgreen")
ggplot(data = csi_df %>% drop_na() %>% 
         mutate(sp_group = ifelse(species %in% c("Fagus_sylvatica", "Quercus_robur", "Betula_pendula", "Quercus_ilex"), "broadleaved", "coniferous"))) +
  # geom_point(aes(x = pca_value, y = mean_change, group = species, col = species, alpha = 0)) +
  geom_smooth(aes(x = MAT_avg, y = csi, group = sp_group, col = sp_group), method = "lm", se = TRUE) +
  scale_color_manual(values = species_colors) +
  ylab("delta CSI [%]") + xlab("cold-wet to warm-dry gradient")

ggplot(data = csi_df %>% drop_na() %>% 
         mutate(sp_group = ifelse(species %in% c("Fagus_sylvatica", "Quercus_robur", "Betula_pendula", "Quercus_ilex"), "broadleaved", "coniferous"))) +
  # geom_point(aes(x = pca_value, y = mean_change, group = species, col = species, alpha = 0)) +
  geom_smooth(aes(x = ANP_avg, y = csi, group = sp_group, col = sp_group), method = "lm", se = TRUE) +
  scale_color_manual(values = species_colors) +
  ylab("delta CSI [%]") + xlab("cold-wet to warm-dry gradient")



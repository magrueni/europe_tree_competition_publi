library(DBI)
library(dplyr)
library(readr)
library(ggplot2)


path <- "/.../"


### load data from the sim database to get the state frequency
## precalculated - see folder dnn_data

# simulation_db <- DBI::dbConnect(RSQLite::SQLite(), paste0("/.../simulation_data/simulation_db_users_v6.sqlite"))
# 
# examples_final <- dbReadTable(simulation_db, "examples_pruned_5_v17")
# examples_final_aug <- dbReadTable(simulation_db, "examples_aug_pruned_5_v17")
# 
# 
# states_count <- rbind(examples_final %>% dplyr::select(-dist), examples_final_aug) %>% 
#   group_by(svd_state) %>% 
#   summarise(count = n()) %>% 
#   ungroup() %>% 
#   mutate(count_class = case_when(count < 20 ~ 1,
#                                  count < 100 ~ 2,
#                                  count < 500 ~ 3,
#                                  count < 1000 ~ 4, 
#                                  count >= 1000 ~ 5))
# 
# hist(states_count$count, breaks = 100)
# nrow(states_count[states_count$count_class == 1,])
# nrow(states_count[states_count$count_class == 2,])
# nrow(states_count[states_count$count_class == 3,])
# nrow(states_count[states_count$count_class == 4,])
# nrow(states_count[states_count$count_class == 5,])
# 
# 
# write_csv(states_count, psate0(path, "/dnn_data/training_samples_statecount.csv"))


# 
# examples_count <- rbind(examples_final %>% select(-dist), examples_final_aug) %>% 
#   group_by(svd_state) %>% 
#   mutate(count = n()) %>% 
#   ungroup() %>% 
#   mutate(count_class = case_when(count < 10 ~ 1,
#                                  count < 100 ~ 2,
#                                  count < 1000 ~ 3,
#                                  count < 10000 ~ 4, 
#                                  count >= 10000 ~ 5))
# 
# hist(examples_count$count)



# at this point, the classes are given to python, where the model predicts on the test data from the different classes
# see script dnn_eval.py 
# the results are loaded here
result_df <- read_csv(paste0(path, "dnn_data/crossval_state.csv"))


colnames(result_df) <- c("Iteration", "1 - 20", "21 - 100", "101 - 500", "501 - 1000", "> 1000", "Overall")

mean(result_df$`1 - 20`)
mean(result_df$Overall)

# Reshape the data to long format
data_long <- result_df %>%
  dplyr::select(-Iteration) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Class",
    values_to = "Value"
  )

# Order the classes by name
data_long$Class <- factor(data_long$Class, levels = c("Overall", "1 - 20", "21 - 100", "101 - 500", "501 - 1000", "> 1000"))


# Create the boxplot
p1 <- ggplot(data_long, aes(x = Class, y = Value, fill = Class)) +
  geom_boxplot() +
  theme_classic() +
  labs(
    x = "State frequency",
    y = "Accuracy"
  )

p1

ggsave(p1, filename = paste0(path, "/figures/figureS22.png"), width = 7, height = 7)
write_csv(data_long, paste0(path, "/figure_data/figureS22.csv"))


### permutation analysis -------------------------------------------------------------------------
result_df <- read_csv(paste0(path, "/dnn_data/variable_permutation.csv"))

base_acc <- mean(result_df$Overall)
result_df <- result_df %>% dplyr::select(-Overall)

plot_df <- base_acc - result_df
colnames(plot_df) <- c("State", "State history", "Residence time", 
                       "Residence time history", "Climate", "Soil")

# Calculate mean and confidence intervals
summary_data <- plot_df %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value") %>%
  group_by(Category) %>%
  summarize(
    Mean = mean(Value),
    Lower = Mean - qt(0.975, df = n() - 1) * sd(Value) / sqrt(n()),
    Upper = Mean + qt(0.975, df = n() - 1) * sd(Value) / sqrt(n())
  )

summary_data <- summary_data %>%
  mutate(Category = factor(Category, levels = Category[order(-Mean)]))

# Create the barplot with error bars
p2 <- ggplot(summary_data, aes(x = Category, y = Mean, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_hline(yintercept = base_acc, color = "red", size = 1) + 
  theme_classic() +
  labs(
    title = "Permutation Importance",
    x = "Variables",
    y = "Explained Accuracy"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

p2
ggsave(p2, filename = paste0(path, "/figures/figureS23.png"), width = 7, height = 7)
write_csv(summary_data, paste0(path, "/figure_data/figureS23.csv"))



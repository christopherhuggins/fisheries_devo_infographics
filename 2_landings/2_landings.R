library(readxl)
library(tidyverse)
library(hrbrthemes)

landings <- read_xlsx("data/Published_dataset_2017.xlsx")

uk_nations <- c("England", "Northern Ireland", "Scotland", "Wales")

landings_overview <- landings %>%
  filter(`Port Nationality` %in% uk_nations) %>%
  group_by(`Port Nationality`) %>%
  summarise(total_weight = sum(`Landed weight (tonnes)`), total_value = sum(`Value(£000s)`))

plot_landings_weight <- ggplot(landings_overview, aes(y = total_weight/1000, x = `Port Nationality`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = format(round(total_weight, digits = 0), big.mark = ",")), vjust = -0.15, fontface = 2, size = 3) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
  labs(title = "Total landings by weight",
       y = "Total landings (thousand tonnes)",
       x = NULL)
ggsave(plot_landings_weight, file = "2_landings/plot_landings_weight.png", height = 8, width = 14, units = "cm", dpi = 1200)

plot_landings_value <- ggplot(landings_overview, aes(y = total_value/1000, x = `Port Nationality`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste("£", format(round((total_value * 1000), digits = 0), big.mark = ","))), vjust = -0.15, fontface = 2, size = 3) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
  labs(title = "Total landings by value",
       y = "Total landings (£millions)",
       x = NULL)
ggsave(plot_landings_value, file = "2_landings/plot_landings_value.png", height = 8, width = 14, units = "cm", dpi = 1200)


landings_species <- landings %>%
  filter(`Port Nationality` %in% uk_nations) %>%
  group_by(`Port Nationality`, `Species Group`) %>%
  summarise(total_landings = sum(`Landed weight (tonnes)`), total_value = sum(`Value(£000s)`))

plot_species_landings <- ggplot(landings_species, aes(x = `Port Nationality`, y = total_landings/1000, fill = `Species Group`)) +
  geom_col(position = position_dodge()) +
#  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#666666", "#D55E00", "#0072B2")) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), legend.position = "bottom") +
  labs(y = "Total landings (thousand tonnes)",
       x = NULL,
       fill = NULL)
ggsave(plot_species_landings, file = "2_landings/plot_species_landings.png", height = 8, width = 14, units = "cm", dpi = 1200)


plot_species_value <- ggplot(landings_species, aes(x = `Port Nationality`, y = total_value/1000, fill = `Species Group`)) +
  geom_col(position = position_dodge()) +
  #  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#666666", "#D55E00", "#0072B2")) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), legend.position = "bottom") +
  labs(y = "Total landings (£millions)",
       x = NULL,
       fill = NULL)
ggsave(plot_species_value, file = "2_landings/plot_species_value.png", height = 8, width = 14, units = "cm", dpi = 1200)


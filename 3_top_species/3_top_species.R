library(readxl)
library(tidyverse)
library(hrbrthemes)

#Data available from Marine Management Organisation, https://www.gov.uk/government/statistical-data-sets/uk-and-foreign-vessels-landings-by-uk-port-and-uk-vessel-landings-abroad
landings <- read_xlsx("data/landings/Published_dataset_2017.xlsx")

uk_nations <- c("England", "Northern Ireland", "Scotland", "Wales")

species_landings <- landings %>%
  filter(`Port Nationality` %in% uk_nations) %>%
  group_by(`Port Nationality`, Species) %>%
  summarise(total_weight = sum(`Landed weight (tonnes)`), total_value = sum(`Value(£000s)`))


uk_top10 <- species_landings %>%
  group_by(Species) %>%
  summarise(total_value = sum(total_value)) %>%
  filter(rank(desc(total_value)) <= 10)

plot_uk_top10 <- ggplot(filter(species_landings, Species %in% uk_top10$Species), aes(x = reorder(Species, total_value), y = total_value/1000, fill = `Port Nationality`)) +
  geom_bar(stat = "sum") +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_fill_manual(values = c("#D55E00","#F0E442", "#0072B2", "#666666")) +
  guides(size = FALSE) +
  coord_flip() +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank()) +
  labs(title = "Top 10 species (by value) landed into UK ports",
       x = "Species",
       y = "Total value of landings in 2017 (£millions)")
ggsave(plot_uk_top10, file = "3_top_species/plot_uk_top10.png", height = 7.2, width = 28, units = "cm", dpi = 1200)


england_top10 <- species_landings %>%
  filter(`Port Nationality` == "England") %>%
  arrange(desc(total_value)) %>%
  top_n(10)

ni_top10 <- species_landings %>%
  filter(`Port Nationality` == "Northern Ireland") %>%
  arrange(desc(total_value)) %>%
  top_n(10)

scotland_top10 <- species_landings %>%
  filter(`Port Nationality` == "Scotland") %>%
  arrange(desc(total_value)) %>%
  top_n(10)

wales_top10 <- species_landings %>%
  filter(`Port Nationality` == "Wales") %>%
  arrange(desc(total_value)) %>%
  top_n(10)


plot_england_top10 <- ggplot(england_top10, aes(x = reorder(Species, total_value), y = total_value/1000)) +
  geom_bar(stat = "identity", fill = "#D55E00") +
  geom_text(aes(label = paste(Species, "\n£", format(round((total_value * 1000), digits = 0), big.mark = ","), sep = "")), hjust = -0.05, fontface = 2, size = 3.2) +
  scale_y_continuous(limits = c(0, 105), breaks = c(0, 20, 40, 60, 80, 100)) +
  coord_flip() +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(title = "England",
       x = "Species",
       y = "Total value of landings in 2017 (£millions)")
ggsave(plot_england_top10, file = "3_top_species/plot_england_top10.png", height = 13, width = 13, units = "cm", dpi = 1200)

plot_ni_top10 <- ggplot(ni_top10, aes(x = reorder(Species, total_value), y = total_value/1000)) +
  geom_bar(stat = "identity", fill = "#F0E442") +
  geom_text(aes(label = paste(Species, "\n£", format(round((total_value * 1000), digits = 0), big.mark = ","), sep = "")), hjust = -0.05, fontface = 2, size = 3.2) +
  scale_y_continuous(limits = c(0, 105), breaks = c(0, 20, 40, 60, 80, 100)) +
  coord_flip() +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(title = "Northern Ireland",
       x = "Species",
       y = "Total value of landings in 2017 (£millions)")
ggsave(plot_ni_top10, file = "3_top_species/plot_ni_top10.png", height = 13, width = 13, units = "cm", dpi = 1200)

plot_scotland_top10 <- ggplot(scotland_top10, aes(x = reorder(Species, total_value), y = total_value/1000)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  geom_text(aes(label = paste(Species, "\n£", format(round((total_value * 1000), digits = 0), big.mark = ","), sep = "")), hjust = -0.05, fontface = 2, size = 3.2) +
  scale_y_continuous(limits = c(0, 105), breaks = c(0, 20, 40, 60, 80, 100)) +
  coord_flip() +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(title = "Scotland",
       x = "Species",
       y = "Total value of landings in 2017 (£millions)")
ggsave(plot_scotland_top10, file = "3_top_species/plot_scotland_top10.png", height = 13, width = 13, units = "cm", dpi = 1200)

plot_wales_top10 <- ggplot(wales_top10, aes(x = reorder(Species, total_value), y = total_value/1000)) +
  geom_bar(stat = "identity", fill = "#666666") +
  geom_text(aes(label = paste(Species, "\n£", format(round((total_value * 1000), digits = 0), big.mark = ","), sep = "")), hjust = -0.05, fontface = 2, size = 3.2) +
  scale_y_continuous(limits = c(0, 105), breaks = c(0, 20, 40, 60, 80, 100)) +
  coord_flip() +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(title = "Wales",
       x = "Species",
       y = "Total value of landings in 2017 (£millions)")
ggsave(plot_wales_top10, file = "3_top_species/plot_wales_top10.png", height = 13, width = 13, units = "cm", dpi = 1200)


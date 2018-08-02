library(readxl)
library(tidyverse)
library(hrbrthemes)
library(sf)


#Data available from Marine Management Organisation, https://www.gov.uk/government/statistical-data-sets/uk-and-foreign-vessels-landings-by-uk-port-and-uk-vessel-landings-abroad
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
  scale_fill_manual(values = c("#666666", "#D55E00", "#0072B2")) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), legend.position = "bottom") +
  labs(y = "Total landings (thousand tonnes)",
       x = NULL,
       fill = NULL)
ggsave(plot_species_landings, file = "2_landings/plot_species_landings.png", height = 8, width = 14, units = "cm", dpi = 1200)


plot_species_value <- ggplot(landings_species, aes(x = `Port Nationality`, y = total_value/1000, fill = `Species Group`)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("#666666", "#D55E00", "#0072B2")) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), legend.position = "bottom") +
  labs(y = "Total landings (£millions)",
       x = NULL,
       fill = NULL)
ggsave(plot_species_value, file = "2_landings/plot_species_value.png", height = 8, width = 14, units = "cm", dpi = 1200)


#NUTS2 Shapefile available from Eurostat, http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
nuts2_map <- read_sf("data/ref-nuts-2013-03m.shp/NUTS_RG_03M_2013_3857_LEVL_2.shp", layer = "NUTS_RG_03M_2013_3857_LEVL_2")

landings_by_nuts2 <- landings %>%
  filter(`Port Nationality` %in% uk_nations) %>%
  group_by(`Port NUTS 2 area`) %>%
  summarise(total_landings = sum(`Landed weight (tonnes)`), total_value = sum(`Value(£000s)`)) %>%
  mutate(NUTS_NAME = `Port NUTS 2 area`)

landings_by_nuts2$NUTS_NAME <- recode(landings_by_nuts2$NUTS_NAME, "East Riding and North Lincolnshire" = "East Yorkshire and Northern Lincolnshire")
landings_by_nuts2$NUTS_NAME <- recode(landings_by_nuts2$NUTS_NAME, "West Wales and the Valleys" = "West Wales and The Valleys")

merge_nuts2 <- merge(nuts2_map, landings_by_nuts2, by = "NUTS_NAME")


plot_landings_by_region <- ggplot(merge_nuts2) +
  geom_sf(aes(fill = total_landings), size = 0.2, inherit.aes = FALSE) +
  scale_fill_gradient(name="Total landings\n(tonnes)", limits = c(0,200000), low="white", high="black", trans = "sqrt", label = scales::comma) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank())
ggsave(plot_landings_by_region, file = "2_landings/plot_landings_by_region.png", height = 16.71, width = 13.55, units = "cm", dpi = 1200)

plot_value_by_region <- ggplot(merge_nuts2) +
  geom_sf(aes(fill = total_value/1000), size = 0.2, inherit.aes = FALSE) +
  scale_fill_gradient(name="Total landings\n(£millions)", low="white", high="black", limits = c(0,250), trans = "sqrt") +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank())
ggsave(plot_value_by_region, file = "2_landings/plot_value_by_region.png", height = 16.71, width = 13.55, units = "cm", dpi = 1200)

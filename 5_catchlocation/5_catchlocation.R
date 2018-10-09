library(tidyverse)
library(readxl)
library(sf)
library(hrbrthemes)
library(scales)

#Import UK Vessels Landing by ICES Rectangle data
#Available from Marine Management Organisation https://www.gov.uk/government/statistics/uk-sea-fisheries-annual-statistics-report-2017
landings <- list.files(path = "data/landings_by_rectangle", pattern = ".xlsx") %>%
  map(read_excel, sheet = 3) %>%
  bind_rows()

landings_summary <- landings %>%
  group_by(Rectangle, `Vessel Nationality`) %>%
  summarise(total_weight = sum(`Live Weight (tonnes)`), total_value = sum(`Value (£)`))

# Load ICES rectangles shapefile, available from ICES, http://ices.dk/marine-data/maps/Pages/default.aspx
ices <- st_read("data/spatial/ices_rectangles/ICES_Statistical_Rectangles_Eco.shp")

# Load EEZ shapefile, available from MarineRegions.org, http://www.marineregions.org/downloads.php
eez <- st_read("data/spatial/marineregions_World_EEZ_v10_20180221/eez_v10.shp")

# Load UK Internal EEZ borders
uk_internal <- st_read("data/spatial/uk_eez_internal/uk_eez_internal_borders.shp")

# Join UK vessel landings to ICES rectangles
landings_rectangles <- inner_join(ices, landings_summary, by = c("ICESNAME" = "Rectangle"))

plot_inset_eng <- ggplot() +
  geom_sf(data = filter(landings_rectangles, `Vessel Nationality` == "UK - England"), aes(fill = total_weight), colour = NA) +
  scale_fill_gradient(guide = FALSE, low = "#eaae7f", high = "#d55e00") +
  geom_sf(data = eez, fill = NA) +
  coord_sf(xlim = c(-38, 38), ylim = c(80, 38)) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank())
ggsave(plot_inset_eng, file = "plot_inset_eng.png", height = 14.5, width = 14.5, units = "cm", dpi = 1200)

plot_inset_ni <- ggplot() +
  geom_sf(data = filter(landings_rectangles, `Vessel Nationality` == "UK - Northern Ireland"), aes(fill = total_weight), colour = NA) +
  scale_fill_gradient(guide = FALSE, low = "#f7f1a0", high = "#f0e442") +
  geom_sf(data = eez, fill = NA) +
  coord_sf(xlim = c(-38, 38), ylim = c(80, 38)) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank())
ggsave(plot_inset_ni, file = "plot_inset_ni.png", height = 14.5, width = 14.5, units = "cm", dpi = 1200)

plot_inset_scot <- ggplot() +
  geom_sf(data = filter(landings_rectangles, `Vessel Nationality` == "UK - Scotland"), aes(fill = total_weight), colour = NA) +
  scale_fill_gradient(guide = FALSE, low = "#7fb8d8", high = "#0072b2") +
  geom_sf(data = eez, fill = NA) +
  coord_sf(xlim = c(-38, 38), ylim = c(80, 38)) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank())
ggsave(plot_inset_scot, file = "plot_inset_scot.png", height = 14.5, width = 14.5, units = "cm", dpi = 1200)

plot_inset_wales <- ggplot() +
  geom_sf(data = filter(landings_rectangles, `Vessel Nationality` == "UK - Wales"), aes(fill = total_weight), colour = NA) +
  scale_fill_gradient(guide = FALSE, low = "#b2b2b2", high = "#666666") +
  geom_sf(data = eez, fill = NA) +
  coord_sf(xlim = c(-38, 38), ylim = c(80, 38)) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank())
ggsave(plot_inset_wales, file = "plot_inset_wales.png", height = 14.5, width = 14.5, units = "cm", dpi = 1200)



plot_zoom_eng <- ggplot() +
  geom_sf(data = filter(landings_rectangles, `Vessel Nationality` == "UK - England"), aes(fill = total_weight), colour = NA) +
  scale_fill_gradient(name = "Total weight\n2012-2017\n(tonnes)", low = "#eaae7f", high = "#d55e00", label = comma) +
  geom_sf(data = eez, fill = NA) +
  geom_sf(data = uk_internal, fill = NA) +
  coord_sf(xlim = c(-15, 7), ylim = c(63.5, 48)) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(legend.justification = "top", panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank()) +
  labs(title = "England")
ggsave(plot_zoom_eng, file = "plot_zoom_eng.png", height = 14.8, width = 14.8, units = "cm", dpi = 1200)

plot_zoom_ni <- ggplot() +
  geom_sf(data = filter(landings_rectangles, `Vessel Nationality` == "UK - Northern Ireland"), aes(fill = total_weight), colour = NA) +
  scale_fill_gradient(name = "Total weight\n2012-2017\n(tonnes)", low = "#f7f1a0", high = "#f0e442", label = comma) +
  geom_sf(data = eez, fill = NA) +
  geom_sf(data = uk_internal, fill = NA) +
  coord_sf(xlim = c(-15, 7), ylim = c(63.5, 48)) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(legend.justification = "top", panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank()) +
  labs(title = "Northern Ireland")
ggsave(plot_zoom_ni, file = "plot_zoom_ni.png", height = 14.8, width = 14.8, units = "cm", dpi = 1200)

plot_zoom_scot <- ggplot() +
  geom_sf(data = filter(landings_rectangles, `Vessel Nationality` == "UK - Scotland"), aes(fill = total_weight), colour = NA) +
  scale_fill_gradient(name = "Total weight\n2012-2017\n(tonnes)", low = "#7fb8d8", high = "#0072b2", label = comma) +
  geom_sf(data = eez, fill = NA) +
  geom_sf(data = uk_internal, fill = NA) +
  coord_sf(xlim = c(-15, 7), ylim = c(63.5, 48)) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(legend.justification = "top", panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank()) +
  labs(title = "Scotland")
ggsave(plot_zoom_scot, file = "plot_zoom_scot.png", height = 14.8, width = 14.8, units = "cm", dpi = 1200)

plot_zoom_wales <- ggplot() +
  geom_sf(data = filter(landings_rectangles, `Vessel Nationality` == "UK - Wales"), aes(fill = total_weight), colour = NA) +
  scale_fill_gradient(name = "Total weight\n2012-2017\n(tonnes)", low = "#b2b2b2", high = "#666666", label = comma) +
  geom_sf(data = eez, fill = NA) +
  geom_sf(data = uk_internal, fill = NA) +
  coord_sf(xlim = c(-15, 7), ylim = c(63.5, 48)) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(legend.justification = "top", panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank()) +
  labs(title = "Wales")
ggsave(plot_zoom_wales, file = "plot_zoom_wales.png", height = 14.8, width = 14.8, units = "cm", dpi = 1200)



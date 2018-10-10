library(tidyverse)
library(readxl)
library(sf)
library(hrbrthemes)
library(scales)
library(RColorBrewer)

#Import UK Vessels Landing by ICES Rectangle data
#Available from Marine Management Organisation https://www.gov.uk/government/statistics/uk-sea-fisheries-annual-statistics-report-2017
landings <- list.files(path = "data/landings_by_rectangle", pattern = ".xlsx", full.names = TRUE) %>%
  map(read_excel, sheet = 3) %>%
  bind_rows()

landings_nephrops <- landings %>%
  filter(`Species Code` == "NEP") %>%
  group_by(Rectangle, `Vessel Nationality`) %>%
  summarise(total_weight = sum(`Live Weight (tonnes)`), total_value = sum(`Value (£)`))

landings_nephrops_all <- landings %>%
  filter(`Species Code` == "NEP") %>%
  group_by(Rectangle) %>%
  summarise(total_weight = sum(`Live Weight (tonnes)`), total_value = sum(`Value (£)`))

# Load ICES rectangles shapefile, available from ICES, http://ices.dk/marine-data/maps/Pages/default.aspx
ices <- st_read("data/spatial/ices_rectangles/ICES_Statistical_Rectangles_Eco.shp")

# Load EEZ shapefile, available from MarineRegions.org, http://www.marineregions.org/downloads.php
eez <- st_read("data/spatial/marineregions_World_EEZ_v10_20180221/eez_v10.shp")

# Load UK Internal EEZ borders
uk_internal <- st_read("data/spatial/uk_eez_internal/uk_eez_internal_borders.shp")

landings_nephrops_all_rectangles <- inner_join(ices, landings_nephrops_all, by = c("ICESNAME" = "Rectangle"))

landings_nephrops_scot_rectangles <- inner_join(ices, filter(landings_nephrops, `Vessel Nationality` == "UK - Scotland"), by = c("ICESNAME" = "Rectangle"))

landings_nephrops_ni_rectangles <- inner_join(ices, filter(landings_nephrops, `Vessel Nationality` == "UK - Northern Ireland"), by = c("ICESNAME" = "Rectangle"))

plot_nephrops_all <- ggplot() +
  geom_sf(data = landings_nephrops_all_rectangles, aes(fill = total_weight), colour = NA) +
  scale_fill_gradientn(name = "Total weight\n2012-2017\n(tonnes)", colours = brewer.pal(7, "YlGn")) +
  geom_sf(data = eez, fill = NA) +
  coord_sf(xlim = c(-38, 38), ylim = c(80, 38)) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank()) +
  labs(title = "Location of UK Nephrops catch",
       subtitle = "2012-2017",
       caption = "Data: Marine Management Organisation\nDr Christopher Huggins, @chris_huggins")
ggsave(plot_nephrops_all, file = "5_catchlocation/plot_nephrops_all.png", height = 14.5, width = 14.5, units = "cm", dpi = 1200)

plot_nephrops_zoom <- ggplot() +
  geom_sf(data = landings_nephrops_all_rectangles, aes(fill = total_weight), colour = NA) +
  scale_fill_gradientn(name = "Total weight\n2012-2017\n(tonnes)", colours = brewer.pal(7, "YlGn")) +
  geom_sf(data = eez, fill = NA) +
  geom_sf(data = uk_internal, fill = NA) +
  coord_sf(xlim = c(-15, 7), ylim = c(63.5, 48)) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank()) +
  labs(title = "Location of UK Nephrops catch",
       subtitle = "2012-2017",
       caption = "Data: Marine Management Organisation\nDr Christopher Huggins, @chris_huggins")
ggsave(plot_nephrops_zoom, file = "5_catchlocation/plot_nephrops_zoom.png", height = 14.5, width = 14.5, units = "cm", dpi = 1200)


plot_nephrops_scot <- ggplot() +
  geom_sf(data = landings_nephrops_scot_rectangles, aes(fill = total_weight), colour = NA) +
  scale_fill_gradientn(name = "Total weight\n2012-2017\n(tonnes)", colours = brewer.pal(7, "YlGn")) +
  geom_sf(data = eez, fill = NA) +
  coord_sf(xlim = c(-38, 38), ylim = c(80, 38)) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank()) +
  labs(title = "Location of Scottish Nephrops catch",
       subtitle = "2012-2017",
       caption = "Data: Marine Management Organisation\nDr Christopher Huggins, @chris_huggins")
ggsave(plot_nephrops_scot, file = "5_catchlocation/plot_nephrops_scot.png", height = 14.5, width = 14.5, units = "cm", dpi = 1200)

plot_nephrops_scot_zoom <- ggplot() +
  geom_sf(data = landings_nephrops_scot_rectangles, aes(fill = total_weight), colour = NA) +
  scale_fill_gradientn(name = "Total weight\n2012-2017\n(tonnes)", colours = brewer.pal(7, "YlGn")) +
  geom_sf(data = eez, fill = NA) +
  geom_sf(data = uk_internal, fill = NA) +
  coord_sf(xlim = c(-15, 7), ylim = c(63.5, 48)) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank()) +
  labs(title = "Location of Scottish Nephrops catch",
       subtitle = "2012-2017",
       caption = "Data: Marine Management Organisation\nDr Christopher Huggins, @chris_huggins")
ggsave(plot_nephrops_scot_zoom, file = "5_catchlocation/plot_nephrops_scot_zoom.png", height = 14.5, width = 14.5, units = "cm", dpi = 1200)


plot_nephrops_ni_zoom <- ggplot() +
  geom_sf(data = landings_nephrops_ni_rectangles, aes(fill = total_weight), colour = NA) +
  scale_fill_gradientn(name = "Total weight\n2012-2017\n(tonnes)", colours = brewer.pal(7, "YlGn")) +
  geom_sf(data = eez, fill = NA) +
  geom_sf(data = uk_internal, fill = NA) +
  coord_sf(xlim = c(-15, 7), ylim = c(63.5, 48)) +
  theme_ipsum(plot_margin = margin(7,7,7,7)) +
  theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"), panel.background = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank()) +
  labs(title = "Location of NI Nephrops catch",
       subtitle = "2012-2017",
       caption = "Data: Marine Management Organisation\nDr Christopher Huggins, @chris_huggins")
ggsave(plot_nephrops_ni_zoom, file = "5_catchlocation/plot_nephrops_ni_zoom.png", height = 14.5, width = 14.5, units = "cm", dpi = 1200)

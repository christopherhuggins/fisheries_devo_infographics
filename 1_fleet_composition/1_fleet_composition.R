library(tidyverse)
library(readxl)
library(hrbrthemes)

under10_data <- read_excel("data/Jul_2018_Under_10m_vessel_list.xls", sheet = 1, skip = 4)

over10_data <- read_excel("data/Jul_2018_Over_10m_vessel_list.xls", sheet = 1, skip = 4)

vessels <- bind_rows(under10_data, over10_data)

scot_ports <- c("ABERDEEN", "AYR", "BUCKIE", "CAMPBELTOWN", "EYEMOUTH", "FRASERBURGH", "KINLOCHBERVIE", "KIRKWALL", "LERWICK", "LOCHINVER", "MALLAIG", "OBAN", "PETERHEAD", "PITTENWEEM", "PORTREE", "SCRABSTER", "STORNOWAY", "ULLAPOOL", "WICK")
ni_ports <- c("BELFAST")
wales_ports <- c("MILFORD HAVEN")
eng_ports <- c("BRIXHAM", "FLEETWOOD", "GRIMSBY", "HASTINGS", "LOWESTOFT", "NEWLYN", "NORTH SHIELDS", "PLYMOUTH", "POOLE", "SCARBOROUGH")

unique_vessels <- vessels %>%
  distinct(`Licence number`, .keep_all = TRUE) %>%
  mutate(category = ifelse(`Overall length` > 10, "Over 10m", "10m and under")) %>%
  mutate(nationality = 
           ifelse(`Administrative port` %in% scot_ports, "Scotland",
                  ifelse(`Administrative port` %in% ni_ports, "Northern Ireland",
                         ifelse(`Administrative port` %in% eng_ports, "England", "Wales"))))


vessels_summary <- unique_vessels %>%
  group_by(nationality) %>%
  summarise(mean_size = mean(`Overall length`, na.rm = TRUE), count = n(), tonnage = sum(`Registered tonnage`))

uk_mean <- mean(unique_vessels$`Overall length`, na.rm = TRUE)

eng_mean <- mean(filter(unique_vessels, nationality == "England")$`Overall length`, na.rm = TRUE)
ni_mean <- mean(filter(unique_vessels, nationality == "Northern Ireland")$`Overall length`, na.rm = TRUE)
scot_mean <- mean(filter(unique_vessels, nationality == "Scotland")$`Overall length`, na.rm = TRUE)
wales_mean <- mean(filter(unique_vessels, nationality == "Wales")$`Overall length`, na.rm = TRUE)

plot_england <- ggplot(filter(unique_vessels, nationality == "England"), aes(x = `Overall length`, stat(count))) +
  geom_density(fill = "#CCCCCC") +
  geom_vline(xintercept =  uk_mean, colour = "#D55E00") +
  geom_text(aes(x = uk_mean, y = 0, label = paste("UK mean (", round(uk_mean, digits = 1), "m)", sep = "")), angle = 90, vjust = 1.5, hjust = 0, size = 3, colour = "#D55E00") +
  geom_vline(xintercept = eng_mean, colour = "#0072B2") +
  geom_text(aes(x = eng_mean, y = 0, label = paste("England mean (", round(eng_mean, digits = 1), "m)", sep = "")), angle = 90, vjust = -1.5, hjust = 0, size = 3, colour = "#0072B2") +
  scale_y_continuous(limits = c(0,500), trans = "sqrt") +
  scale_x_continuous(limits = c(0,120), breaks = c(30,60,90,120)) +
  theme_ipsum(plot_margin = margin(7, 7, 7, 7),  plot_title_margin = 5) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "England", y = "Count", x = "Vessel length (metres)")
ggsave(plot_england, file = "1_fleet_composition/composition_england.png", height = 2.531496, width = 7.5905512, dpi = 1200)


plot_northernireland <- ggplot(filter(unique_vessels, nationality == "Northern Ireland"), aes(x = `Overall length`, stat(count))) +
  geom_density(fill = "#CCCCCC") +
  geom_vline(xintercept =  uk_mean, colour = "#D55E00") +
  geom_text(aes(x = uk_mean, y = 0, label = paste("UK mean (", round(uk_mean, digits = 1), "m)", sep = "")), angle = 90, vjust = -1.5, hjust = 0, size = 3, colour = "#D55E00") +
  geom_vline(xintercept = ni_mean, colour = "#0072B2") +
  geom_text(aes(x = ni_mean, y = 0, label = paste("Northern Ireland mean (", round(ni_mean, digits = 1), "m)", sep = "")), angle = 90, vjust = 1.5, hjust = 0, size = 3, colour = "#0072B2") +
  scale_y_continuous(limits = c(0,500), trans = "sqrt") +
  scale_x_continuous(limits = c(0,120), breaks = c(30,60,90,120)) +
  theme_ipsum(plot_margin = margin(7, 7, 7, 7),  plot_title_margin = 5) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Northern Ireland", y = "Count", x = "Vessel length (metres)")
ggsave(plot_northernireland, file = "1_fleet_composition/composition_northernireland.png", height = 2.531496, width = 7.5905512, dpi = 1200)


plot_scotland <- ggplot(filter(unique_vessels, nationality == "Scotland"), aes(x = `Overall length`, stat(count))) +
  geom_density(fill = "#CCCCCC") +
  geom_vline(xintercept =  uk_mean, colour = "#D55E00") +
  geom_text(aes(x = uk_mean, y = 0, label = paste("UK mean (", round(uk_mean, digits = 1), "m)", sep = "")), angle = 90, vjust = -1.5, hjust = 0, size = 3, colour = "#D55E00") +
  geom_vline(xintercept = scot_mean, colour = "#0072B2") +
  geom_text(aes(x = scot_mean, y = 0, label = paste("Scotland mean (", round(scot_mean, digits = 1), "m)", sep = "")), angle = 90, vjust = 1.5, hjust = 0, size = 3, colour = "#0072B2") +
  scale_y_continuous(limits = c(0,500), trans = "sqrt") +
  scale_x_continuous(limits = c(0,120), breaks = c(30,60,90,120)) +
  theme_ipsum(plot_margin = margin(7, 7, 7, 7),  plot_title_margin = 5) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Scotland", y = "Count", x = "Vessel length (metres)")
ggsave(plot_scotland, file = "1_fleet_composition/composition_scotland.png", height = 2.531496, width = 7.5905512, dpi = 1200)


plot_wales <- ggplot(filter(unique_vessels, nationality == "Wales"), aes(x = `Overall length`, stat(count))) +
  geom_density(fill = "#CCCCCC") +
  geom_vline(xintercept =  uk_mean, colour = "#D55E00") +
  geom_text(aes(x = uk_mean, y = 0, label = paste("UK mean (", round(uk_mean, digits = 1), "m)", sep = "")), angle = 90, vjust = 1.5, hjust = 0, size = 3, colour = "#D55E00") +
  geom_vline(xintercept = wales_mean, colour = "#0072B2") +
  geom_text(aes(x = wales_mean, y = 0, label = paste("Wales mean (", round(wales_mean, digits = 1), "m)", sep = "")), angle = 90, vjust = -1.5, hjust = 0, size = 3, colour = "#0072B2") +
  scale_y_continuous(limits = c(0,500), trans = "sqrt") +
  scale_x_continuous(limits = c(0,120), breaks = c(30,60,90,120)) +
  theme_ipsum(plot_margin = margin(7, 7, 7, 7),  plot_title_margin = 5) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Wales", y = "Count", x = "Vessel length (metres)")
ggsave(plot_wales, file = "1_fleet_composition/composition_wales.png", height = 2.531496, width = 7.5905512, dpi = 1200)


u10_england <- unique_vessels %>%
  filter(nationality == "England") %>%
  group_by(category) %>%
  summarise(count = n()) %>%
  mutate(pct = count / sum(count))
u10_england$labs <- scales::percent(u10_england$pct)

plot_u10_england <- ggplot(u10_england, aes(x = "", y = pct, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#D55E00","#0072B2")) +
  geom_text(aes(label = labs), size = 7, colour = "white", fontface = 2, position = position_stack(vjust = 0.5)) +
  theme_ipsum(plot_margin = margin(0, 0, 0, 0)) +
  theme(axis.text = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.text=element_text(size=20)) +
  labs(fill = NULL, x = NULL, y = NULL)
ggsave(plot_u10_england, file="1_fleet_composition/u10_england.png", height = 4.070866, width = 4.377952, dpi = 1200)


u10_northernireland <- unique_vessels %>%
  filter(nationality == "Northern Ireland") %>%
  group_by(category) %>%
  summarise(count = n()) %>%
  mutate(pct = count / sum(count))
u10_northernireland$labs <- scales::percent(u10_northernireland$pct)

plot_u10_northernireland <- ggplot(u10_northernireland, aes(x = "", y = pct, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#D55E00","#0072B2")) +
  geom_text(aes(label = labs), size = 7, colour = "white", fontface = 2, position = position_stack(vjust = 0.5)) +
  theme_ipsum(plot_margin = margin(0, 0, 0, 0)) +
  theme(axis.text = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.text=element_text(size=20)) +
  labs(fill = NULL, x = NULL, y = NULL)
ggsave(plot_u10_northernireland, file="1_fleet_composition/u10_northernireland.png", height = 4.070866, width = 4.377952, dpi = 1200)


u10_scotland <- unique_vessels %>%
  filter(nationality == "Scotland") %>%
  group_by(category) %>%
  summarise(count = n()) %>%
  mutate(pct = count / sum(count))
u10_scotland$labs <- scales::percent(u10_scotland$pct)

plot_u10_scotland <- ggplot(u10_scotland, aes(x = "", y = pct, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#D55E00","#0072B2")) +
  geom_text(aes(label = labs), size = 7, colour = "white", fontface = 2, position = position_stack(vjust = 0.5)) +
  theme_ipsum(plot_margin = margin(0, 0, 0, 0)) +
  theme(axis.text = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.text=element_text(size=20)) +
  labs(fill = NULL, x = NULL, y = NULL)
ggsave(plot_u10_scotland, file="1_fleet_composition/u10_scotland.png", height = 4.070866, width = 4.377952, dpi = 1200)


u10_wales <- unique_vessels %>%
  filter(nationality == "Wales") %>%
  group_by(category) %>%
  summarise(count = n()) %>%
  mutate(pct = count / sum(count))
u10_wales$labs <- scales::percent(u10_wales$pct)

plot_u10_wales <- ggplot(u10_wales, aes(x = "", y = pct, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#D55E00","#0072B2")) +
  geom_text(aes(label = labs), size = 7, colour = "white", fontface = 2, position = position_stack(vjust = 0.5)) +
  theme_ipsum(plot_margin = margin(0, 0, 0, 0)) +
  theme(axis.text = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.text=element_text(size=20)) +
  labs(fill = NULL, x = NULL, y = NULL)
ggsave(plot_u10_wales, file="1_fleet_composition/u10_wales.png", height = 4.070866, width = 4.377952, dpi = 1200)
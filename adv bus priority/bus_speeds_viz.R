#Setup for Plotting
library(tidyverse)
library(sf)
library(tidytransit)
library(hms)
library(units)
library(ggplot2)
library(RColorBrewer)
library(showtext)
library(readxl)
font_add_google("Montserrat", family = "Montserrat")
showtext_auto()

nvtc_colors <- c(`pinegreen` = "#005c3c", `applegreen` = "#62bb46",
                 `oceanblue` = "#0097d6", `skyblue` = "#44c8f5",
                 `violet` ="#7f3f98", `darkpurple` = "#45196f",
                 `limegreen` = "#97cb64", `pastelblue` =  "#abe1fa",
                 `purple` ="#5d2684") %>% as.vector()

agencies <- c("ART", "DASH", "Fairfax Connector", "Loudoun County Transit",
              "Metrobus", "OmniRide")
GTFS_path <- file.path ("Z:",
                        "NVTC General", "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data", "GTFS")
Project_path <- file.path ("Z:",
                           "NVTC General","Projects and Programs",
                           "Transit Resource Center (TRC)",
                           "Data Projects", "22-07 Bus Speed and Congestion Study")

load(file.path(Project_path, "data/busspeeds.RData"))

NovaSpeed <- NovaSpeed %>%
  mutate(Agency =
           ifelse(Agency == "MB", "Metrobus",
                  ifelse(Agency == "PRTC", "OmniRide",
                         ifelse(Agency == "LCT", "Loudoun County Transit",
                                ifelse(Agency == "FFX", "Fairfax Connector",
                                       Agency)))))

#Bus Speed Viz#############
#general stats by route
Stats <- NovaSpeed %>%  group_by(Agency) %>%
  summarize(min_spacing = min(dist_mi),
            mean_spacing = mean(dist_mi),
            median_spacing = median(dist_mi),
            max_spacing = max(dist_mi),
            sd_spacing = sd(dist_mi),
            min_speed = min(speed_mph),
            mean_speed = mean(speed_mph),
            median_speed = median(speed_mph),
            max_speed = max(speed_mph),
            sd_speed = sd(speed_mph),
            count = n())
write_csv(Stats, file.path(Project_path, "SummaryStats.csv"))


#save width: 950 height = 600
##Speed boxplot-------------

NovaSpeed %>% mutate(all = "All Transit Agencies") %>%
  ggplot(., aes( x=speed_mph)) +
  geom_boxplot(aes(y=Agency, color = Agency), linewidth = 1) +
  scale_color_manual(values = nvtc_colors)+
  stat_summary(aes(y = Agency, color = Agency), fun = "mean", shape = 1)+
  geom_boxplot(aes(y = all), linewidth = 1, color = "grey")+
  stat_summary(aes(y = all), fun = "mean", shape = 1, color = "grey")+
  scale_x_continuous(limits =c(0,50))+
  scale_y_discrete(limits = rev)+
  labs(x = "Speed (mph)") +
  #theme
  theme(text = element_text(family = "Montserrat", size = 40),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "lightgrey", linetype = 2),
        panel.grid.minor.x = element_line(color = "lightgrey", linetype = 2),
        legend.position = "none")
  ggsave(file.path(Project_path, "plots/speeds_box_h.png"), scale  = 1.5)

#speed Point Plot
  ##-- Median values only
medians <- NovaSpeed %>% group_by(Agency) %>%
  summarize(speed_mph = median(speed_mph),
            dist_mi = median(dist_mi))

NovaSpeed %>%
  ggplot(., aes(x=speed_mph, y=Agency, color = Agency))+
  geom_point(data = medians, aes(speed_mph, Agency), size = 3)+
  geom_label(data = medians, aes(label = paste0(round(speed_mph, 0), " mph")), nudge_y = -0.25, size = 18, family = "Montserrat")+
  scale_color_manual(values = nvtc_colors)+
  scale_y_discrete(limits = rev)+
  labs(x = "Speed (mph)") +
  #theme
  theme(text = element_text(family = "Montserrat", size = 40),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),

        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey", linetype = 2),
        panel.grid.minor.y = element_line(color = "lightgrey", linetype = 2),
        legend.position = "none")
  ggsave(file.path(Project_path, "plots/speeds_median_h.png"), scale  = 0.7)

# Bus stop spacing############

#Spacing Boxplot

  NovaSpeed %>% mutate(all = "All Transit Agencies") %>%
    ggplot(., aes(x=dist_mi)) +
    geom_boxplot(aes(y=Agency, color = Agency), linewidth = 1) +
    scale_color_manual(values = nvtc_colors)+
    stat_summary(aes(y = Agency, color = Agency), fun = "mean", shape = 1)+
    geom_boxplot(aes(y = all), linewidth = 1, color = "grey")+
    stat_summary(aes(y = all), fun = "mean", shape = 1, color = "grey")+
    scale_x_continuous(limits = c(0, 1))+
    scale_y_discrete(limits = rev)+
    labs(x = "Stop Spacing (mi)") +
    #theme
    theme(text = element_text(family = "Montserrat", size = 40),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "lightgrey", linetype = 2),
          panel.grid.minor.x = element_line(color = "lightgrey", linetype = 2),
          legend.position = "none")
  ggsave(file.path(Project_path, "plots/spacing_box_h.png"), scale  = 1.5)


#Spacing Point Plot
  ##-- median values only

NovaSpeed %>%
  ggplot(., aes(x=dist_mi, y=Agency, color = Agency))+
  geom_point(data = medians, aes(dist_mi, Agency), size = 5)+
  geom_label(data = medians, aes(label = paste0(round(dist_mi, 2), " mph")), nudge_y = -0.25, size = 18, family = "Montserrat")+
  scale_color_manual(values = nvtc_colors)+
  scale_x_continuous(limits = c(0, 1))+
  scale_y_discrete(limits = rev)+
  labs(x = "Stop Spacing (mi)") +
  #theme
  theme(text = element_text(family = "Montserrat", size = 40),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),

        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey", linetype = 2),
        panel.grid.minor.y = element_line(color = "lightgrey", linetype = 2),
        legend.position = "none")
ggsave(file.path(Project_path, "plots/spacing_median_h.png"), scale  = 0.7)

# LCT and PRTC box plots --------------
lookup <- read_excel(file.path(Project_path, "Analysis/LCT-PRTC_NovaSpeeds.xlsx"),
                     sheet = "Lookup", range = "A1:F77")

lctprtc <- NovaSpeed %>% filter(Agency == "Loudoun County Transit" | Agency == "OmniRide") %>%
  inner_join(., lookup, by = "route_id")

lctprtc %>%
  ggplot(., aes(x = Agency.x, y = speed_mph, line = ServiceType, color = Agency.x))+
  geom_boxplot(alpha = 0.1, linewidth = 1, width = 0.4, position = position_dodge(1))+
  stat_summary(fun = "mean", shape = 1, position = position_dodge(width = 1))+
  scale_color_manual(values = nvtc_colors)+
  coord_flip() +
  #theme
  theme(text = element_text(family = "Montserrat", size = 40),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "lightgrey", linetype = 2),
        panel.grid.minor.x = element_line(color = "lightgrey", linetype = 2),
        legend.position = "none")
ggsave(file.path(Project_path, "plots/lctprtc_speeds_group_gg.png"), scale  = 1.5)

# Case Study: Metrobus 28A ------------------------------------------------
## setup for MB --------------
Metrobuszip <- "2023-02_Metrobus.zip"
MB <- read_gtfs(file.path(GTFS_path, Metrobuszip))

twenty8 <- inner_join(MB$stop_times, MB$trips %>% select(route_id, trip_id, direction_id)) %>%
  filter(route_id == "28A") %>% select(trip_id, direction_id, arrival_time, departure_time) %>%
  group_by(trip_id, direction_id) %>%
  summarize(time = max(departure_time)-min(arrival_time),
            start = first(arrival_time)) %>%
  mutate(timeperiod = case_when(
    start <= parse_hms("01:00:00") ~ "01:00",
    start <= parse_hms("02:00:00") ~ "02:00",
    start <= parse_hms("03:00:00") ~ "03:00",
    start <= parse_hms("04:00:00") ~ "04:00",
    start <= parse_hms("05:00:00") ~ "05:00",
    start <= parse_hms("06:00:00") ~ "06:00",
    start <= parse_hms("07:00:00") ~ "07:00",
    start <= parse_hms("08:00:00") ~ "08:00",
    start <= parse_hms("09:00:00") ~ "09:00",
    start <= parse_hms("10:00:00") ~ "10:00",
    start <= parse_hms("11:00:00") ~ "11:00",
    start <= parse_hms("12:00:00") ~ "12:00",
    start <= parse_hms("13:00:00") ~ "13:00",
    start <= parse_hms("14:00:00") ~ "14:00",
    start <= parse_hms("15:00:00") ~ "15:00",
    start <= parse_hms("16:00:00") ~ "16:00",
    start <= parse_hms("17:00:00") ~ "17:00",
    start <= parse_hms("18:00:00") ~ "18:00",
    start <= parse_hms("19:00:00") ~ "19:00",
    start <= parse_hms("20:00:00") ~ "20:00",
    start <= parse_hms("21:00:00") ~ "21:00",
    start <= parse_hms("22:00:00") ~ "22:00",
    start <= parse_hms("23:00:00") ~ "23:00",
    start > parse_hms("23:00:00") ~ "24:00",
    TRUE ~ NA_character_))

twenty8$time <- twenty8$time %>% as.numeric() %>% set_units("sec") %>% set_units("min")


## visualize median 28A trip times -----
twenty8 %>% group_by(timeperiod) %>%
  summarize(med = median(time),
            maxtime = max(time),
            mintime = min(time)) %>%
  ggplot(aes(x = timeperiod, group = 1))  +
  #geom_line(aes(y = maxtime), color = "darkgrey", linewidth = 1)+
  #geom_line(aes(y = mintime), color = "darkgrey", linewidth = 1)+
  geom_ribbon(aes(ymin = mintime, ymax = maxtime), fill = "grey", alpha = 0.3)+
  geom_point(aes(y = med), color = "#64bb46", size = 3) +
  geom_label(aes(y = med, label = paste0(med, " min")), nudge_y  = 1.5, family = "Montserrat")+
  labs(title = "Metrobus 28A",
       x = "Time of Day",
       y = "Trip Time") +
  theme(text = element_text(family = "Montserrat"),
        plot.title = element_text(face = "bold"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgrey", linetype = 2),
        panel.grid.minor = element_line(color = "lightgrey", linetype = 2))



## visualize median 28A trip times WITH DIRECTIONS -----
twenty8 %>% group_by(timeperiod, direction_id) %>%
  summarize(med = median(time),
            maxtime = max(time),
            mintime = min(time)) %>%
  mutate(direction_id = ifelse(direction_id == 0, "northbound", "southbound")) %>%
  filter(direction_id == "northbound") %>%
  ggplot(aes(x = timeperiod, group = direction_id, fill = direction_id))  +
  geom_ribbon(aes(ymin = mintime, ymax = maxtime), alpha = 0.3)+
  geom_line(aes(y = med, color = direction_id), linewidth = 0.75) +
  labs(title = "Metrobus 28A",
       x = "Time of Day",
       y = "Trip Time") +
  scale_fill_manual(values = nvtc_colors)+
  scale_color_manual(values = nvtc_colors)+
  theme(plot.title = element_blank(), #element_text(face = "bold"),
        text = element_text(family = "Montserrat", size = 24),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25, color = "lightgrey", linetype = 2),
        panel.grid.minor = element_line(linewidth = 0.25, color = "lightgrey", linetype = 2),
        legend.position = "none")
ggsave(file.path(Project_path, "plots/mb28a-north_gg.png"), scale  = 1, width = 7, height = 3, dpi = 300)



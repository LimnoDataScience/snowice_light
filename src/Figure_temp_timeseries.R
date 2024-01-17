# Load libraries
library(zoo)
library(tidyverse)
library(patchwork)
library(scales)
library(lubridate)

########################################
########## Time series plot  ###########
########################################
raw <- read_csv("field/SSB_HoboClean.csv")

df1 <- raw %>%
  filter(buoyDeploy %in% c(1,3,4,6)) |> 
  mutate(Depth = case_when(Sensor == 1  ~ 0.75, 
                           Sensor == 2  ~ 1.25, 
                           Sensor == 3  ~ 1.50, 
                           Sensor == 4  ~ 2.00, 
                           Sensor == 5  ~ 3.00, 
                           Sensor == 6  ~ 4.50, 
                           Sensor == 7  ~ 7.50)) |> 
  arrange(dateTime, Depth) %>%
  dplyr::select(dateTime, Depth, Temp_C) |> 
  mutate(winter = case_when(dateTime >= as.Date('2018-11-09') & dateTime < as.Date('2019-04-13') ~ 'winter18-19',
                            dateTime >= as.Date('2019-11-06') & dateTime < as.Date('2020-04-24') ~ 'winter19-20',
                            dateTime >= as.Date('2020-11-16') & dateTime < as.Date('2021-04-05') ~ 'winter20-21',
                            TRUE ~ NA)) |> 
  filter(!is.na(winter)) |> 
  mutate(fakeyear = if_else(month(dateTime) >= 08, `year<-`(dateTime, 2019), `year<-`(dateTime, 2020))) 

df2 = df1 |> 
  group_by(winter, Depth) |> 
  mutate(tempMA = rollapply(Temp_C, width = 10, mean, align='right', fill=NA))

usedepths = sort(unique(df1$Depth))[-2]

tsplots = list()
for (i in c(1:6)) {
  usedepth = usedepths[i]
  tsplots[[i]] = ggplot(df2 %>% filter(Depth == usedepth), 
                        aes(fakeyear, tempMA, color = winter)) +
    geom_line() +
    ylab('Temperature (°C)') +
    scale_color_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
    scale_fill_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
    labs(subtitle = paste0(usedepth, " m depth")) +
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank())
  
  if(usedepth > 2) {
    tsplots[[i]] = tsplots[[i]] + ylim(3.2,4.7)
  }
}

# wrap plots
wrap_plots(tsplots, ncol = 2) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ')') +
  plot_layout(guides = 'collect') &
  theme(plot.tag = element_text(size = 8),
        legend.position = 'bottom', 
        legend.title=element_blank(),
        legend.margin=margin(c(1,1,1,1)))

ggsave(filename = 'figs/wtemptimeseries2.png', width = 15, height = 15, units = 'cm')

########################################
###########  Energy Plot   #############
########################################
conv.layer = read.csv("output/interpolated_hourly_convective.csv") %>% 
  mutate(Time = as.POSIXct(Time))

all.dates <- seq(as.POSIXct('2018-11-01 00:00:00'), as.POSIXct('2021-05-31 00:00:00'), by = 3600)
idx.dates <- which(all.dates %in% conv.layer$Time)

add.dates <- data.frame('Time' = all.dates[-idx.dates],
                        'Buoydep' = NA,
                        'Convdep' = NA,
                        'energy' = NA,
                        'minT' = NA)

conv.layer <- rbind(conv.layer, add.dates) %>% arrange(Time)


conv.layer = conv.layer %>% 
  mutate(doy = yday(Time), year = year(Time), month = month(Time)) |> 
  mutate(winter = case_when(Time >= as.Date('2018-11-01') & Time < as.Date('2019-06-01') ~ 'winter18-19',
                            Time >= as.Date('2019-11-01') & Time < as.Date('2020-06-01') ~ 'winter19-20',
                            Time >= as.Date('2020-11-01') & Time < as.Date('2021-06-01') ~ 'winter20-21',
                            TRUE ~ NA))

winter.layer = conv.layer %>% filter(!is.na(winter)) %>%
  mutate(col =ifelse(winter == 'winter18-19', '#34cceb', ifelse(winter == 'winter19-20','#1b535e' ,'#dead1b'))) %>%
  mutate(fakeyear = if_else(month(as.Date(Time)) >= 08, `year<-`(as.Date(Time), 2019), `year<-`(as.Date(Time), 2020))) 

p1 <- ggplot(winter.layer) +
  geom_line(aes(fakeyear, energy,  col = winter)) +
  scale_color_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
  scale_fill_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
  scale_x_date(date_breaks = 'month', date_minor_breaks = 'week',
               date_labels = '%b') +
  ylim(5e7, 1.25e8) +
  labs(y = expression(paste("Internal energy (J ",m^-2,")")), x = "") +
  geom_vline(xintercept = winter.layer$fakeyear[ which(abs(winter.layer$minT - 4) < 0.003)], 
             col = winter.layer$col[ which(abs(winter.layer$minT - 4) < 0.003)]) +
  theme_bw(base_size = 9) + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        # axis.text.x = element_text(angle = 15, vjust = 0.5, hjust = 1), 
        axis.title.x = element_blank()); p1

###################### Combine plots #############################
layout <- "
AB
CD
EF
GG
"

wrap_plots(tsplots, ncol = 2) + p1 +
  plot_annotation(tag_levels = 'A', tag_suffix = ')') +
  plot_layout(design = layout, guides = 'collect') &
  theme(plot.tag = element_text(size = 8),
        legend.position = 'bottom', 
        legend.title=element_blank(),
        legend.margin=margin(c(1,1,1,1)))

ggsave(filename = 'figs/wtemptimeseries3.png', 
       width = 6, height = 7, units = 'in')




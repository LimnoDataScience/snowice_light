library(tidyverse)
library(patchwork)
library(geomtextpath) # For contour labels
library(MetBrewer)
library(scales)

# Long temp string data from Trout Bog 2021 
ui = read_csv('/Users/hilarydugan/Dropbox/SSB_TB data/TB_Buoy/2020-2021_underice/TB_2020-2021ui_TLStempstring.csv') |> 
  mutate(date_time_UTC = mdy_hm(date_time_UTC), Time = as.Date(date_time_UTC)) |> 
  group_by(Time) |> 
  summarise_all(mean)

ui.long = ui |> 
  pivot_longer(cols = tempC_120cm:tempC_720cm) |> 
  mutate(depth = parse_number(name)) |> 
  mutate(fakeyear = if_else(month(Time) >= 08, `year<-`(Time, 2019), `year<-`(Time, 2020))) |> 
  mutate(Depth_m = depth/100, winter = 'TroutBog_Snow_20-21') |> 
  select(Time, Depth_m, Temp = value, winter, fakeyear)

# ggplot(ui.long) +
#   geom_path(aes(x = Time, y = value, color = depth/100, group = depth)) +
#   scale_color_viridis_c() +
#   ylab('Temp (°C)') +
#   scale_x_date(date_breaks = 'month', date_labels = '%b') +
#   theme_bw()


# Read in raw data
raw <- read_csv("field/SSB_HoboClean.csv")

df_day <- raw %>%
  mutate(Time = as.Date(dateTime)) %>%
  arrange(Time, Depth_m) %>%
  group_by(Time, Depth_m) %>%
  summarise(Temp = mean(Temp_C)) |> 
  mutate(winter = case_when(Time >= as.Date('2018-10-01') & Time <= as.Date('2019-05-01') ~ 'SSB_Snow_18-19',
                            Time >= as.Date('2019-10-01') & Time <= as.Date('2020-05-01') ~ 'SSB_WhiteIce_19-20',
                            Time >= as.Date('2020-10-01') & Time <= as.Date('2021-05-01') ~ 'SSB_BlackIce_20-21',
                            TRUE ~ NA)) |> 
  # mutate(fakeyear = `year<-`(Time, 2020)) |> 
  mutate(fakeyear = if_else(month(Time) >= 08, `year<-`(Time, 2019), `year<-`(Time, 2020))) 

combo = df_day |> bind_rows(ui.long) |> 
  mutate(winter = factor(winter, levels = c('SSB_Snow_18-19','SSB_WhiteIce_19-20',
                                             'SSB_BlackIce_20-21','TroutBog_Snow_20-21')))

icedates = data.frame(winter = c('SSB_Snow_18-19','SSB_WhiteIce_19-20',
                                 'SSB_BlackIce_20-21','TroutBog_Snow_20-21'),
  iceon = c(as.Date('2019-11-09'), as.Date('2019-11-06'), as.Date('2019-11-16'), as.Date('2019-11-16')),
  iceoff = c(as.Date('2020-04-13'), as.Date('2020-04-24'), as.Date('2020-04-05'), as.Date('2020-04-05'))) |> 
  mutate(winter = factor(winter, levels = c('SSB_Snow_18-19','SSB_WhiteIce_19-20',
                                            'SSB_BlackIce_20-21','TroutBog_Snow_20-21')))


ggplot(combo |> filter(!is.na(winter))) +
  geom_path(aes(x = fakeyear, y = Temp, group = Depth_m, color = Depth_m)) +
  geom_vline(data = icedates, aes(xintercept = iceon), linetype = 2) +
  geom_vline(data = icedates, aes(xintercept = iceoff), linetype = 3) +
  scale_color_met_c(name = 'Degas') +
  facet_wrap(~winter, ncol = 1) +
  scale_x_date(limits = c(as.Date('2019-10-15'), as.Date('2020-05-01')),
                          date_breaks = 'month', date_labels = '%b') +
  ylim(0,7.5) +
  ylab('Temperature (°C)') +
  theme_bw(base_size = 9) +
  theme(strip.background =element_rect(fill="#9dbbcf"))+
  theme(strip.text = element_text(size = 11, face = 'bold'))

ggsave('figs/tempPlot_Lines.png', width = 6.5, height = 8, dpi = 500)


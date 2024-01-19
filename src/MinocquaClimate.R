library(tidyverse)

clim = read_csv('field//GHCND_USC00475516_Minocqua.csv') |> 
  mutate(year = year(DATE), day = yday(DATE), month = month(DATE)) |> 
  mutate(year = if_else(month >= 7, year + 1, year)) |> 
  dplyr::mutate(dayJul = if_else(month <= 6, day + 182, day-183)) |> 
  mutate(group = case_when(DATE >= as.Date('2018-11-01') & DATE <= as.Date('2019-04-30') ~ 'Year 1',
                           DATE >= as.Date('2019-11-01') & DATE <= as.Date('2020-04-30') ~ 'Year 2',
                           DATE >= as.Date('2020-11-01') & DATE <= as.Date('2021-04-30') ~ 'Year 3'))

## Total degdays ####
clim |> 
  mutate(TAVG = (TMAX + TMIN) / 2) |> 
  group_by(group) |> 
  filter(TAVG < 0) |> 
  summarise(ddegNEG = sum(TAVG))


#### Air Temperature ####
ggplot(clim) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_ribbon(data = clim |> filter(group == "Year 3"), 
              aes(x = dayJul, ymin = TMIN, ymax = TMAX, fill = 'Year 3'), alpha = 0.7) +
  geom_ribbon(data = clim |> filter(group == "Year 2"), 
              aes(x = dayJul, ymin = TMIN, ymax = TMAX, fill = 'Year 2'), alpha = 0.7) +
  geom_ribbon(data = clim |> filter(group == "Year 1"), 
              aes(x = dayJul, ymin = TMIN, ymax = TMAX, fill = 'Year 1'), alpha = 0.7) +

  scale_x_continuous(breaks = c(0,62,123,183,243,304),
                     labels = c('Jul','Sep','Nov','Jan','Mar','May')) +
  ylab("Air Temp (°C)") +
  scale_fill_manual(values = c("Year 3" = "lightblue4","Year 2" = "lightblue3","Year 1" = "black"),
                    name = "") +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        legend.key.size = unit(0.3, 'cm'),
        legend.position = c(0.1,0.15),
        legend.title = element_blank()) 

ggsave('../Climate/Minocqua_AirTemp.png', width = 5, height = 2.5, dpi = 500)  

#### Snow ####
ggplot(clim) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_col(data = clim |> filter(group == "Year 3"), 
              aes(x = dayJul, y = SNOW, fill = 'Year 3'), alpha = 0.7) +
  geom_col(data = clim |> filter(group == "Year 2"), 
              aes(x = dayJul, y = SNOW, fill = 'Year 2'), alpha = 0.7) +
  geom_col(data = clim |> filter(group == "Year 1"), 
              aes(x = dayJul, y = SNOW, fill = 'Year 1'), alpha = 0.7) +
  
  scale_x_continuous(breaks = c(0,62,123,183,243,304),
                     labels = c('Jul','Sep','Nov','Jan','Mar','May')) +
  ylab("Snowfall (mm)") +
  scale_fill_manual(values = c("Year 3" = "lightblue4","Year 2" = "lightblue3","Year 1" = "black"),
                    name = "") +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        legend.key.size = unit(0.3, 'cm'),
        legend.position = c(0.1,0.85),
        legend.title = element_blank()) 

ggsave('../Climate/Minocqua_Snow.png', width = 5, height = 2.5, dpi = 500)  

#### Precip ####
ggplot(clim) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_col(data = clim |> filter(group == "Year 3"), 
           aes(x = dayJul, y = PRCP, fill = 'Year 3'), alpha = 0.7) +
  geom_col(data = clim |> filter(group == "Year 2"), 
           aes(x = dayJul, y = PRCP, fill = 'Year 2'), alpha = 0.7) +
  geom_col(data = clim |> filter(group == "Year 1"), 
           aes(x = dayJul, y = PRCP, fill = 'Year 1'), alpha = 0.7) +
  
  scale_x_continuous(breaks = c(0,62,123,183,243,304),
                     labels = c('Jul','Sep','Nov','Jan','Mar','May')) +
  ylab("Snowfall (mm)") +
  scale_fill_manual(values = c("Year 3" = "lightblue4","Year 2" = "lightblue3","Year 1" = "black"),
                    name = "") +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        legend.key.size = unit(0.3, 'cm'),
        legend.position = c(0.1,0.85),
        legend.title = element_blank()) 

ggsave('../Climate/Minocqua_PRCP.png', width = 5, height = 2.5, dpi = 500)  

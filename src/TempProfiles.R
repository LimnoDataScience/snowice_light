library(NTLlakeloads)
library(tidyverse)

temp = loadLTERtemp() |> 
  mutate(year = year(sampledate), day = yday(sampledate), month = month(sampledate)) |> 
  mutate(year = if_else(month >= 7, year + 1, year)) |> 
  dplyr::mutate(dayJul = if_else(month <= 6, day + 182, day-183)) 

df = temp |> 
  filter(lakeid %in% c('TB','CB', 'AL')) |> 
  mutate(group = case_when(sampledate >= as.Date('2018-12-01') & sampledate <= as.Date('2019-04-01') ~ 'Winter 18-19',
                           sampledate >= as.Date('2019-12-01') & sampledate <= as.Date('2020-04-01') ~ 'Winter 19-20',
                           sampledate >= as.Date('2020-12-01') & sampledate <= as.Date('2021-04-01') ~ 'Winter 20-21')) |> 
  filter(!is.na(group)) |> 
  filter(!is.na(wtemp)) |> 
  arrange(lakeid, sampledate, depth)

### Bring in SSB data ###
tb.dates = df |> filter(lakeid == 'TB') |> distinct(sampledate)

ssb.profiles = tb.dates |> left_join(m.df, by = join_by(sampledate == Time)) |> 
  mutate(lakeid = 'SSB') |> 
  mutate(group = case_when(sampledate >= as.Date('2018-12-01') & sampledate <= as.Date('2019-04-01') ~ 'Winter 18-19',
                           sampledate >= as.Date('2019-12-01') & sampledate <= as.Date('2020-04-01') ~ 'Winter 19-20',
                           sampledate >= as.Date('2020-12-01') & sampledate <= as.Date('2021-04-01') ~ 'Winter 20-21')) |> 
  select(lakeid, sampledate, depth = Depth, wtemp = Temp, group)

### Join data
lakenames = data.frame(lakeid = c('AL','CB','TB','SSB'), 
                       lakename = c('Allequash Lake', 'Crystal Bog','Trout Bog', 'South Sparkling Bog'))

join.profiles = df |> select(lakeid, sampledate, depth, wtemp, group) |> 
  bind_rows(ssb.profiles) |> 
  left_join(lakenames) |> 
  mutate(lakename = factor(lakename, levels = c('Allequash Lake', 'Crystal Bog','Trout Bog', 'South Sparkling Bog'))) 


ggplot(join.profiles) +
  geom_path(aes(x = wtemp, y = depth, color = group, group = sampledate), linewidth = 0.5) +
  geom_point(aes(x = wtemp, y = depth, fill = group), shape = 21, stroke = 0.2) +
  scale_color_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
  scale_fill_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
  scale_y_reverse() +
  ylab('Depth (m)') +
  xlab('Water Temperature (°C)') +
  facet_wrap(~lakename) +
  theme_bw(base_size = 9)


ggsave('figs/tempProfiles.png', width = 6, height = 4, dpi = 500)



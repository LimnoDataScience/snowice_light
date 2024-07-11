library(tidyverse)
library(patchwork)

# Read in data file 
ice = read_csv('field/ice_snow_data.csv')
head(ice)

# Snowfall normal NC wisconsin https://climatology.nelson.wisc.edu/wisconsin-climate-divisions/divisional-12-month-snowfall/
# 1.5 + 9.4 + 17.3 + 17.4 + 14.5 + 11.1 + 6.7 + 0.4 #normal
# 2.1 + 2.7 + 3.6 + 10.1 + 5 + 12.7 + 5.3 # 2023-2024 winter 
# difference 36.8 inches ~ 90 cm

# Munge climate data
climate = read_csv('field/GHCND_USC00475516_Minocqua.csv') |> 
  filter(DATE >= as.Date('2018-12-01') & DATE <= as.Date('2021-04-01')) |> 
  mutate(winter = case_when(DATE >= as.Date('2018-11-01') & DATE < as.Date('2019-04-01') ~ '2018-19',
                            DATE >= as.Date('2019-11-01') & DATE < as.Date('2020-04-01') ~ '2019-20',
                            DATE >= as.Date('2020-11-01') & DATE < as.Date('2021-04-01') ~ '2020-21',
                            TRUE ~ NA)) |> 
  mutate(col = ifelse(winter == '2018-19', '#34cceb', ifelse(winter == '2019-20','#1b535e' ,'#dead1b'))) %>%
  mutate(fakeyear = if_else(month(as.Date(DATE)) >= 08, `year<-`(as.Date(DATE), 2019), `year<-`(as.Date(DATE), 2020))) |> 
  filter(!is.na(winter)) |> 
  mutate(TMEAN = (TMAX + TMIN)/2)

p.clim = ggplot(climate) +
  geom_hline(aes(yintercept = 0), linetype =2) +
  geom_ribbon(aes(fakeyear, ymin = TMIN, ymax = TMAX,  fill = winter), alpha = 0.3) +
  geom_line(aes(fakeyear, TMEAN, col = winter)) +
  scale_color_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
  scale_fill_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
  scale_x_date(date_breaks = 'month', date_minor_breaks = 'week',
               date_labels = '%b') +
  labs(y = 'Mean air temp (°C)', x = "") +
  theme_bw(base_size = 9) + 
  theme(legend.position = "right", 
        legend.key.width = unit(0.2,'cm'),
        legend.title = element_blank(),
        # axis.text.x = element_text(angle = 15, vjust = 0.5, hjust = 1), 
        axis.title.x = element_blank())


# Compare ice thicknesses in 2021
ice |> filter(Lake %in% c('SSB', 'TB'), year(sample_date) == 2019)
ice |> filter(Lake %in% c('SSB', 'TB'), year(sample_date) == 2020)
ice |> filter(Lake %in% c('SSB', 'TB'), year(sample_date) == 2021)


# Manipulation ice/snow data for plotting 
lakenames = data.frame(Lake = c('AL','CB','TB','SSB'), 
                       lakename = c('Allequash Lake', 'Crystal Bog','Trout Bog', 'South Sparkling Bog'))

icesnow = ice |> select(-secchi, -totice) |> 
  rename(snow = avsnow) |> 
  mutate(whiteice = -whiteice, blackice = -blackice) |> 
  rename(`white ice` = whiteice, `black ice` = blackice) |> 
  pivot_longer(cols = snow:`black ice`, names_to = 'icetype', values_to = 'thickness') |> 
  bind_rows(data.frame(Lake = 'SSB',sample_date = as.Date('2019-03-25'),icetype = 'white ice',thickness = NA)) |> 
  bind_rows(data.frame(Lake = 'TB',sample_date = as.Date('2020-01-07'),icetype = 'white ice',thickness = NA)) |> 
  mutate(icetype = factor(icetype, levels = c('snow','black ice','white ice'))) |> 
  left_join(lakenames) |> 
  mutate(lakename = factor(lakename, levels = c('Allequash Lake', 'Crystal Bog','Trout Bog', 'South Sparkling Bog'))) 
  
### Join data
# View(icesnow)
  
#### Ice Thickness ####
p.ice = ggplot(data = icesnow, aes(x = factor(sample_date), y = thickness, fill = icetype)) +
  geom_hline(aes(yintercept = -50), linewidth = 0.3, linetype = 2) +
  geom_hline(aes(yintercept = 0), linewidth = 0.3) +
  geom_col(width=0.5, color = 'black', linewidth = 0.2) + 
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = rev(c('#E0E0E0','#404040','lightblue3')), name = '') +
  labs(y = "Thickness (cm)") +
  facet_wrap(~lakename, scales = 'free_x', ncol = 1) +
  # annotate(geom = 'rect', xmin = 5.5, xmax = 9.5, ymin = Inf, ymax = -Inf, fill = 'lightblue2', alpha = 0.2) +
  # annotate(geom = 'rect',xmin = 0.4, xmax = 3.5, ymin = -19, ymax = -Inf, fill = 'lightblue1', alpha = 1) +
  geom_vline(aes(xintercept = 5.5), linetype = 1) +
  geom_vline(aes(xintercept = 9.5), linetype = 1) +
  # annotate("text", x = 2, y = -23, label = "Reference Year", size = 9/.pt, hjust = 0.5) +
  # annotate("text", x = 5.5, y = -23, label = "Manipulation Year 1", size = 9/.pt) +
  # annotate("text", x = 10, y = -23, label = "Manipulation Year 2", size = 9/.pt) +
  theme_bw(base_size = 9) +
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
        legend.position = 'right',
        legend.key.size = unit(0.3, 'cm'),
        # panel.grid.minor = element_line(size = rel(0.2)),
        panel.grid = element_line(linewidth = rel(0.3)),
        axis.title.x = element_blank(),
        legend.margin = margin(c(0,0,0,0), unit = "cm"))

# ggsave('figs/icePlots.png', width = 3.5, height = 4, dpi = 500)

p.clim / p.ice + 
  plot_annotation(tag_levels = 'A', tag_suffix = ')') +
  plot_layout(heights = c(1,2)) &
  theme(plot.tag = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.title=element_blank(),
        legend.margin=margin(c(1,1,1,1)))

ggsave('figs/Figure2_icePlots.png', width = 3.5, height = 5, dpi = 500)


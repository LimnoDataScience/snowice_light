library(tidyverse)

# Read in data file 
ice = read_csv('field/ice_snow_data.csv')
head(ice)

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
  pivot_longer(cols = snow:blackice, names_to = 'icetype', values_to = 'thickness') |> 
  bind_rows(data.frame(Lake = 'SSB',sample_date = as.Date('2019-03-25'),icetype = 'whiteice',thickness = NA)) |> 
  bind_rows(data.frame(Lake = 'TB',sample_date = as.Date('2020-01-07'),icetype = 'whiteice',thickness = NA)) |> 
  mutate(icetype = factor(icetype, levels = c('snow','blackice','whiteice'))) |> 
  left_join(lakenames) |> 
  mutate(lakename = factor(lakename, levels = c('Allequash Lake', 'Crystal Bog','Trout Bog', 'South Sparkling Bog'))) 
  
### Join data
View(icesnow)
  
#### Ice Thickness ####
ggplot(data = icesnow, aes(x = factor(sample_date), y = thickness, fill = icetype)) +
  geom_hline(aes(yintercept = -50), size = 0.3, linetype = 2) +
  geom_hline(aes(yintercept = 0), size = 0.3) +
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
        legend.position = 'bottom',
        legend.key.size = unit(0.3, 'cm'),
        # panel.grid.minor = element_line(size = rel(0.2)),
        panel.grid = element_line(linewidth = rel(0.3)),
        axis.title.x = element_blank(),
        legend.margin = margin(c(0,0,0,0), unit = "cm"))

ggsave('figs/icePlots.png', width = 3.5, height = 4, dpi = 500)


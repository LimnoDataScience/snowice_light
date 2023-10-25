ice = read_csv('field/ice_snow_data.csv')
head(ice)


ggplot(ice) +
  geom_col(aes(x = sample_date, y = totice)) +
  facet_wrap(~Lake)


icesnow = ice |> select(-secchi, -totice) |> 
  rename(snow = avsnow) |> 
  mutate(whiteice = -whiteice, blackice = -blackice) |> 
  pivot_longer(cols = snow:blackice, names_to = 'icetype', values_to = 'thickness') |> 
  mutate(icetype = factor(icetype, levels = c('snow','blackice','whiteice'))) 
  
  
#### Ice Thickness ####
ggplot(data = icesnow, aes(x = factor(sample_date), y = thickness, fill = icetype)) +
  geom_hline(aes(yintercept = 0), size = 0.3) +
  geom_col(width=0.5, color = 'black', linewidth = 0.2) + 
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = rev(c('#E0E0E0','#404040','lightblue3')), name = '') +
  labs(y = "Thickness (cm)") +
  facet_wrap(~Lake, scales = 'free_x', ncol = 1) +
  # annotate(geom = 'rect',xmin = 0.4, xmax = 3.5, ymin = -19, ymax = -Inf, fill = 'lightblue1', alpha = 1) +
  # annotate(geom = 'rect', xmin = 3.5, xmax = Inf, ymin = -19, ymax = -Inf, fill = 'lightblue2', alpha = 1) +
  geom_vline(aes(xintercept = 4.5), linetype = 2) +
  geom_vline(aes(xintercept = 8.5), linetype = 2) +
  annotate("text", x = 2, y = -23, label = "Reference Year", size = 9/.pt, hjust = 0.5) +
  annotate("text", x = 5.5, y = -23, label = "Manipulation Year 1", size = 9/.pt) +
  annotate("text", x = 10, y = -23, label = "Manipulation Year 2", size = 9/.pt) +
  theme_bw(base_size = 9) +
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1),
        # panel.grid.minor = element_line(size = rel(0.2)),
        panel.grid = element_line(linewidth = rel(0.3)),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.margin = margin(c(0,0,0,0), unit = "cm"))



library(tidyverse)
library(lubridate)
library(patchwork)

raw <- read_csv("field/SSB_HoboClean.csv")

df1 <- raw %>%
  arrange(dateTime, Depth_m) %>%
  mutate(year = year(dateTime),
         month = month(dateTime),
         hour = hour(dateTime)) %>%
  dplyr::select(dateTime, Depth_m, Temp_C, year, month, hour) |> 
  mutate(winter = case_when(dateTime >= as.Date('2018-11-01') & dateTime < as.Date('2019-04-13') ~ 'winter18-19',
                            dateTime >= as.Date('2019-11-01') & dateTime < as.Date('2020-04-24') ~ 'winter19-20',
                            dateTime >= as.Date('2020-11-01') & dateTime < as.Date('2021-04-05') ~ 'winter20-21',
                            TRUE ~ NA))
  

df2 = df1 %>% 
  dplyr::filter(!is.na(winter)) %>% 
  filter(hour == 12) %>%
  mutate(date = as.Date(dateTime)) |> 
  group_by(winter, date, Depth_m) %>%
  summarise(Temp = mean(Temp_C)) %>% mutate(yday = yday(date), month = month(date)) |> 
  arrange(winter, date, Depth_m)
# 

densplots = list() 
for (i in c(1:5)) {
  usemonth = c(12,1:4)[i]
  densplots[[i]] <- ggplot(df2 %>%
               filter(month == usemonth) |> 
               group_by(winter, date) %>%
               # filter(n_distinct(winter) == n_distinct(df_plot$winter)) %>%
               mutate(water_dens = water.density(Temp)/water.density(4.0),
                      dens_flag = ifelse(water.density(Temp) < (999.999), "<999.999", ">=999.999"))) +
  geom_path(aes(water.density(Temp), Depth_m, group = date, col = winter), alpha = 0.5) +
  geom_point(aes(water.density(Temp), Depth_m, group = date, col = winter), alpha = 0.5) +
  scale_y_reverse() +
  scale_x_continuous(breaks = c(999.99, 999.995, 1000), limits = c(999.990, 1000.0001)) +
  geom_vline(xintercept = 1000.00, linetype = 'dashed', colour = 'black')+
  labs(x = expression(paste("Water density (kg ",m^-3,")")), y = "Depth (m)") +
  scale_color_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
  scale_fill_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
  # xlim(999.990, 1000.0001) +
  scale_shape_discrete(name = "") +
  theme_bw(base_size = 9) +
  labs(subtitle = month.name[usemonth]) +
  theme(strip.background = element_blank(), 
        strip.placement = "outside")
}

# guide_area() puts legend in empty facet
wrap_plots(densplots) + guide_area() + 
  plot_layout(guides = 'collect')

ggsave(filename = 'figs/density_profile2.png', width = 15, height = 15, units = 'cm')


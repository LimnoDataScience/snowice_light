library(tidyverse)
library(patchwork)
library(geomtextpath) # For contour labels

# Read in raw data
raw <- read_csv("field/SSB_HoboClean.csv")

df_day <- raw %>%
  mutate(Time = as.Date(dateTime)) %>%
  arrange(Time, Depth_m) %>%
  group_by(Time, Depth_m) %>%
  summarise(Temp = mean(Temp_C))


# Select depth interval
dz = 0.5
depths = seq(0.15, max(df_day$Depth_m), dz)
df_temp = matrix(NA, nrow = length(unique(df_day$Time)), ncol = 1 + length(depths))
df_temp = as.data.frame(df_temp)
colnames(df_temp) = c('Time', depths)
df_temp$Time = as.Date(df_temp$Time)

for (t in unique(df_day$Time)){
  data = df_day %>%
    filter(Time == t) %>%
    arrange(Depth_m)
  
  interpolated <- approx(data$Depth_m, data$Temp, seq(0.15,  max(df_day$Depth_m), dz) , rule = 2)
  idx = match(mean(data$Time),  unique(df_day$Time))
  
  # df_temp[idx,] = c(mean(data$Time), interpolated$y)
  df_temp$Time[idx] = as.Date(mean(data$Time))
  df_temp[idx, 2:ncol(df_temp)] = interpolated$y
}

df_temp = as_tibble(df_temp)
m.df = df_temp |> pivot_longer(cols = -1, names_to = 'Depth', values_to = 'Temp') |> 
  mutate(year = year(Time), month = month(Time)) |> 
  mutate(winter = case_when(Time >= as.Date('2018-11-01') & Time <= as.Date('2019-05-01') ~ 'winter18-19',
                            Time >= as.Date('2019-11-01') & Time <= as.Date('2020-05-01') ~ 'winter19-20',
                            Time >= as.Date('2020-11-01') & Time <= as.Date('2021-05-01') ~ 'winter20-21',
                            TRUE ~ NA)) |> 
  mutate(Depth = as.numeric(Depth)) |> 
  mutate(fakeDate = `year<-`(Time, 2020)) |> 
  mutate(fakeDate = if_else(month(fakeDate) > 7, `year<-`(fakeDate, 2019), fakeDate))

makeTemp <- function(useyear, year1, year2) {
 ggplot(m.df %>% filter(winter == useyear), 
              aes(x = fakeDate, y = Depth, z = Temp)) +
    geom_raster(aes(fill = Temp), interpolate = TRUE) +
    geom_contour(colour = 'black', breaks = c(1:4)) +
    geom_textcontour(data = m.df %>% filter(winter == useyear, month %in% 1:2), 
                      breaks = c(1:4), color = 'black', size = 3,
                      aes(label = stat(level)), vjust = 1.2, straight = TRUE) +
    scale_fill_gradientn(limits = c(0,5),
                         colours = rev(RColorBrewer::brewer.pal(11, 'RdYlBu'))) +
    theme_minimal() + 
    scale_x_date(limits = c(as.Date('2019-11-01'),as.Date('2020-05-10')), 
                 date_breaks = 'month', date_labels = '%b', expand = c(0,0)) +
    annotate('text', label = paste0(year1,' - ', year2), 
            x = as.Date('2020-05-05'), 
            y = 3, hjust = 0.5, vjust = 0.5, angle = 270, size = 2.5) +
    xlab('Time') +
    ylab('Depth (m)') +
    labs(fill = 'Water Temp (°C)')+
    scale_y_reverse() + 
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank())
}

p1 = makeTemp(useyear = 'winter18-19', year1 = '2018', year2 = '2019'); p1
p2 = makeTemp(useyear = 'winter19-20', year1 = '2019', year2 = '2020')
p3 = makeTemp(useyear = 'winter20-21', year1 = '2020', year2 = '2021')

p1/p2/p3 + plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom', 
        legend.key.height = unit(0.2,'cm'))

ggsave('figs/tempPlot.png', width = 6.5, height = 5, dpi = 500)

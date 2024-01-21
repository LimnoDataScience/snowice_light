library(tidyverse)
library(directlabels)
library(patchwork)
library(scales)

df_hour <- read_csv("output/hourly_observed_wtemp.csv")
df_temp = read_csv("output/interpolated_hourly_wtemp.csv") 
# 
timestamps = as.POSIXct(df_hour$Date, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')
df_temp$Time <- unique(df_hour$Date)
m.df <- reshape2::melt(df_temp, "Time")

all.dates <- seq(as.POSIXct('2018-11-01 00:00:00'), as.POSIXct('2021-05-31 00:00:00'), by = 3600)
idx = match(m.df$Time, as.numeric(all.dates))

idx.dates <- which(all.dates %in% m.df$Time)
all.df <- data.frame('Time' = all.dates[-idx.dates])
df.empty = matrix(NA, nrow= length(all.dates[-idx.dates]), ncol = length(seq(0.15,7.5, 0.1)))
colnames(df.empty) = as.character(seq(0.15,7.5, 0.1))
all.df <- as.data.frame(cbind(all.df, df.empty))

m.all.df <- reshape2::melt(all.df, "Time")
m.all.df$value <- as.numeric(m.all.df$value)


m.df <- rbind(m.df, m.all.df)
m.df = m.df %>% arrange(Time, variable)

m.df = m.df %>%
  mutate(year = year(Time),
         month = month(Time))

m.df$winter = NA
m.df$winter[m.df$month >= 11 & m.df$year == 2018] = 'winter18-19'
m.df$winter[m.df$month < 5 & m.df$year == 2019] = 'winter18-19'
m.df$winter[m.df$month >= 11 & m.df$year == 2019] = 'winter19-20'
m.df$winter[m.df$month < 5 & m.df$year == 2020] = 'winter19-20'
m.df$winter[m.df$month >= 11 & m.df$year == 2020] = 'winter20-21'
m.df$winter[m.df$month < 5 & m.df$year == 2021] = 'winter20-21'

brks = c(4,(3.5),(3.0), (2.5),0.5)

m.df$variable = as.numeric(gsub("X",'',m.df$variable))

plotHeatMap <- function(usewinter, usetitle) {
  ggplot(m.df %>% filter(winter == usewinter), aes(x=(Time),y= ((variable)), z = (as.numeric(value)))) +
    geom_raster(aes(fill = (as.numeric(value))), interpolate = TRUE) +
    geom_contour(colour = 'black', breaks = c(4,(3.5),(3.0), (2.5),0.5)) +
    scale_fill_gradientn(limits = c(0,8),
                         colours = rev(RColorBrewer::brewer.pal(11, 'RdBu')))+
    # geom_dl(aes(label=..level..), method="bottom.pieces",
    #         stat="contour", breaks = brks) +
    ylab('Depth [m]') +
    labs(fill = 'Water temperature (\u00B0C)')+
    labs(subtitle = usetitle) +
    # geom_line(data = avgtemp, aes(time, thermoclineDep, col = 'thermocline depth'), linetype = 'dashed', col = 'brown') +
    # geom_line(data = df.ice, aes(time, ice_h * (-1), col = 'ice thickness'), linetype = 'solid', col = 'darkblue') +
    scale_y_reverse(limits = c(7.45,0.7), expand = c(0,0)) + 
    scale_x_datetime(breaks = 'month', date_labels = '%b', expand = c(0,0)) +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom",
          axis.title.x = element_blank()) 
}

g1 = plotHeatMap('winter18-19', 'Winter 2018-19'); g1
g2 = plotHeatMap('winter19-20', 'Winter 2019-20'); g2
g3 = plotHeatMap('winter20-21', 'Winter 2020-21'); g3

p1 = g1 / g2 / g3 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8),
        legend.key.height = unit(0.3, 'cm'), 
        legend.key.width = unit(1, 'cm'),
        legend.position = 'bottom', 
        legend.margin=margin(c(1,1,1,1)))

ggsave(filename = 'figs/wtempmap.png', plot = p1, width = 6, height = 9, units = 'in')

#####
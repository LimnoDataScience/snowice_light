library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(MetBrewer)
library(rLakeAnalyzer)
library(patchwork)
library(ggridges)
library(tidyr)
library(plotly)
# library(plot3D)
library(grDevices)
library(directlabels)

# setwd("C:/Documents and Settings/ladwi/Documents/Projects/R/snowice_light/")
# setwd('/home/robert/Projects/snowice_light')
setwd("/Users/robertladwig/Documents/snowice_light")

raw <- read_csv("field/SSB_HoboClean.csv")

df <- raw %>%
  arrange(dateTime, Depth_m) %>%
  mutate(year = year(dateTime),
         month = month(dateTime),
         hour = hour(dateTime)) %>%
  dplyr::select(dateTime, Depth_m, Temp_C, year, month, hour)

df$winter = NA
df$winter[df$month >= 11 & df$year == 2018] = 'winter18-19'
df$winter[df$month <= 4 & df$year == 2019] = 'winter18-19'
df$winter[df$month >= 11 & df$year == 2019] = 'winter19-20'
df$winter[df$month <= 4 & df$year == 2020] = 'winter19-20'
df$winter[df$month >= 11 & df$year == 2020] = 'winter20-21'
df$winter[df$month <= 4 & df$year == 2021] = 'winter20-21'


ggplot(df ) +
  geom_line(aes(dateTime, Temp_C, group = as.factor(Depth_m), col = as.factor(Depth_m)))

ggplot(df %>% filter(Depth_m == 0.7)) +
  geom_line(aes(dateTime, Temp_C))

df %>% mutate(year = year(dateTime), hour = hour(dateTime), date = yday(dateTime)) %>% filter(year == 2021) %>%
  group_by(hour, date,dateTime, Depth_m) %>% summarise(Temp = mean(Temp_C)) %>% 
  arrange(dateTime, date, hour, Depth_m) %>%
  # filter(row_number() %% 72 == 1) %>%
  ggplot() +
  geom_path(aes(Temp, Depth_m, group = as.factor(hour), col = date)) +
  scale_y_reverse() +
  # theme(legend.position = "none") +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) 

ggplot(df %>% dplyr::filter(!is.na(winter)) %>% mutate(date = yday(dateTime), weekday = wday(dateTime)) %>% 
         dplyr::filter(weekday %in% c(1,4,7)) %>%
         group_by(winter, date, Depth_m) %>%
         summarise(Temp = mean(Temp_C))) +
  geom_path(aes(Temp, Depth_m, group = as.factor(date), col = (date))) +
  scale_y_reverse() +
  # theme(legend.position = "none") +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  facet_wrap(~ winter, ncol =  1)

df_plot = df %>% dplyr::filter(!is.na(winter)) %>% mutate(date = yday(dateTime), weekday = wday(dateTime),
                                                          hour = hour(dateTime),
                                                          Time = as.POSIXct(paste0(as.Date(dateTime),' ', hour,':00:00'))) %>% 
  filter(hour == 12 ) %>%
  group_by(winter, Time, Depth_m) %>%
  summarise(Temp = mean(Temp_C)) %>% mutate(date = yday(Time), month = month(Time))

df_plot %>%
  group_by(date) %>%
  filter(n_distinct(winter) == n_distinct(df_plot$winter))

ggplot(df_plot %>%
         group_by(date) %>%
         filter(n_distinct(winter) == n_distinct(df_plot$winter))) +
  geom_path(aes(Temp, Depth_m, group = as.factor(date), col = (Temp))) +
  scale_y_reverse() +
  # theme(legend.position = "none") +
  scale_color_gradientn(colors = rev(met.brewer("Hokusai1", n=100))) +
  facet_wrap(~ winter + factor(month, levels = c(12,1,2,3,4)), ncol =  5)


















ggplot(df %>% dplyr::filter(!is.na(winter)) %>% mutate(date = yday(dateTime), weekday = wday(dateTime)) %>% 
         group_by(winter, date, Depth_m) %>%
         summarise(Temp = mean(Temp_C))) +
  geom_path(aes(Temp, Depth_m, group = as.factor(date), col = (date))) +
  scale_y_reverse() +
  # theme(legend.position = "none") +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  facet_wrap(~ winter, ncol =  1)

ggplot(df %>% dplyr::filter(!is.na(winter)) %>% mutate(date = yday(dateTime), weekday = wday(dateTime)) %>% 
         group_by(winter, date, Depth_m) %>%
         summarise(Temp = mean(Temp_C))) +
  geom_point(aes(date, Depth_m)) +
  scale_y_reverse() +
  # theme(legend.position = "none") +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  facet_wrap(~ winter, ncol =  1)





df_day <- df %>%
  # mutate(hour = hour(dateTime),
  #        Time = as.POSIXct(paste0(as.Date(dateTime),' ', hour,':00:00'))) %>%
  mutate(day = day(dateTime),
         Time = as.Date(dateTime)) %>%
  arrange(Time, Depth_m) %>%
  group_by(Time, Depth_m) %>%
  summarise(Temp = mean(Temp_C))

# df_day <- df %>%
#   mutate(hour = hour(dateTime),
#          Time = as.POSIXct(paste0(as.Date(dateTime),' ', hour,':00:00'))) %>%
#   # mutate(day = day(dateTime),
#          # Time = as.Date(dateTime)) %>%
#   arrange(Time, Depth_m) %>%
#   group_by(Time, Depth_m) %>%
#   summarise(Temp = mean(Temp_C))


dz = 0.1
depths = seq(0.15, max(df_day$Depth_m), dz)
df_temp = matrix(NA, nrow = length(unique(df_day$Time)), ncol = 1 + length(depths))
df_temp = as.data.frame(df_temp)
colnames(df_temp) = c('Time', depths)
df_temp$Time = as.Date(df_temp$Time)
conv.layer <- data.frame('Time' = NA,
                         'Buoydep' = NA,
                         'Convdep' = NA,
                         'energy' = NA,
                         'minT' = NA)
conv.layer$Time = as.Date(conv.layer$Time)

for (t in unique(df_day$Time)){
  data = df_day %>%
    filter(Time == t) %>%
    arrange(Depth_m)
  
  interpolated <- approx(data$Depth_m, data$Temp, seq(0.15,  max(df_day$Depth_m), dz) , rule = 2)
  
  buoy.dep <- center.buoyancy(interpolated$y, interpolated$x)
  
  idx = match(mean(data$Time),  unique(df_day$Time))
  
  # df_temp[idx,] = c(mean(data$Time), interpolated$y)
  df_temp$Time[idx] = as.Date(mean(data$Time))
  df_temp[idx, 2:ncol(df_temp)] = interpolated$y
  
  
  df.test = data.frame('depth' = interpolated$x,
                       'temp' = interpolated$y) %>%
    mutate(            'density'= water.density( temp),
                       'diff.dens' = c(NA, diff(density)),
                       'diff.temp' = c(NA, diff(temp))) %>%
    mutate(flag.dens = abs(diff.dens)< 1e-5,
           flag.temp = abs(diff.temp) < 1e-3,
           flag = ifelse(flag.dens == T & flag.temp == T, T, F))
  
  if (any(na.omit(df.test$flag) == T)){
    conv.layer.depth = df.test %>% filter(flag == T) %>%
    summarise(max(depth))
  } else {
    conv.layer.depth = NA
  }
  
  bathymetry = approx.bathy(Zmax = 8, Zmean = 3.6, lkeArea = 4400, method = 'voldev', zinterval = dz)
  energy = internal.energy(wtr = interpolated$y, depths=interpolated$x,
                           bthA = bathymetry$Area.at.z, bthD = bathymetry$depths)
  
  conv.layer = rbind(conv.layer, data.frame('Time' = as.Date(mean(data$Time)),
                                            'Buoydep' = buoy.dep,
                                            'Convdep' = as.numeric(conv.layer.depth),
                                            'energy' = energy,
                                            'minT' = min(interpolated$y)))
}
all.dates <- seq(as.Date('2018-11-01'), as.Date('2021-05-31'), by = 1)
idx.dates <- which(all.dates %in% conv.layer$Time)

add.dates <- data.frame('Time' = all.dates[-idx.dates],
                        'Buoydep' = NA,
                        'Convdep' = NA,
                        'energy' = NA,
                        'minT' = NA)

conv.layer <- rbind(conv.layer, add.dates) %>% arrange(Time)


conv.layer = conv.layer %>% 
  mutate(doy = yday(Time), year = year(Time), month = month(Time))
conv.layer$winter = NA
conv.layer$winter[conv.layer$month >= 11 & conv.layer$year == 2018] = 'winter18-19'
conv.layer$winter[conv.layer$month <= 5 & conv.layer$year == 2019] = 'winter18-19'
conv.layer$winter[conv.layer$month >= 11 & conv.layer$year == 2019] = 'winter19-20'
conv.layer$winter[conv.layer$month <= 5 & conv.layer$year == 2020] = 'winter19-20'
conv.layer$winter[conv.layer$month >= 11 & conv.layer$year == 2020] = 'winter20-21'
conv.layer$winter[conv.layer$month <= 5 & conv.layer$year == 2021] = 'winter20-21'

ggplot(conv.layer %>% filter(!is.na(winter))) +
  geom_point(aes(doy, energy)) +
  facet_wrap(~ winter + factor(month, levels = c(10, 11,12,1,2,3,4,5,6)), ncol =  9, scales = 'free_x')

winter.layer = conv.layer %>% filter(!is.na(winter)) %>%
  mutate(col =ifelse(winter == 'winter18-19', 'darkred', ifelse(winter == 'winter19-20','#E69F00' ,"#56B4E9"))) %>%
  mutate(date = as.Date(format(Time, format ='%m-%d'), format ='%m-%d' ))
p1 <- ggplot(winter.layer) +
  geom_line(aes(date, energy,  col = winter), linewidth = 1.5) +
  # facet_wrap(~ winter , ncol= 1, scales = 'free_x') +
  # scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(11, 'RdYlBu'))) +
  scale_color_manual(values = c('darkred','#E69F00', "#56B4E9"), name='') +
  labs(y = expression(paste("Internale energy (J ",m^-2,")")), x = "") +
  geom_vline(xintercept= winter.layer$date[ which(abs(winter.layer$minT - 4) < 0.01)], col = winter.layer$col[ which(abs(winter.layer$minT - 4) < 0.01)]) +
  theme_bw(); p1

ggsave(filename = 'figs/energy.png', plot = p1, width = 30, height = 15, units = 'cm')


## DENSITy PLOT
m.df <- reshape2::melt(df_temp, "Time")
m.df$Time <- unique(df_day$Time)

all.dates <- seq(as.Date('2018-11-01'), as.Date('2021-05-31'), by = 1)
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

g1 <- ggplot(m.df %>% filter(winter == 'winter18-19'), aes((Time), as.numeric(as.character(variable)), z = water.density(as.numeric(value)))) +
  geom_raster(aes(fill = water.density(as.numeric(value))), interpolate = TRUE) +
  geom_contour(colour = 'black', breaks = c(water.density(3.5),water.density(3.0), water.density(2.5))) +
  scale_fill_gradientn(limits = c(999.890,999.9999999999999999999999999999999),
                       colours = rev(RColorBrewer::brewer.pal(11, 'PuBuGn')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth [m]') +
  labs(fill = 'Density [kg/m3]')+
  ggtitle('Winter 18-19')+
  # geom_line(data = avgtemp, aes(time, thermoclineDep, col = 'thermocline depth'), linetype = 'dashed', col = 'brown') +
  # geom_line(data = df.ice, aes(time, ice_h * (-1), col = 'ice thickness'), linetype = 'solid', col = 'darkblue') +
  scale_y_reverse(limits = c(3.5,0.15)) + theme_bw(); g1

g2 <- ggplot(m.df %>% filter(winter == 'winter19-20'), aes((Time), as.numeric(as.character(variable)), z = water.density(as.numeric(value)))) +
  geom_raster(aes(fill = water.density(as.numeric(value))), interpolate = TRUE) +
  geom_contour(colour = 'black', breaks = c(water.density(3.5),water.density(3.0), water.density(2.5))) +
  scale_fill_gradientn(limits = c(999.890,999.9999999999999999999999999999999),
                       colours = rev(RColorBrewer::brewer.pal(11, 'PuBuGn')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth [m]') +
  labs(fill = 'Density [kg/m3]')+
  ggtitle('Winter 19-20')+
  # geom_line(data = avgtemp, aes(time, thermoclineDep, col = 'thermocline depth'), linetype = 'dashed', col = 'brown') +
  # geom_line(data = df.ice, aes(time, ice_h * (-1), col = 'ice thickness'), linetype = 'solid', col = 'darkblue') +
  scale_y_reverse(limits = c(3.5,0.15)) + theme_bw(); g2

g3 <- ggplot(m.df %>% filter(winter == 'winter20-21'), aes(x=(Time),y= as.numeric(as.character(variable)), z = water.density(as.numeric(value)))) +
  geom_raster(aes(fill = water.density(as.numeric(value))), interpolate = TRUE) +
  geom_contour(colour = 'black', breaks = c(water.density(3.5),water.density(3.0), water.density(2.5))) +
  scale_fill_gradientn(limits = c(999.890,999.9999999999999999999999999999999),
                       colours = rev(RColorBrewer::brewer.pal(11, 'PuBuGn')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth [m]') +
  labs(fill = 'Density [kg/m3]')+
  ggtitle('Winter 20-21')+
  # geom_line(data = avgtemp, aes(time, thermoclineDep, col = 'thermocline depth'), linetype = 'dashed', col = 'brown') +
  # geom_line(data = df.ice, aes(time, ice_h * (-1), col = 'ice thickness'), linetype = 'solid', col = 'darkblue') +
  scale_y_reverse(limits = c(3.5,0.15)) + theme_bw() ; g3

p2 <- g1 / g2 / g3
ggsave(filename = 'figs/densitymap.png', plot = p2, width = 20, height = 20, units = 'cm')
# write.csv(x = df_save,'~/Desktop/temp.csv', quote = F, row.names =  F)






# CHECK HOURLY
# 
# df_hour <- df %>%
#   # mutate(hour = hour(dateTime),
#   #        Time = as.POSIXct(paste0(as.Date(dateTime),' ', hour,':00:00'))) %>%
#   mutate(hour = hour(dateTime),
#          Time = (dateTime),
#          Dateie = as.Date(Time), 
#          Date = as.POSIXct(paste0(Dateie,' ',hour,':00:00'))) %>%
#   
#   group_by(Date, Depth_m) %>%
#   arrange(Depth_m) %>%
#   summarise(Temp = mean(Temp_C)) %>%
#   select(Date, Depth_m, Temp)
# 
# 
# 
# dz = 0.1
# depths = seq(0.15, max(df_hour$Depth_m), dz)
# df_temp = matrix(NA, nrow = length(unique(df_hour$Date)), ncol = 1 + length(depths))
# df_temp = as.data.frame(df_temp)
# colnames(df_temp) = c('Time', depths)
# 
# conv.layer <- data.frame('Time' = NA,
#                          'Buoydep' = NA,
#                          'Convdep' = NA,
#                          'energy' = NA,
#                          'minT' = NA)
# conv.layer$Time = as.Date(conv.layer$Time)
# 
# for (t in unique(df_hour$Date)){
#   
#   print(round((match(t, unique(df_hour$Date)) * 100)/length( unique(df_hour$Date))),2)
#   data = df_hour %>%
#     filter(Date == t) %>%
#     arrange(Depth_m)
#   
#   if (nrow(data) <2){
#     next  
#   }
#   
#   interpolated <- approx(data$Depth_m, data$Temp, seq(0.15,  max(df_hour$Depth_m), dz) , rule = 2)
#   
#   buoy.dep <- center.buoyancy(interpolated$y, interpolated$x)
#   
#   idx = match(mean(data$Date),  unique(df_hour$Date))
#   
#   # df_temp[idx,] = c(mean(data$Time), interpolated$y)
#   df_temp$Time[idx] = as.Date(mean(data$Date))
#   df_temp[idx, 2:ncol(df_temp)] = interpolated$y
#   
#   
#   df.test = data.frame('depth' = interpolated$x,
#                        'temp' = interpolated$y) %>%
#     mutate(            'density'= water.density( temp),
#                        'diff.dens' = c(NA, diff(density)),
#                        'diff.temp' = c(NA, diff(temp))) %>%
#     mutate(flag.dens = abs(diff.dens)< 1e-5,
#            flag.temp = abs(diff.temp) < 1e-3,
#            flag = ifelse(flag.dens == T & flag.temp == T, T, F))
#   
#   if (any(na.omit(df.test$flag) == T)){
#     conv.layer.depth = df.test %>% filter(flag == T) %>%
#       summarise(max(depth))
#   } else {
#     conv.layer.depth = NA
#   }
#   
#   bathymetry = approx.bathy(Zmax = 8, Zmean = 3.6, lkeArea = 4400, method = 'voldev', zinterval = dz)
#   energy = internal.energy(wtr = interpolated$y, depths=interpolated$x,
#                            bthA = bathymetry$Area.at.z, bthD = bathymetry$depths)
#   
#   conv.layer = rbind(conv.layer, data.frame('Time' = as.Date(mean(data$Date)),
#                                             'Buoydep' = buoy.dep,
#                                             'Convdep' = as.numeric(conv.layer.depth),
#                                             'energy' = energy,
#                                             'minT' = min(interpolated$y)))
# }
# 
# write.csv(x = df_temp, file = 'output/interpolated_hourly_wtemp.csv', quote = F, row.names = F)
# write.csv(x = conv.layer.depth, file = 'output/interpolated_hourly_convective.csv', quote = F, row.names = F)
# 


## DENSITy PLOT

df_temp = read.csv("output/interpolated_hourly_wtemp.csv")
m.df <- reshape2::melt(df_temp, "Time")
m.df$Time <- unique(df_hour$Date)

all.dates <- seq(as.Date('2018-11-01 00:00:00'), as.Date('2021-05-31 00:00:00'), by = 1)
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

g1 <- ggplot(m.df %>% filter(winter == 'winter18-19'), aes((Time), as.numeric(as.character(variable)), z = (as.numeric(value)))) +
  geom_raster(aes(fill = (as.numeric(value))), interpolate = TRUE) +
  geom_contour(colour = 'black', breaks = brks) +
  scale_fill_gradientn(limits = c(0,8),
                       colours = rev(RColorBrewer::brewer.pal(11, 'RdBu')))+
  # metR::geom_text_contour(aes(z = value)) +
  geom_dl(aes(label=..level..), method="bottom.pieces", 
          stat="contour",breaks = brks) +
  theme_minimal()  +xlab('Time') +
  ylab('Depth [m]') +
  labs(fill = 'Water temperature (\u00B0C)')+
  ggtitle('Winter 2018-2019')+
  # geom_line(data = avgtemp, aes(time, thermoclineDep, col = 'thermocline depth'), linetype = 'dashed', col = 'brown') +
  # geom_line(data = df.ice, aes(time, ice_h * (-1), col = 'ice thickness'), linetype = 'solid', col = 'darkblue') +
  scale_y_reverse(limits = c(7.45,0.7)) + theme_bw()+theme(legend.position = "bottom"); g1

g2 <- ggplot(m.df %>% filter(winter == 'winter19-20'), aes((Time), as.numeric(as.character(variable)), z = (as.numeric(value)))) +
  geom_raster(aes(fill = (as.numeric(value))), interpolate = TRUE) +
  geom_contour(colour = 'black', breaks = c(4,(3.5),(3.0), (2.5),0.5)) +
  scale_fill_gradientn(limits = c(0,8),
                       colours = rev(RColorBrewer::brewer.pal(11, 'RdBu')))+
  geom_dl(aes(label=..level..), method="bottom.pieces", 
          stat="contour",breaks = brks) +
  theme_minimal()  +xlab('Time') +
  ylab('Depth [m]') +
  labs(fill = 'Water temperature (\u00B0C)')+
  ggtitle('Winter 2019-2020')+
  # geom_line(data = avgtemp, aes(time, thermoclineDep, col = 'thermocline depth'), linetype = 'dashed', col = 'brown') +
  # geom_line(data = df.ice, aes(time, ice_h * (-1), col = 'ice thickness'), linetype = 'solid', col = 'darkblue') +
  scale_y_reverse(limits = c(7.45,0.7)) + theme_bw()+theme(legend.position = "bottom"); g2

g3 <- ggplot(m.df %>% filter(winter == 'winter20-21'), aes(x=(Time),y= as.numeric(as.character(variable)), z = (as.numeric(value)))) +
  geom_raster(aes(fill = (as.numeric(value))), interpolate = TRUE) +
  geom_contour(colour = 'black', breaks = c(4,(3.5),(3.0), (2.5),0.5)) +
  scale_fill_gradientn(limits = c(0,8),
                       colours = rev(RColorBrewer::brewer.pal(11, 'RdBu')))+
  geom_dl(aes(label=..level..), method="bottom.pieces", 
          stat="contour",breaks = brks) +
  theme_minimal()  +xlab('Time') +
  ylab('Depth [m]') +
  labs(fill = 'Water temperature (\u00B0C)')+
  ggtitle('Winter 2020-2021')+
  # geom_line(data = avgtemp, aes(time, thermoclineDep, col = 'thermocline depth'), linetype = 'dashed', col = 'brown') +
  # geom_line(data = df.ice, aes(time, ice_h * (-1), col = 'ice thickness'), linetype = 'solid', col = 'darkblue') +
  scale_y_reverse(limits = c(7.45,0.7)) + theme_bw() +theme(legend.position = "bottom"); g3

p1=g1 / g2 / g3 +plot_layout(guides = "collect") & theme(legend.position = 'bottom')& plot_annotation(tag_levels = 'A');p1
ggsave(filename = 'figs/wtempmap.png', plot = p1, width = 40, height = 30, units = 'cm')
#####

m_df_timeseries = m.df %>% 
  mutate(doy = yday(Time),
         date_noyear = (format((Time), format ='%m-%d %H:00:00')),
         day = day(Time),
         hour = hour(Time),
         week = week(Time),
         datetime = ifelse(month > 6, lubridate::make_datetime(2020, month, day, hour, 0, 0), lubridate::make_datetime(2021, month, day, hour, 0, 0)),
         plot_label = (format(Time, format ='%m-%d %H:00:00') ))

ggplot(m_df_timeseries %>% filter(variable == 0.75 & !is.na(winter) & week == 2)) +
  geom_line(aes(datetime, value, color = winter))

as.POSIXct((m_df_timeseries$datetime), origin='1970-01-01') 
library(scales)
plot_label = (format(m_df_timeseries$Time, format ='%m-%d %H:00:00') )
plot_breaks = seq(1, length(m_df_timeseries$datetime), 100000)


ts1 <- ggplot(m_df_timeseries %>% filter(variable == 0.75 & !is.na(winter)), aes(datetime, value, color = winter)) +
  geom_line() +
  ylab('Water temperature (\u00B0C)') +
  scale_x_continuous(breaks = plot_breaks,labels= plot_label[plot_breaks]) +
  ggtitle("0.75 m depth") +
  theme_bw() +theme(legend.position = "bottom"); ts1

ts2 <- ggplot(m_df_timeseries %>% filter(variable == 1.55 & !is.na(winter))) +
  geom_line(aes(datetime, value, color = winter)) +
  ylab('Water temperature (\u00B0C)') +
  ggtitle("1.55 m depth") +
  theme_bw() +theme(legend.position = "bottom")
ts3 <- ggplot(m_df_timeseries %>% filter(variable == 2.55 & !is.na(winter))) +
  geom_line(aes(datetime, value, color = winter)) +
  ylab('Water temperature (\u00B0C)') +
  ggtitle("2.55 m depth") +
  theme_bw() +theme(legend.position = "bottom")
ts4 <- ggplot(m_df_timeseries %>% filter(variable == 3.05 & !is.na(winter))) +
  geom_line(aes(datetime, value, color = winter)) +
  ylab('Water temperature (\u00B0C)') +
  ggtitle("3.05 m depth") +
  theme_bw() +theme(legend.position = "bottom")
ts5 <- ggplot(m_df_timeseries %>% filter(variable == 7.45 & !is.na(winter))) +
  geom_line(aes(datetime, value, color = winter)) +
  ylab('Water temperature (\u00B0C)') +
  ggtitle("7.45 m depth") +
  theme_bw() +theme(legend.position = "bottom")

p3=ts1/ts2/ts3/ts4/ts5 +plot_layout(guides = "collect") & theme(legend.position = 'bottom')& plot_annotation(tag_levels = 'A');p3

####

df_heat <- df %>% dplyr::filter(!is.na(winter)) %>% mutate(date = as.Date(dateTime), weekday = wday(dateTime)) %>% 
  # dplyr::filter(weekday %in% c(1,4,7)) %>%
  group_by(winter, date, Depth_m) %>%
  summarise(Temp = mean(Temp_C)) 

calc_heat_water <- function(input){
  
  output = data.frame(date = NULL,
                      winter = NULL,
                      conv_depth = NULL,
                      heat = NULL)
  
  dz = 0.1
  cp = 4184
  Tf = 0
  for (i in unique(input$date)){
    
    data <- input %>%
      filter(date == i)
    
    max_depth = max(input$Depth_m)
    interpolated <- approx(data$Depth_m, data$Temp, seq(0, max_depth, dz) , rule = 2)
    
    heat = (water.density(interpolated$y) * cp) %*% (interpolated$y - Tf) * dz # J/m3
    
    interpolated_belowdiff <- approx(data$Depth_m, data$Temp, seq(0, max_depth, dz) , rule = 2)
    
    conv_depth = thermo.depth(wtr = interpolated_belowdiff$y, depths = interpolated_belowdiff$x, Smin = 0.1)
    
    output = rbind(output, data.frame(date = unique(data$date),
                                      winter = unique(data$winter),
                                      conv_depth = conv_depth,
                                      heat = heat))
  }
  return(output)
}

heat_water = calc_heat_water(df_heat)

ggplot(df %>% dplyr::filter(!is.na(winter)) %>% mutate(date = yday(dateTime), weekday = wday(dateTime)) %>% 
         group_by(winter, date, Depth_m) %>%
         summarise(Temp = mean(Temp_C))) +
  geom_path(aes(Temp, Depth_m, group = as.factor(date), col = (date))) +
  scale_y_reverse() +
  # theme(legend.position = "none") +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  facet_wrap(~ winter, ncol =  1)


ggplot(heat_water) +
  geom_point(aes(date, heat/1000)) +
  ylab('Heat (kJ/m3)') + xlab('')

ggplot(heat_water) +
  geom_point(aes(date, conv_depth)) +
  ylab('Mixed layer depth') + xlab('')

snowice <- read_csv("field/SSB_ice_snow_secchi.csv")

summary(snowice)

ggplot(snowice) +
  geom_point(aes(sample_date, totice, col = 'ice')) +
  geom_point(aes(sample_date, whiteice, col = 'whiteice')) +
  geom_point(aes(sample_date, blackice, col = 'blackice')) +
  geom_point(aes(sample_date, avsnow, col = 'snow')) 

ggplot(snowice) +
  geom_point(aes(sample_date, secchi))

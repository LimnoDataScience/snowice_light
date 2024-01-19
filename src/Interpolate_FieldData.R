library(tidyverse)
library(rLakeAnalyzer)
library(stringr)

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

df_hour <- df %>%
  mutate(hour = str_pad(hour(dateTime),2, pad = '0'),
         Time = (dateTime),
         Dateie = as.Date(Time),
         Date = as.POSIXct(paste0(Dateie,' ',hour,':00:00'),
                           format = '%Y-%m-%d %H:%M:%S')) %>%
  group_by(Date, Depth_m) %>%
  arrange(Depth_m) %>%
  summarise(Temp = mean(Temp_C)) %>%
  select(Date, Depth_m, Temp)

## WHAT?!
test <- df %>%
  mutate(hour = str_pad(hour(dateTime),2, pad = '0'),
         Time = (dateTime),
         Dateie = as.Date(Time),
         Date = as.character(paste0(Dateie,' ',hour,':00:00'),
                           format = '%Y-%m-%d %H:%M:%S'))

length(unique(test$Date)) == 19930
length(unique(df_hour$Date)) == 19930
##

dz = 0.1
depths = seq(0.15, max(df_hour$Depth_m), dz)
df_temp = matrix(NA, nrow = length(unique(df_hour$Date)), ncol = 1 + length(depths))
df_temp = as.data.frame(df_temp)
colnames(df_temp) = c('Time', depths)

conv.layer <- data.frame('Time' = NULL,
                         'Buoydep' = NULL,
                         'Convdep' = NULL,
                         'energy' = NULL,
                         'minT' = NULL)
conv.layer$Time = as.Date(conv.layer$Time)

ix = 1
for (t in unique(df_hour$Date)){
  
  print(round((match(unique(df_hour$Date)[ix], 
                     unique(df_hour$Date)) * 100)/length( unique(df_hour$Date))),2)
  data = df_hour %>%
    filter(Date == t) %>%
    arrange(Depth_m)
  
  if (nrow(data) <2){
    next
  }
  
  interpolated <- approx(data$Depth_m, data$Temp, seq(0.15,  
                                                      max(df_hour$Depth_m), dz) , rule = 2)
  
  buoy.dep <- center.buoyancy(interpolated$y, interpolated$x)
  
  idx = match(mean(data$Date),  unique(df_hour$Date))
  
  df_temp$Time[idx] = as.Date(mean(data$Date))
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
  
  ### INCORPORATE REAL BATHYMETRY HERE ----------------------------------
  
  bathymetry = approx.bathy(Zmax = 8, Zmean = 3.6, lkeArea = 4400, method = 
                              'voldev', zinterval = dz)
  
  ###  ------------------------------------------------------------------
  
  energy = internal.energy(wtr = interpolated$y, depths=interpolated$x,
                           bthA = bathymetry$Area.at.z, bthD = bathymetry$depths)
  
  conv.layer = rbind(conv.layer, data.frame('Time' = (mean(data$Date)),
                                            'Buoydep' = buoy.dep,
                                            'Convdep' = as.numeric(conv.layer.depth),
                                            'energy' = energy,
                                            'minT' = min(interpolated$y)))
  
  ix = ix +1
}


write.csv(x = df_hour, file = 'output/hourly_observed_wtemp.csv', quote = F,
          row.names = F)
write.csv(x = df_temp, file = 'output/interpolated_hourly_wtemp.csv', quote = F, 
          row.names = F)
write.csv(x = conv.layer, file = 'output/interpolated_hourly_convective.csv', 
          quote = F, row.names = F)

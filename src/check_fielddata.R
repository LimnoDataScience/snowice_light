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
library(plot3D)
library(grDevices)

# setwd("C:/Documents and Settings/ladwi/Documents/Projects/R/snowice_light/")
setwd('/home/robert/Projects/snowice_light')

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

ggplot(df %>% dplyr::filter(!is.na(winter)) %>% mutate(date = yday(dateTime), weekday = wday(dateTime)) %>% 
         dplyr::filter(weekday %in% c(1,4,7)) %>%
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
  geom_path(aes(Temp, Depth_m, group = as.factor(date), col = (date))) +
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


dz = 0.1
depths = seq(0, max(df_day$Depth_m), dz)
df_temp = matrix(NA, nrow = length(unique(df_day$Time)), ncol = 1 + length(depths))
df_temp = as.data.frame(df_temp)
colnames(df_temp) = c('Time', depths)

for (t in unique(df_day$Time)){
  data = df_day %>%
    filter(Time == t)
  
  interpolated <- approx(data$Depth_m, data$Temp, seq(0,  max(df_day$Depth_m), dz) , rule = 2)
  
  idx = match(mean(data$Time),  unique(df_day$Time))
  
  df_temp[idx,] = c(mean(data$Time), interpolated$y)
}

df_temp = df_temp %>%
  mutate(Time = unique(df_day$Time))

temp = df_temp %>%
  filter(row_number() %% 7 == 1)

# write.csv(x = df_save,'~/Desktop/temp.csv', quote = F, row.names =  F)


temp %>% gather(key = depth, value = temp, '0':'7.5') %>% 
  mutate(depth = gsub("X", "", depth), depth = as.numeric(depth),
         Time = ymd(Time)) %>% 
  ggplot(aes(x = depth, y = Time, height = temp, group = Time)) + 
  geom_ridgeline() + 
  scale_x_reverse() + 
  coord_flip() 


temp_long = reshape2::melt(temp, "Time")
temp_long$Time <- unique(temp$Time)

temp_long = temp_long %>%
  mutate(year = year(Time),
         month = month(Time))

temp_long$winter = NA
temp_long$winter[temp_long$month >= 12 & temp_long$year == 2018] = 'winter18-19'
temp_long$winter[temp_long$month <= 3 & temp_long$year == 2019] = 'winter18-19'
temp_long$winter[temp_long$month >= 12 & temp_long$year == 2019] = 'winter19-20'
temp_long$winter[temp_long$month <= 3 & temp_long$year == 2020] = 'winter19-20'
temp_long$winter[temp_long$month >= 12 & temp_long$year == 2020] = 'winter20-21'
temp_long$winter[temp_long$month <= 3 & temp_long$year == 2021] = 'winter20-21'

temp_long |>
  na.omit() |> 
  plot_ly(y = ~value, x = ~variable, z = ~Time, split = ~Time,
          type = 'scatter3d', mode = 'lines', color = ~Time)


temp_long = temp_long %>% rename(Depth = variable, Temp = value) %>%
  mutate(Depth = as.numeric(as.character(Depth)))
# change depth to height above ground
temp_long$Depth <- max(temp_long$Depth, na.rm = TRUE) - temp_long$Depth


temp_ggplot = temp_long %>% filter(!is.na(winter)) %>% mutate(doy = yday(Time))
ggplot(temp_ggplot) +
  geom_line(aes(Temp, Depth, col = doy, group = doy)) +
  facet_wrap(~ winter)

temp_long = temp_long %>% filter(!is.na(winter)) %>% filter(winter == 'winter19-20') %>%
  select(Time, Depth, Temp)
## we need to prepare the data in a specific format
# subset of days and remove NAs

dat2 <- split(temp_long, as.factor(temp_long$Time))

# between each polygon we need NAs and to get a filled polygon we need two
# points for boarders. this is not very elegant but it works
dat3 <- rbind(dat2[[1]],
              data.frame(Time = dat2[[1]]$Time[1],
                         Depth = min(dat2[[1]]$Depth),
                         Temp = 0),
              data.frame(Time = dat2[[1]]$Time[1],
                         Depth = max(dat2[[1]]$Depth),
                         Temp = 0))
for (i in 2:length(dat2)) {
  dat3 <- rbind(dat3,
                data.frame(Time = NA,
                           Depth = NA,
                           Temp = NA),
                dat2[[i]],
                data.frame(Time = dat2[[i]]$Time[1],
                           Depth = min(dat2[[i]]$Depth),
                           Temp = 0),
                data.frame(Time = dat2[[i]]$Time[1],
                           Depth = max(dat2[[i]]$Depth),
                           Temp = 0))
}


x <- dat3$Temp
z <- dat3$Depth
#+ need to rescale date as the function doesnt take Posix as input
y <- (month(dat3$Time)+day(dat3$Time)/30) #* (-1)


# transparent colors (alpha)
par(mar=c(7,7,1,1), oma=c(0,0,0,0),bg = "slategray")
pmat <- polygon3D(x, y, z, border = "black", lwd = 0.75,
                  col = gg.col(1, alpha = 0.5), bg = "white",
                  xlab = "Temperature (°C)", ylab = "Month",
                  zlab = "Depth (m)", ticktype = "detailed",
                  theta = 50, phi = 10, axes = FALSE, box = TRUE,
                  expand = 0.75,
                  bty = "b2", scale = FALSE)


# data for own axis plots
xa <- range(x, na.rm = TRUE)
ya <- range(y, na.rm = TRUE)
za <- range(z, na.rm = TRUE)
# coordinates of ticks
xt <- pretty(xa)[between(pretty(xa), min (x, na.rm = TRUE), max(x, na.rm = TRUE))]
yt <- pretty(ya)[between(pretty(ya), min (y, na.rm = TRUE), max(y, na.rm = TRUE))]
zt <- pretty(za)[between(pretty(za), min (z, na.rm = TRUE), max(z, na.rm = TRUE))]
# lablels of ticks
xl <- pretty(xa)[between(pretty(xa), min (x, na.rm = TRUE), max(x, na.rm = TRUE))]
yl <- month.abb[pretty(ya)[between(pretty(ya), min (y, na.rm = TRUE), max(y, na.rm = TRUE))]]
zl <- pretty(za)[between(pretty(za), min (z, na.rm = TRUE), max(z, na.rm = TRUE))]

if(0 %in% intersect(xl, zl)) {
  zl <- as.character(zl)
  zl[zl == "0"] <- ""
}

# axis lines
lines(trans3d(xa, min(ya), min(za), pmat) , col = 1, lwd = 2)
lines(trans3d(max(xa), ya, min(za), pmat) , col = 1, lwd = 2)
lines(trans3d(min(xa), min(ya), za, pmat) , col = 1, lwd = 2)
# xaxis ticks
tick.start <- trans3d(xt, min(ya), min(za), pmat)
tick.end <- trans3d(xt, min(ya) - 0.2, min(za), pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y, lwd = 2)
# yaxis ticks
tick.start <- trans3d(max(xa), yt, min(za), pmat)
tick.end <- trans3d(max(xa) + 0.2, yt, min(za), pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y, lwd = 2)
#z axis ticks
tick.start <- trans3d(min(xa), min(ya), zt, pmat)
tick.end <- trans3d(min(xa), min(ya) - 0.2, zt, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y, lwd = 2)

# xaxis tick labels
label.pos <- trans3d(xt, min(ya) - 0.3, min(za), pmat)
text(label.pos$x, label.pos$y, labels=xl, adj=c(0, NA), srt=270, cex=1.2)
# yaxis tick labels
label.pos <- trans3d(max(xa) + 0.3, yt, min(za), pmat)
text(label.pos$x, label.pos$y, labels=yl, adj=c(0, NA), srt=-45, cex=1.2)
# zaxis tick labels
label.pos <- trans3d(min(xa), min(ya) - 0.5, zt, pmat)
text(label.pos$x, label.pos$y, labels=zl, adj=c(0, NA), srt=270, cex=1.2)

# xaxis label
label.pos <- trans3d(quantile(xa, 0.25), min(ya) - 1, min(za), pmat)
text(label.pos$x, label.pos$y, labels="Temp (°C)", adj=c(0, NA), srt=325, cex=1.3)
# yaxis label
label.pos <- trans3d(max(xa) + 2, quantile(ya, 0.35), min(za), pmat)
text(label.pos$x, label.pos$y, labels="Month", adj=c(0, NA), srt=45, cex=1.3)
# zaxis label
label.pos <- trans3d(min(xa), min(ya) - 1.2, quantile(za, 0.7), pmat)
text(label.pos$x, label.pos$y, labels="Level (m)", adj=c(0, NA), srt=280, cex=1.3)


m.df <- reshape2::melt(df_temp, "Time")
m.df$Time <- unique(df_day$Time)

m.df = m.df %>%
  mutate(year = year(Time),
         month = month(Time))

m.df$winter = NA
m.df$winter[m.df$month >= 12 & m.df$year == 2018] = 'winter18-19'
m.df$winter[m.df$month <= 3 & m.df$year == 2019] = 'winter18-19'
m.df$winter[m.df$month >= 12 & m.df$year == 2019] = 'winter19-20'
m.df$winter[m.df$month <= 3 & m.df$year == 2020] = 'winter19-20'
m.df$winter[m.df$month >= 12 & m.df$year == 2020] = 'winter20-21'
m.df$winter[m.df$month <= 3 & m.df$year == 2021] = 'winter20-21'

g1 <- ggplot(m.df %>% filter(winter == 'winter18-19'), aes((Time), as.numeric(as.character(variable)))) +
  geom_raster(aes(fill = as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(-1,4),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth [m]') +
  labs(fill = 'Temp [degC]')+
  ggtitle('Winter 18-19') +
  # geom_line(data = avgtemp, aes(time, thermoclineDep, col = 'thermocline depth'), linetype = 'dashed', col = 'brown') +
  # geom_line(data = df.ice, aes(time, ice_h * (-1), col = 'ice thickness'), linetype = 'solid', col = 'darkblue') +
  scale_y_reverse()

g2 <- ggplot(m.df %>% filter(winter == 'winter19-20'), aes((Time), as.numeric(as.character(variable)))) +
  geom_raster(aes(fill = as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(-1,4),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth [m]') +
  labs(fill = 'Temp [degC]')+
  ggtitle('Winter 19-20') +
  # geom_line(data = avgtemp, aes(time, thermoclineDep, col = 'thermocline depth'), linetype = 'dashed', col = 'brown') +
  # geom_line(data = df.ice, aes(time, ice_h * (-1), col = 'ice thickness'), linetype = 'solid', col = 'darkblue') +
  scale_y_reverse()

g3 <- ggplot(m.df %>% filter(winter == 'winter20-21'), aes((Time), as.numeric(as.character(variable)))) +
  geom_raster(aes(fill = as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(-1,4),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth [m]') +
  labs(fill = 'Temp [degC]')+
  ggtitle('Winter 20-21') +
  # geom_line(data = avgtemp, aes(time, thermoclineDep, col = 'thermocline depth'), linetype = 'dashed', col = 'brown') +
  # geom_line(data = df.ice, aes(time, ice_h * (-1), col = 'ice thickness'), linetype = 'solid', col = 'darkblue') +
  scale_y_reverse()

g1 / g2 / g3

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

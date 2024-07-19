# Load libraries
library(zoo)
library(tidyverse)
library(patchwork)
library(scales)
library(rLakeAnalyzer)
library(lubridate)

########################################
########## Time series plot  ###########
########################################
raw <- read_csv("field/SSB_HoboClean.csv")

df1 <- raw %>%
  filter(buoyDeploy %in% c(1,3,4,6)) |> 
  mutate(Depth = case_when(Sensor == 1  ~ 0.75, 
                           Sensor == 2  ~ 1.25, 
                           Sensor == 3  ~ 1.50, 
                           Sensor == 4  ~ 2.00, 
                           Sensor == 5  ~ 3.00, 
                           Sensor == 6  ~ 4.50, 
                           Sensor == 7  ~ 7.50)) |> 
  arrange(dateTime, Depth) %>%
  dplyr::select(dateTime, Depth, Temp_C) |> 
  mutate(winter = case_when(dateTime >= as.Date('2018-11-09') & dateTime < as.Date('2019-04-13') ~ 'winter18-19',
                            dateTime >= as.Date('2019-11-06') & dateTime < as.Date('2020-04-24') ~ 'winter19-20',
                            dateTime >= as.Date('2020-11-16') & dateTime < as.Date('2021-04-05') ~ 'winter20-21',
                            TRUE ~ NA)) |> 
  filter(!is.na(winter)) |> 
  mutate(fakeyear = if_else(month(dateTime) >= 08, `year<-`(dateTime, 2019), `year<-`(dateTime, 2020))) 

# (1) method 1 is profile method using eddy approach
# assuming laminar under-ice conditions, flux = -k * (T_0 - T_s) / (z_s)
k_molecular = 0.56 # molecular diffusivity, laminar flow W / m C
k_petrov = 20 # Petrov 2006, Shirasawa 2006, https://library.arcticportal.org/2769/1/A2104010.pdf 
# (2) method 2 is bulk formula for heat flux, https://eprints.lib.hokudai.ac.jp/dspace/bitstream/2115/38935/1/18IAHR2006_s84.pdf 
# flux = rho c_w C_H (T - T0) Uw
C_H = 1 * 10^(-3)
U_w = 1/100
c_w = 4186

df2 = df1 |> 
  filter(Depth <= 1) |> 
  mutate(f_molecular = - k_molecular * (0 - Temp_C) / Depth,
         f_turbulent = - k_petrov * (0 - Temp_C) / Depth,
         f_bulk = water.density(Temp_C) * c_w * C_H * U_w * (Temp_C - 0) ) |>
  group_by(winter, Depth) |> 
  mutate(tempMA = rollapply(Temp_C, width = 10, mean, align='right', fill=NA),
         molecularMA = rollapply(f_molecular, width = 10, mean, align='right', fill=NA),
         turbulentMA = rollapply(f_turbulent, width = 10, mean, align='right', fill=NA),
         bulkMA = rollapply(f_bulk, width = 10, mean, align='right', fill=NA))

usedepths = sort(unique(df1$Depth))[-2]

tsplots = list()
usedepth = usedepths[1]
ggplot(df2 %>% filter(Depth == usedepth), 
                        aes(fakeyear, molecularMA, color = 'molecular')) +
    geom_line() +
    geom_line(aes(fakeyear, turbulentMA, color = 'empirical')) +
    geom_line(aes(fakeyear, bulkMA, color = 'bulk')) +
    ylab('Heat flux (W m-2)') +
    scale_color_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
    scale_fill_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
    labs(subtitle = paste0("Surface heat flux")) +
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank())+
    facet_wrap(~ winter)
# 
# # wrap plots
# wrap_plots(tsplots, ncol = 1) + 
#   plot_layout(guides = 'collect') &
#   theme(plot.tag = element_text(size = 8),
#         legend.position = 'bottom', 
#         legend.title=element_blank(),
#         legend.margin=margin(c(1,1,1,1)))


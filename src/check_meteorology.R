library(tidyverse)
library(lubridate)
library(ggpubr)
library(ggsignif)
library(data.table)

setwd('/home/robert/Projects/snowice_light')

raw <- read_csv("field/GHCND_USC00475516_Minocqua.csv")

data = raw %>%
  mutate(month = month(DATE),
         year = year(DATE),
         doy = yday(DATE))

data$winter = NA
data$winter[data$month >= 11 & data$year == 2018] = 'winter18-19'
data$winter[data$month <= 4 & data$year == 2019] = 'winter18-19'
data$winter[data$month >= 11 & data$year == 2019] = 'winter19-20'
data$winter[data$month <= 4 & data$year == 2020] = 'winter19-20'
data$winter[data$month >= 11 & data$year == 2020] = 'winter20-21'
data$winter[data$month <= 4 & data$year == 2021] = 'winter20-21'

ggplot(data %>% filter(!is.na(winter))) +
  geom_line(aes(DATE, SNOW, col = 'Snowfall')) +
  geom_line(aes(DATE, PRCP, col = 'Rainfall')) +
  facet_wrap(~ winter, scales = 'free_x', ncol = 1) +
  ggtitle('Precipitation')

ggplot(data %>% filter(!is.na(winter))) +
  geom_line(aes(DATE, PRCP)) +
  facet_wrap(~ winter, scales = 'free_x', ncol = 1) +
  ggtitle('Rainfall')


ggplot(data %>% filter(!is.na(winter)) %>% group_by(winter) %>%
        mutate(cumSnow = cumsum(SNOW))) +
  geom_line(aes(DATE, cumSnow)) +
  facet_wrap(~ winter, scales = 'free_x', ncol = 1) +
  ggtitle('CumSnow')

ggplot(data %>% filter(!is.na(winter)) %>% group_by(winter) %>%
         mutate(cumT = cumsum(TOBS))) +
  geom_line(aes(DATE, cumT)) +
  facet_wrap(~ winter, scales = 'free_x', ncol = 1) +
  ggtitle('CumTemp')

ggplot(data %>% filter(!is.na(winter)) %>% mutate(flag = ifelse(TOBS <= 0, 1, 0))) +
  geom_line(aes(DATE, TOBS)) +
  geom_point(aes(DATE, TOBS, col = (flag))) +
  facet_wrap(~ winter, scales = 'free_x', ncol = 1) +
  ggtitle('Air temperature')




getp <- function(data, pval=.05){
  a <- stats::pairwise.wilcox.test(x=data$TOBS, g=data$winter,
                                   p.adjust.method="none", paired=FALSE)
  return(as.data.table(as.table(a$p.value)))#[!is.na(N) & N < pval])
}
dmp <- getp(data, getp)
dmp = dmp[-c(3),]
data.table::setnames(dmp, c( "start", "end", "label"))
dmp$label <- formatC(
  signif(dmp$label, digits = 3),
  digits = 3,
  format = "g",
  flag = "#"
)
dmp[, y := (0:(.N-1)) * (20/.N)+20]
data.table::setDF(dmp)
ggplot(data %>% filter(!is.na(winter)), aes(x = winter, y = TOBS) ) +
  geom_boxplot() +
  ggsignif::geom_signif(data=dmp,
                        aes(xmin=start, xmax=end, annotations=label, y_position=y),
                        textsize = 2, vjust = -0.2,
                        manual=TRUE) 


getp <- function(data, pval=.05){
  a <- stats::pairwise.wilcox.test(x=data$SNOW, g=data$winter,
                                   p.adjust.method="none", paired=FALSE)
  return(as.data.table(as.table(a$p.value)))#[!is.na(N) & N < pval])
}
dmp <- getp(data, getp)
dmp = dmp[-c(3),]
data.table::setnames(dmp, c( "start", "end", "label"))
dmp$label <- formatC(
  signif(dmp$label, digits = 3),
  digits = 3,
  format = "g",
  flag = "#"
)
dmp[, y := (0:(.N-1)) * (1e2/.N)+1e2]
data.table::setDF(dmp)
ggplot(data %>% filter(!is.na(winter)), aes(x = winter, y = SNOW) ) +
  geom_boxplot() +
  ggsignif::geom_signif(data=dmp,
                      aes(xmin=start, xmax=end, annotations=label, y_position=y),
                      textsize = 2, vjust = -0.2,
                      manual=TRUE) 

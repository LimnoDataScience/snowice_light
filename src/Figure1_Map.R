library(ggspatial)
library(sf)
library(tidyverse)
library(patchwork)
library(png)

# Read in google earth image of the bogs
# img1 <- readPNG("src/Map/Bogs.png", native = T)
img2 <- readPNG("src/Map/Bogs3.png", native = T)

# Create location data.frame
ssb <- data.frame(site = c("South Sparkling Bog", "Trout Bog", "Allequash Lake", "Crystal Bog"),
                                lat = c(46.003285, 46.041324, 46.038317, 46.007583),
                                lon = c(-89.705346, -89.682232, -89.620617, -89.606183)) %>%
  st_as_sf(coords = c('lon','lat'),crs = 4326) 

## Map of Wisconsin
states = st_read('src/Map/WI_borderstates.shp')
NHD.simple = st_read('src/Map/NHD_simple.shp')
wi.simple = st_read('src/Map/Wisconsin_State_Boundary_simple.shp')
greatLakes = st_read('src/Map/greatLakes.shp')

w1 = ggplot(wi.simple) +
  geom_sf(data = states, col = 'grey50', fill = 'grey90', alpha = 0.5, size = 0.2) +
  geom_sf(data = NHD.simple, col = NA, fill = 'lightsteelblue2') +
  geom_sf(data = greatLakes, fill = 'lightsteelblue2', col = 'lightsteelblue2') +
  # geom_sf(data = st_as_sfc(st_bbox(ssb)), fill = 'red4', color = 'red4', size = 5) + # Inset box
  geom_sf(data = ssb[1,], fill = 'red3', color = 'black', size = 4, shape = 22, stroke = 0.5) + # Inset box
  coord_sf(ylim = c(42.3,47.5), xlim = c(-93, -86), expand = FALSE) + # limit axes
  theme_bw(base_size = 8) +
  theme(#plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1))

# Google image of the bog
# w2 = ggplot(data.frame(x = 0:1, y = 0:1), aes(x,y)) +
#   geom_blank() +
#   annotation_custom(grid::rasterGrob(img1)) +
#   annotate('text', label = 'South Sparkling Bog', x = 0.2, y = 0.16, color = 'white', size = 3) +
#   annotate("segment", x = 0.13, y = 0.13, xend = 0.205, yend = 0.025,
#            arrow = arrow(angle = 30, length = unit(.15,"cm")), color = 'white', linewidth = 0.5) +
#   annotate('text', label = 'Trout Bog', x = 0.3, y = 0.91, color = 'white', size = 3) +
#   annotate("segment", x = 0.34, y = 0.88, xend = 0.42, yend = 0.82,
#            arrow = arrow(angle = 30, length = unit(.15,"cm")), color = 'white', linewidth = 0.5) +
#   theme_void()

w2 = ggplot(data.frame(x = 0:1, y = 0:1), aes(x,y)) +
  geom_blank() +
  annotation_custom(grid::rasterGrob(img2)) +
  annotate('text', label = 'South Sparkling Bog', x = 0.24, y = 0.16, color = 'white', size = 3) +
  annotate("segment", x = 0.06, y = 0.16, xend = 0, yend = 0.16,
           arrow = arrow(angle = 30, length = unit(.15,"cm")), color = 'white', linewidth = 0.5) +
  annotate('text', label = 'Trout Bog', x = 0.05, y = 0.9, color = 'white', size = 3) +
  annotate("segment", x = 0.14, y = 0.88, xend = 0.16, yend = 0.91,
           arrow = arrow(angle = 30, length = unit(.15,"cm")), color = 'white', linewidth = 0.5) +
  annotate('text', label = 'Allequash Lake', x = 0.75, y = 0.95, color = 'white', size = 3) +
  annotate("segment", x = 0.7, y = 0.9, xend = 0.8, yend = 0.9,
           arrow = arrow(angle = 30, length = unit(.15,"cm")), color = 'white', linewidth = 0.5) +
  annotate('text', label = 'Crystal Bog', x = 0.90, y = 0.34, color = 'white', size = 3) +
  annotate("segment", x = 0.9, y = 0.3, xend = 0.97, yend = 0.25,
           arrow = arrow(angle = 30, length = unit(.15,"cm")), color = 'white', linewidth = 0.5) +
  theme_void(); w2



# # Patchwork combine map 
w1 + w2 + 
plot_layout(widths = c(1,1.58)) +
plot_annotation(tag_levels = 'A', tag_suffix = ')') & 
theme(plot.tag = element_text(size = 8), 
      plot.margin = unit(c(0, 0.1, 0, 0), "cm")) 

ggsave('figs/Figure1_Map2.png', width = 6.5, height = 3, dpi = 500, bg = "transparent")
  

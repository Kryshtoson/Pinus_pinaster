library(ggnewscale)
library(raster)
library(tidyverse)
library(devtools)

Italy <- ne_countries(scale = "large", returnclass = "sf") %>% 
  filter(admin == 'Italy')

head <- read_xlsx('meta\\header_data.xlsx') %>% 
  st_as_sf(coords = c('X', 'Y'), crs = 4326) 
r <- raster("C:\\Users\\krystof\\Dropbox\\GIS_db\\DEM\\DEM-EUROPE.TIF")
r_Italy <-raster::mask(crop(r, Italy), Italy)

palette_dem <- c("#8FCE00", "#FFE599", "#F6B26B",	"#996633", "#993300")  
r_Italy_pt <- rasterToPoints(r_Italy, spatial = TRUE)
dem_df <- data.frame(r_Italy_pt)      

hsd <- hillShade(terrain(r_Italy, 'slope'), terrain(r_Italy, 'aspect'))
hsd_pt <- rasterToPoints(hsd, spatial = TRUE)
hsd_df <- data.frame(hsd_pt)      

ggplot() + 
  geom_raster(data = hsd_df, aes(x = x, y = y, fill = layer), 
              show.legend = F) +
  scale_fill_gradientn(colours = c('black', 'white')) +
  ggnewscale::new_scale_fill() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = DEM.EUROPE), alpha = .5,
              show.legend = F) +
  geom_sf(data = Italy, fill = NA, linewidth = .5) + 
  geom_sf(data = head, aes(colour = factor(twin)), size = 3, shape = 21, stroke = 1.2) +
  scale_fill_gradientn(colours = palette_dem,              
                       name = "Elevation") +
  scale_colour_discrete(name = 'Twinspan group') +
  theme_bw() +
  coord_sf(xlim = c(7, 12), ylim = c(42, 45), expand = FALSE) +
  theme(legend.position = c(0,0),
        panel.background = element_rect(fill = '#CDF5F7'),
        legend.background = element_blank(),
        axis.title = element_blank(),
        legend.justification = c(0,0))

ggsave('outputs\\Pinpir_map_DEM_3-divs.svg', height = 6, width = 8)
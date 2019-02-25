# Libraries

library(osmdata)
library(sf)
library(ggplot2)
library(ggrepel)
library(ggsflabel)
library(maps)
library(ggsn)
library(pdftools)
library(jpeg)
library(grid)
library(gridExtra)

if(!identical(getOption("bitmapType"), "cairo") && isTRUE(capabilities()[["cairo"]])){
  options(bitmapType = "cairo")
}

# Get data

## https://wiki.openstreetmap.org/wiki/Map_Features

## Azores

if(!file.exists('output/azores_coastline_line.shp')) {
  q0 <- opq ("azores portugal")
  q1 <- add_osm_feature(opq = q0,key="natural", value = "coastline") 
  azores_coastline <- osmdata_sf(q1)
  st_write(azores_coastline$osm_lines, "output/azores_coastline_line.shp", delete_dsn = TRUE)
  
  q0 <- opq ("azores portugal")
  q1 <- add_osm_feature(opq = q0,key="place", value = "archipelago") 
  azores_boundary <- osmdata_sf(q1)
  st_write(azores_boundary$osm_multipolygons, "output/azores_achipelago.shp", delete_dsn = TRUE)
  
  q0 <- opq ("azores portugal")
  q1 <- add_osm_feature(opq = q0,key="place", value = "island") 
  azores_boundary <- osmdata_sf(q1)
  st_write(azores_boundary$osm_multipolygons, "output/azores_island.shp", delete_dsn = TRUE)
} 

  azores_coastline.sf <- st_read(dsn = 'output/azores_island.shp')
  azores_bbox <- st_as_sfc(st_bbox(azores_coastline.sf))
  

    
p <- ggplot() +
  geom_sf(data=azores_coastline.sf, fill="black", color=NA) +
  geom_sf_text_repel(data=azores_coastline.sf, aes(label = name), 
                     nudge_y = c(0.1, 0.15, 0.1, 0.15, -0.15, 0.1, 0.1, 0.15, 0.15),
                     nudge_x = c(0, 0, 0, 0, 0, 0.35, 0, 0, -0.15)) +
  scale_x_continuous(breaks = c(-30, -26)) + 
  scale_y_continuous(breaks = c(37.5, 39)) + 
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=12, family = "Arial")) +
  labs(title="Azores Archipelago", x="", y="") + 
  scalebar(data=azores_coastline.sf, dist = 100, dist_unit = "km", height = 0.01, st.size = 3,
           anchor = c(x=-29.1, y=36.6) , 
           transform = TRUE, model = "WGS84", location = "bottomleft")



ggsave(filename="output/azores.pdf", 
       plot = p, 
       device = cairo_pdf, 
       width = 297, 
       height = 210, 
       units = "mm")

## Madeira

if(!file.exists('output/madeira_coastline_line.shp') | !file.exists('output/madeira_administrative_boundary.shp')) {
  q0 <- opq ("madeira portugal")
  q1 <- add_osm_feature(opq = q0,key="natural", value = "coastline") 
  madeira_coastline <- osmdata_sf(q1)
  st_write(madeira_coastline$osm_lines, "output/madeira_coastline_line.shp", delete_dsn = TRUE)
  
  q0 <- opq ("madeira portugal")
  q1 <- add_osm_feature(opq = q0,key="boundary", value = "administrative") 
  madeira_boundary <- osmdata_sf(q1)
  st_write(madeira_boundary$osm_multipolygons, "output/madeira_administrative_boundary.shp", delete_dsn = TRUE)
} 


# madeira_coastline.sf <- st_read(dsn = 'output/madeira_coastline_line.shp')
madeira_coastline.sf <- st_read(dsn = 'output/madeira_administrative_boundary.shp')
madeira_bbox <- st_as_sfc(st_bbox(madeira_coastline.sf))
q <- ggplot() +
  geom_sf(data=madeira_coastline.sf, fill="black", color=NA) +
  geom_sf_text_repel(data=madeira_coastline.sf, aes(label = c("Ilha da Madeira", "Porto Santo", "")), 
                     nudge_y = c(0.15,0.07, 0),
                     nudge_x = c(0, 0, 0)) +
  coord_sf(xlim=c(-17.2659372395217, -16.2774372395217), ylim=c(32.4037555102953, 33.1282555102953)) + 
  scale_x_continuous(breaks = c(-17, -16.5)) + 
  scale_y_continuous(breaks = c(32.6, 33)) + 
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=12, family = "Arial")) +
  labs(title="Madeira Archipelago", x="", y="") +
  scalebar(data=madeira_coastline.sf, dist = 10, dist_unit = "km", height = 0.0020, st.size = 3, st.dist =  0.005 , 
           anchor = c(x=-16.9, y=32.4) , 
           transform = TRUE, model = "WGS84", location = "bottomleft")

ggsave(filename="output/madeira.pdf", 
       plot = q, 
       device = cairo_pdf, 
       width = 297, 
       height = 210, 
       units = "mm")

  
## world

world <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
world_map <- rnaturalearth::ne_countries(scale = 'large', returnclass = c("sf"))
world_map_low <- rnaturalearth::ne_countries(scale = 'small', returnclass = c("sf"))

r <- ggplot() +
  geom_sf(data=world_map, fill="black", color="white") +
  geom_sf(data=madeira_bbox, fill=NA) +
  geom_sf_text(data=madeira_bbox, aes(label = "Madeira"), position=position_stack(vjust = 1.1)) + 
  geom_sf(data=madeira_coastline.sf, fill="black", color=NA) +
  geom_sf(data=azores_bbox, fill= NA) +
  geom_sf_text(data=azores_bbox, aes(label = "Azores"), position=position_stack(vjust = 1.075)) + 
  geom_sf(data=azores_coastline.sf, fill="black", color=NA) +
  coord_sf(crs = 54030, datum = 4326) +
  scale_x_continuous(breaks = c(-180,-60, -30, 0, 30, 60, 180)) + 
  scale_y_continuous(breaks = c(-90, -45, 0, 45, 90)) + 
  theme_light() +
  labs(title="", x="", y="")

ggsave(filename="output/world.pdf", 
       plot = r, 
       device = cairo_pdf, 
       width = 297, 
       height = 210, 
       units = "mm")


directory <- "output"
file.list <- paste(directory, "/",list.files(directory, pattern = "*.pdf"), sep = "")

lapply(file.list, FUN = function(files) {
  pdf_convert(files, format = "jpeg", dpi=300)
})

azores <-  rasterGrob(as.raster(readJPEG("azores_1.jpeg")), interpolate = FALSE)
madeira <-  rasterGrob(as.raster(readJPEG("madeira_1.jpeg")), interpolate = FALSE)
world <-  rasterGrob(as.raster(readJPEG("world_1.jpeg")), interpolate = FALSE)

pdf("output/maps.pdf", width = 8, height = 12, paper = "A4") # Open a new pdf file
grid.arrange(azores, madeira, world ,ncol = 1, nrow=3)
dev.off() #


pdf("output/maps_2.pdf", width = 8, height = 12, paper = "A4") # Open a new pdf file
grid.arrange(arrangeGrob(madeira,azores, ncol=2), world,  ncol=1, heights=c(0.5, 0.5))
dev.off() #


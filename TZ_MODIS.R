library(sf)
library(raster)
library(here)
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggsn)

#Get the shapefile of Tanzania 
map_b<-st_read("D:/Library/MODIS/TZ/Tanzania_adm0.shp")

modis_data<- raster("D:/Library/MODIS/TZ/modis12.tif")

#Projecting the raster
modis_data <- projectRaster(modis_data, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Cropping data
modis_data <- raster::mask(modis_data, as_Spatial(map_b))

# Converting the raster object into a data frame and converting the IGBP classification into a factor
modis_df <- as.data.frame(modis_data, xy = TRUE, na.rm = TRUE) %>%
  mutate(modis12 = as.factor(round(modis12)))
rownames(modis_df) <- c()

# Renaming IGBP classification levels
levels(modis_df$modis12) <- c("Dense Vegetation",
                            "Shrubs",
                            "Grass Land",
                            "Permanent Wetlands",
                            "Croplands",
                            "Built-Up Areas",
                            "Bare Land",
                            "Water Bodies")


#Aes
colors <- c("#004d25", "#00db00", "#8c9938", 
            "#3624a8", "#e39636","#c4291c", 
            "#8F4C0F", "#0080ff")

# Visualising using ggplot2
p<-ggplot() + 
  geom_tile(data = modis_df,aes(x = x, y = y, fill =modis12)) +
  geom_sf(data = map_b, inherit.aes = FALSE, fill = NA) +
  scale_fill_manual(values=colors)+
  labs(title="LAND COVER IN TANZANIA",
       subtitle ="01/01/2019 - 31/12/2019",fill="Legend")+
  theme_minimal()+
  theme(
    legend.position ="right",
    legend.key.width = unit(1, "line"),
    legend.key.height = unit(4.5, "line"),
    legend.title = element_text(color = "#102f2f", lineheight = .5, 
                                hjust = 0.5, face = "bold", size=24),
    legend.text=element_text(size = 22),
    axis.title =element_blank(),
    axis.line = element_line(linetype = "solid"),
    axis.ticks =element_line(),
    axis.text.y=element_text(size=18,angle=90, hjust = 0.5),
    axis.text.x = element_text(size = 18),
    plot.title=element_text(size =30, hjust=0.5, face ="bold", color="#102f2f"),
    plot.subtitle =element_text(size =25, hjust=0.5, color="#102f2f", face = "italic"),
  )

ggdraw(p) + 
  draw_label(x = 0.97, y = 0.025, label = "Data: MODIS Land Cover\n Reginald Faustus S\n +255(0)754284641", 
             hjust = 1,size = 14)
  
ggsave("~/tz.jpg", width = 21, height = 20, dpi= 300)










#********************************
#Author: Reginald Faustus S.
#Phone: +255(0)754284641
#Email: bwanamuki@hotmail.com 
#Twitter: @bwanamuki
#github: github.com/bwanamuki
#********************************

library(tidyverse)
library(raster)
library(ggplot2)
library(sf)
library(cowplot)

#importing Bukoba Urban Shapefile
boundary<-st_read("D:/PRACTICALS/Kagera_Shapefiles/bukoba_urban.shp")

#Import classified Bukoba Urban Landsat image
bukoba <- raster("D:/PRACTICALS/Outputs/kagera_img/bukoba.tif")

## Converting the raster object into a dataframe
bk <- as.data.frame(bukoba, xy = TRUE, na.rm = TRUE) %>% 
  rename(value="bukoba") %>% 
  mutate(value=as.factor(value))
rownames(bk) <- c()

#Renaming bk classification levels
levels(bk$value) <- c( "Dense Vegetation",
                        "Built-Up",
                        "Water Bodies",
                        "Bare Land",
                        "Sparse Vegetation")

#Aes
colors <- c("#387d4a", "#c4291c", "#409cde", "#e39636", "#87b054")

#Visualizing using ggplot
p<-ggplot() + 
    geom_tile(data = bk,aes(x = x, y = y, fill = value)) +
    geom_sf(data = boundary, inherit.aes = FALSE, fill = NA) +
    scale_x_continuous(breaks =seq(364000,373000,by=3000))+
    scale_y_continuous(breaks =seq(9848000,9861000,by=3000))+
    coord_sf(datum = st_crs(boundary))+
    scale_fill_manual(values=colors)+
    labs(title="BUKOBA URBAN",
         subtitle ="2021 Land Cover (LandSat 8)",fill="Land Cover Types")+
    theme_minimal()+
    theme(
      legend.position ="right",
      legend.key.width = unit(1, "line"),
      legend.key.height = unit(4.5, "line"),
      legend.title = element_text(color = "#102f2f", lineheight = .5, 
                                  hjust = 0.5, face = "bold", size=14),
      legend.text=element_text(size = 12),
      axis.title =element_blank(),
      axis.line = element_line(linetype = "solid"),
      axis.ticks =element_line(),
      axis.text.y=element_text(angle=90, hjust = 0.5),
      plot.title=element_text(size =20, hjust=0.5, face ="bold", color="#102f2f"),
      plot.subtitle =element_text(size =16, hjust=0.5, color="#102f2f", face = "italic"),
      )

ggdraw(p) + 
  draw_label(x = 0.97, y = 0.04, label = "Created by: Reginald Faustus S\n bwanamuki@hotmail.com\n +255(0)754284641", 
             fontface ="italic",hjust = 1,size = 10)
  #theme(plot.background = element_rect(fill="white"))

#Saving the output in .png format
ggsave("~/bukoba.png", width = 9, height = 12, dpi = 300)

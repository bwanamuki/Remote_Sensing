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

#importing Bukoba Urban Shapefile
boundary<-st_read("D:/PRACTICALS/Kagera_Shapefiles/bukoba_urban.shp")

#Import classified Bukoba Urban Landsat image
bukoba <- raster("D:/PRACTICALS/Outputs/kagera_img/bukoba.tif")

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
ggplot() + 
    geom_tile(data = bk,aes(x = x, y = y, fill = value)) +
    geom_sf(data = boundary, inherit.aes = FALSE, fill = NA) +
    scale_fill_manual(values=colors)+
    labs(title="BUKOBA URBAN 2021 LAND COVER",
         subtitle ="Represented by 5 Land Cover Classes",
    fill="Land Cover Types", caption ="Prepared by: Reginald Faustus S.")+
    theme_void()+
    theme(
      legend.position = c(0.8, 0.21),
      legend.key.width = unit(2, "line"),
      legend.key.height = unit(3.5, "line"),
      legend.title = element_text(color = "#102f2f", lineheight = .5, 
                                  hjust = 0.5, face = "bold", size=25),
      legend.text=element_text(size = 22),
      plot.caption = element_text(colour = "#102f2f", hjust=0, size = 18, face = "italic"),
      plot.title=element_text(size =32, hjust=0.5, face ="bold", color="#102f2f"),
      plot.subtitle =element_text(size =25, hjust=0.5, color="#102f2f", face = "italic"),
      plot.background = element_rect(fill="#E9EBEE", color =NA))

#Saving the output in .png format
ggsave("~/bukoba.png", width = 10, height = 16, dpi = 300)
  
    
    
    
    
    
    
    
 

  
  
  
  










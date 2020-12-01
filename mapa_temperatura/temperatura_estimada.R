
#ggplot2
#geobr
#raster
#fields
#ggspatial

library(ggplot2)
library(geobr)
library(raster)
library(ggspatial)
library(fields)

setwd('C:/Users/bsanches/Downloads/mapa_temperatura')

dados.temp <- read.csv('dados/dados_temperatura.csv')

relevo <- raster('dados/relevo_minas_gerais.tif')

modelo <- lm(formula = temp~longitude+latitude+altitude, data=dados.temp)

names(dados.temp)

top.df <-as.data.frame(relevo,xy=TRUE)

top.df <- na.omit(top.df)

names(top.df) <- c('long','lat', 'alt')

temp.mg <- top.df

temp.mg$temp <- 23.49 - 0.25 * top.df$long + 0.48 * top.df$lat - 0.0053 * top.df$alt

temp.mg$temp.cat <- cut(temp.mg$temp, breaks=c(8,10,12,14,16,18,20,22,24,26), labels = c('8-10','10-12','12-14','14-16','16-18','18-20','20-22','22-24','24-26'))

mg <- read_state(code_state = 'MG')

ggplot(data = temp.mg) + 
  geom_raster(aes(x=long, y=lat, fill=temp.cat)) + 
  geom_sf(data = mg, fill='transparent') + 
  scale_fill_manual(values = tim.colors(9)) + 
  annotation_scale() +
  annotation_north_arrow()

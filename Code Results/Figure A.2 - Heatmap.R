
#Directorio
setwd("//wmedesrv/GAMMA/Christian Posso/_banrep_research/Proyectos_finalizados_replicacion/Project The Luck of the Draw/Reproduction - AN")

#Librerias
library(sf)
library(haven)
library(dplyr)
library(ggplot2)

#Shapefile
shp  <- st_read("Data/Anonimizada/Shp municipios/MGN_MPIO_POLITICO.shp")

#Data
data <- read_dta("Data/poblacion_munic.dta")

#Merge
data_merge <- left_join(shp,data,by="MPIO_CCNCT")

#Map
mapa <- ggplot(data = data_merge) +
  geom_sf(aes(fill = poblacion), color = "white") +  # Añadir bordes negros para mayor visibilidad
  scale_fill_gradient(
    na.value = "#FFE4B5",    # Color dorado para valores NA
    low = "gray90",       # Color gris claro para valores bajos
    high = "gray40",      # Color negro para valores altos
    name = "Population",    # Etiqueta para la escala de colores
    limits = c(0,3)       # Ajustar la barra de colores de 0 a 3
  ) +
  theme_void() +       # Usa un tema vacio
  labs(
    title = "",
    fill = "Population"
  )
mapa
ggsave("Results/Figure A2.png", plot = mapa, width = 10, height = 7, dpi = 300)


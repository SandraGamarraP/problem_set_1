##############################
# Punto 2
##############################


# Cargar paquete necesarios -----------------------------
# install.packages('pacman')
require(pacman)
p_load(tidyverse,rvest)


# Cargar datos ------------------------------------------

# Crear tabla vacía para añadir datos
datos<-tibble()
# Extraer tabla de cada data chunk y unirlo en una sola tabla
for (i in 1:10){
  
  #crear link para descargar datos de cada uno de los 10 data chunks
  link <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")

  #descargar los datos del data chunk actual en una tabla
  my_table <- read_html(link) %>% html_table()

  #Unir información de cada nuevo data chunks con la información ya guardada
  datos <- bind_rows(datos,my_table)
}


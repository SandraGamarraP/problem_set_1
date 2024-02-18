##############################
# Punto 2
##############################


# Cargar paquete necesarios -----------------------------
# install.packages('pacman')
require(pacman)
p_load(tidyverse,rvest)


# Cargar datos ------------------------------------------

# Reporte de "Medición de Pobreza Monetaria y Desigualdad" de GEIH Bogotá 2018
my_url = "https://ignaciomsarmiento.github.io/GEIH2018_sample/" 
# browseURL(my_url) # confirmar que la página si es la que se busca

my_html = read_html(my_url)

# Traer los elementos del código HTML respecto de la lista de data chunks de la página 
sub_html <- my_html %>% html_nodes(xpath = '/html/body/div/div/div[2]/ul/li') %>% html_elements("a")

# Sacar la información de links y nombre de cada data chunk
title <- sub_html %>% html_text()
refs <- sub_html %>% html_attr("href")

# Añadir el resto del link extraidos del data chunk
db <- tibble(title,url=paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/",refs))

# Extraer tabla de Data chunk 1
link <- filter(db,title==" Data chunk 1")$url
my_table <- read_html(link) %>% html_table() ## falla por carga dinámica de la tabla 


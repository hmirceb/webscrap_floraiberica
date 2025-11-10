library(rvest)
library(httr)
library(tidyverse)
rm(list = ls())

# URLS base para generar el resto de enlaces
fi_base_url <- "http://www.floraiberica.es/PHP/"
genera_list_url <- "http://www.floraiberica.es/PHP/generos_lista.php"

# Lee el desplegable de la web con la lista de generos
genus_list <- read_html(genera_list_url) %>%
  html_element(".td_encabezado > select:nth-child(1)") %>%
  html_text2()

generos <- genus_list %>%
  # Separa las palabras por las letras mayusculas
  gsub('\\B([[:upper:]])', ' \\1', .) %>%
  # Separa por los espacios para obtener un vector
  strsplit(., " ") %>%
  # Convierte la lista a un vector
  unlist(.)
# Quitamos los tres primeros porque corresponden al texto del desplegable
generos <- generos[-c(1:3)]

gen_list <- list()
for(gen in seq_along(generos)) {

  print(gen)
  # Genera una URL para un genero de la lista
  temp_genus_url <- paste0("http://www.floraiberica.es/PHP/cientificos_.php?gen=",
                           generos[gen])

  # Lee el codigo HTML de la pagina del genero
  genus_page <- read_html(temp_genus_url)

  # Extrae la familia del genero
  familia <- genus_page %>%
    html_element(".menu > i:nth-child(1)") %>%
    html_text2()

  # Extrae las URLs de las especies del genero
  genus_urls <- genus_page %>%
    html_elements(".fondo a") %>%
    html_attr('href')

  if (length(genus_urls) == 0) {
    gen_list[[gen]] <- data.frame(familia = NA,
                                 genero = generos[gen],
                                 accepted_taxa = NA,
                                 taxa = NA,
                                 synonym = NA)
  } else {
    # Bucle para extraer en cada especie. Como se hace con foreach,
    # y ya estamos dentro de otro bucle foreach, no hacemos nada porque
    # el resultado ya ira directo al bucle superior
    sps_list <- list()
    for(sp in seq_along(genus_urls)) {
      # Genera la URL de la especie
      temp_sp_url <- paste0(fi_base_url, genus_urls[[sp]])

      # Extrae el genero de la URL
      genus <- str_split_i(temp_sp_url, "gen=", 2)
      genus <- str_split_i(genus, "\\&", 1)

      # Lee el HTML de la pagina
      species_page <- read_html(temp_sp_url)

      # Saca el nombre aceptado de la pagina
      acc_species <- species_page %>%
        html_elements(".td_encabezado i") %>%
        html_text2() %>%
        paste(collapse = " ")

      # Lee HTML para la especie
      sp_html <- species_page %>%
        html_element("#cuerpo > table:nth-child(1)") %>%
        html_elements("td dt")

      # Extrae tabla con los nombres
      sp_names <- sp_html%>%
        html_text()

      # Extrae tipo de sinonimo en funcion del color de la fuente
      # usada en FI
      # "#800000" (rojo) -> homotipico
      # "#008000" (verde) -> heterotipico
      synonym_type <- sapply(sp_html[-1], # el primero es el aceptado asi que lo ignora
                             function(x){
        a <- unlist(strsplit(as.character(x), split = "\"")) # Parte en funcion de las comillas " que delimitan el atributo color
        b <- a[which(grepl("#", a))[1]] # busca en que elementos hay una almohadilla # que indica el color HTML
        return(b)
      })
      synonym_type <- unlist(c(NA, synonym_type)) # Para que cuadre ponemos un NA al principio correspondiente al nombre aceptado

      if (length(sp_names) == 0) {
        sp_names <- acc_species
      }
      print(paste(gen, sp, sep = "."))

      # Juntamos en un dataframe
      sps_list[[sp]] <- data.frame(familia = familia,
                                   genero = genus,
                                   accepted_taxa = acc_species,
                                   taxa = sp_names,
                                   synonym = synonym_type)
    }
    sps_df <- do.call("rbind", sps_list)
    gen_list[[gen]] <- sps_df
  }
}
fi_names_temp <- do.call("rbind", gen_list)

save(fi_names_temp, file = "temp/fi_names_temp.RData")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# R Script to clean up the data scraped from Flora Iberica #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

library(tidyverse)
library(stringr)
rm(list = ls())

# Load raw scraped data and auxiliary functions
load("temp/fi_names_temp.RData")
source("R/utils.R")

# Quitamos los subgeneros (cuando la columna accepted_taxa tiene dos)
# palabras que empiezan por mayuscula
fi_names_temp <- fi_names_temp %>%
  # Separamos la columna accepted_taxa
  separate(accepted_taxa,
           sep = " ",
           into = c("a", "b"),
           remove = FALSE) %>%
  # Nos quedamos con las que la primera tenga mayuscula y la segunda no
  filter(grepl("(^[[:upper:]])", a) &
           !grepl("(^[[:upper:]])", b)) %>%
  dplyr::select(-c(a, b))

# Ponemos el nivel taxonomico del taxon
# \\b indica un espacio en blanco y \\. un punto (\\ es un escaper porque son caracteres reservados)
fi_names_temp <- fi_names_temp %>%
  mutate(taxonomic_level = case_when(grepl("\\bf\\.", taxa) ~ "forma",
                                         grepl("\\bvar\\.", taxa) ~ "variedad",
                                         grepl("\\bsubvar\\.", taxa) ~ "subvariedad",
                                         grepl("\\bsubsp\\.", taxa) ~ "subespecie",
                                     genero == accepted_taxa ~ "genero",
                                     .default = "especie")) %>%
  mutate(taxonomic_level = case_when(grepl(" x ", taxa) ~ "hibrido",
                                     .default = taxonomic_level))

# Use eidos_clean_names()
fi_names_temp <- fi_names_temp %>%
  mutate(taxa2 = sapply(taxa, eidos_clean_names))

# Separate clean name into words and remove those words from full name
# to get the authority
fi_names_temp2 <- fi_names_temp %>%
  separate(taxa2, into = c("a", "b", "c"), sep = " ", remove = FALSE) %>%
  mutate(taxonomic_level = case_when(grepl("\\[\\?\\]", taxa) ~ "[?]",
                                     .default = taxonomic_level)) %>%
  mutate(authority = trimws(str_replace_all(pattern = a,
                                 replacement = "",
                                 string = taxa))) %>%
  mutate(authority = ifelse(taxonomic_level != "genero",
                        str_replace_all(pattern = b,
                                        replacement = "",
                                        string = authority),
                        authority)) %>%
  mutate(authority = ifelse(taxonomic_level %in% c("subespecie", "variedad", "forma", "[?]"),
                        str_replace_all(pattern = c,
                                        replacement = "",
                                        string = authority),
                        authority)) %>%
  mutate(authority = str_replace_all(authority,
                                 "f.",
                                 "")) %>%
  mutate(authority = str_replace_all(authority,
                                 "var.",
                                 "")) %>%
  mutate(authority = str_replace_all(authority,
                                 "subsp.",
                                 "")) %>%
  mutate(authority = str_remove_all(authority, "\\[\\?\\]")) %>%
  mutate(authority = trimws(eidos_clean_whitespaces(authority)))

# Remove authority for hybrids and create new column with name
# and form
fi_names_temp2 <- fi_names_temp2 %>%
  mutate(authority = case_when(taxonomic_level == "hibrido" ~ NA,
                           authority == "" ~ NA,
                           .default = authority)) %>%
  mutate(taxa4 = case_when(taxonomic_level == "forma" ~ paste(a, b, "f.", c),
                           taxonomic_level == "variedad" ~ paste(a, b, "var.", c),
                           taxonomic_level == "subespecie" ~ paste(a, b, "subsp.", c),
                           .default = taxa2)) %>%
  dplyr::select(-c(a, b, c))

# Turn synonym type color codes into words
fi_names_temp2 <- fi_names_temp2 %>%
  mutate(synonym_type = case_when(synonym == "#008000" ~ "heterotipico",
                                  synonym == "#800000" ~ "homotipico",
                                  .default = "aceptado")) %>%
  mutate(taxa = eidos_clean_whitespaces(taxa))

# Genera in hybrids appears as A. instead of the full name,
# fix that:
fi_names_temp2 <- fi_names_temp2 %>%
  mutate(hyb_gen = ifelse(taxonomic_level == "hibrido",
                          str_split_i(taxa, pattern = " ", i = 1),
                          "Aknkdfigjdfkg")) %>% # random word to avoid substituting good genera
  mutate(across(c(accepted_taxa, taxa, taxa2, taxa4),
                \(x) str_replace_all(x,
                                     pattern = hyb_gen,
                                     replacement = genero))) %>%
  mutate(taxa2 = ifelse(taxonomic_level == "hibrido",
                        taxa,
                        taxa2),
         taxa4 = ifelse(taxonomic_level == "hibrido",
                        taxa,
                        taxa4))

# Remove abbreviations from canonical names
fi_names_temp2 <- fi_names_temp2 %>%
  mutate(taxa2 = str_replace_all(taxa2,
                                      " f.",
                                      "")) %>%
  mutate(taxa2 = str_replace_all(taxa2,
                                      " var.",
                                      "")) %>%
  mutate(taxa2 = str_replace_all(taxa2,
                                      " subsp.",
                                      "")) %>%
  mutate(accepted_taxa = str_replace_all(accepted_taxa,
                                 " f.",
                                 "")) %>%
  mutate(accepted_taxa = str_replace_all(accepted_taxa,
                                 " var.",
                                 "")) %>%
  mutate(accepted_taxa = str_replace_all(accepted_taxa,
                                 " subsp.",
                                 ""))

### FALTA
# revisar las que salen sin autoria pero si que la tienen
# sacar informacion taxonomica superior de los generos from WFO o POWO,
# incluyendo los generos que no estan completos en FI y si familia es NA


FI_clean <- fi_names_temp2 %>%
  dplyr::select(familia, genero, taxonomic_level, taxa, taxa4, authority, taxa2,
                accepted_taxa, synonym_type) %>%
  rename(nombre = taxa4,
         nivel_taxonomico = taxonomic_level,
         nombre_completo = taxa,
         nombre_canonico = taxa2,
         nombre_aceptado = accepted_taxa,
         tipo_sinonimo = synonym_type,
         autoridad = authority) %>%
  # Remove some NA in strings and wierd symbols
  mutate(across(everything(), \(x) trimws(str_remove_all(x, "NA")))) %>%
  mutate(across(everything(), \(x) trimws(str_remove_all(x, "€"))))

# Write CSV
write.csv(FI_clean,
          "thesaurus/Flora_iberica_thesaurus.csv")

# Compress CSV to save space
zip(zipfile = "thesaurus/Flora_iberica_thesaurus.zip",
    files = "thesaurus/Flora_iberica_thesaurus.csv")

# Remove CSV
file.remove("thesaurus/Flora_iberica_thesaurus.csv")

library(tidyverse)
library(stringr)
rm(list = ls())

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
                                     genero == accepted_taxa ~ "genus",
                                     .default = "species")) %>%
  mutate(taxonomic_level = case_when(grepl(" x ", taxa) ~ "hybrid",
                                     .default = taxonomic_level))

# Usamos la funcion eidos_clean_names()
fi_names_temp <- fi_names_temp %>%
  mutate(taxa2 = sapply(taxa, eidos_clean_names))

fi_names_temp2 <- fi_names_temp %>%
  separate(taxa2, into = c("a", "b", "c"), sep = " ", remove = FALSE) %>%
  mutate(authorship = trimws(str_replace_all(pattern = a,
                                 replacement = "",
                                 string = taxa))) %>%
  mutate(authorship = ifelse(taxonomic_level != "genus",
                        str_replace_all(pattern = b,
                                        replacement = "",
                                        string = authorship),
                        authorship)) %>%
  mutate(authorship = ifelse(taxonomic_level %in% c("subespecie", "variedad", "forma"),
                        str_replace_all(pattern = c,
                                        replacement = "",
                                        string = authorship),
                        authorship)) %>%
  mutate(authorship = str_replace_all(authorship,
                                 "f.",
                                 "")) %>%
  mutate(authorship = str_replace_all(authorship,
                                 "var.",
                                 "")) %>%
  mutate(authorship = str_replace_all(authorship,
                                 "subsp.",
                                 "")) %>%
  mutate(authorship = trimws(eidos_clean_whitespaces(authorship)))

fi_names_temp2 <- fi_names_temp2 %>%
  mutate(authorship = case_when(taxonomic_level == "hybrid" ~ NA,
                           authorship == "" ~ NA,
                           .default = authorship)) %>%
  mutate(taxa4 = case_when(taxonomic_level == "forma" ~ paste(a, b, "f.", c),
                           taxonomic_level == "variedad" ~ paste(a, b, "var.", c),
                           taxonomic_level == "subespecie" ~ paste(a, b, "subsp.", c),
                           .default = taxa2)) %>%
  dplyr::select(-c(a, b, c))

fi_names_temp2 <- fi_names_temp2 %>%
  mutate(synonym_type = case_when(synonym == "#008000" ~ "heterotipico",
                                  synonym == "#800000" ~ "homotipico",
                                  .default = "aceptado")) %>%
  mutate(taxa = eidos_clean_whitespaces(taxa))

fi_names_temp2 <- fi_names_temp2 %>%
  mutate(hyb_gen = paste0(str_sub_all(genero, 1, 1), "\\.")) %>%
  mutate(across(c(accepted_taxa, taxa, taxa2, taxa4),
                \(x) str_replace_all(x,
                                     pattern = hyb_gen,
                                     replacement = genero)))

### FALTA
# revisar las que salen sin autoria pero si que la tienen
# las que tienen esto: [?] porque podrian lo que va depsues aparece en la autoria y no deberia
# sacar informacion taxonomica superior de los generos from WFO o POWO,
# incluyendo los generos que no estan completos en FI y si familia es NA


FI_clean <- fi_names_temp2 %>%
  dplyr::select(familia, genero, taxa, taxa4, authorship, taxa2,
                taxonomic_level, accepted_taxa, synonym_type) %>%
  rename(name = taxa4,
         full_name = taxa,
         canonical_name = taxa2,
         accepted_name = accepted_taxa)

# Write CSV
write.csv(FI_clean,
          "thesaurus/Flora_iberica_thesaurus.csv")

# Compress CSV to save space
zip(zipfile = "thesaurus/Flora_iberica_thesaurus.zip",
    files = "thesaurus/Flora_iberica_thesaurus.csv")

# Remove CSV
file.remove("thesaurus/Flora_iberica_thesaurus.csv")

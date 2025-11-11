#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Funciones auxiliares basadas en el paquete eidosapi #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Autor: Hector Miranda Cebrian
#

eidos_clean_whitespaces = function(x) {
  # Substitute Unicode whitespaces with normal whitespaces
  x = gsub("\\p{Zs}+", " ", x, perl = TRUE)

  # Remove zero-width spaces
  x = gsub("\u200B", "", x)

  # Remove double whitespaces
  x = gsub("\\s+", " ", x, fixed = TRUE)

  return(x)
}

eidos_clean_names = function(taxa_names){

  # Remove underscores if any
  taxa_names = gsub(pattern = "_", replacement = " ", x = taxa_names)

  # Remove UNICODE whitespaces, doble whitespaces and zero-width spaces
  taxa_names = eidos_clean_whitespaces(taxa_names)

  # Remove anything between parentheses
  taxa_names = gsub("\\(.*?\\)", "", taxa_names)

  # Remove anything between brackets
  taxa_names = gsub("\\[.*?\\]", "", taxa_names)

  # Split by whitespaces
  taxa_split = unlist(strsplit(taxa_names, split = " "))

  # Detect "subsp.", "var.", words starting in capital letter and
  # non letter characters associated with
  # authorities except "-"
  indice1 = which(grepl("(^[[:upper:]])|&|[^A-Za-z-]", taxa_split))

  # Look for letters with umlaut "ö"
  indice2 = which(grepl("^[[\U00E0-\U017F]]", taxa_split))

  # Look for letters with dot
  indice3 = which(grepl("\\.", taxa_split))

  # Detect common words in names and authorities that
  # might go undetected in previous checks
  indice4 = which(taxa_split %in% c("ex", "et", "in",
                                    "y", "zur", "sensu",
                                    "pro", "parte", "de",
                                    "la", "non", "da", "nud",
                                    "del", "von", "van", "der", "auct",
                                    "den", "and", "-", "degli",
                                    "en", "prensa", "subst",
                                    "var", "species", "unrecognised", "x",
                                    "subsp.", "var.", "subsp",
                                    "e", "du", "di", "des",
                                    "nec", "f", "illeg",
                                    "delle", ""))

  # Join indices
  indices = unique(c(indice1, indice2, indice3, indice4))[-1]

  # Collapse into full name:
  # The [-1] excludes the genus because they always start with capital letter
  if(length(indices) == 0){ # If the name did not meet any of the above criteria it returns 0 and leads to errors
    taxa_names_clean = paste(taxa_split,
                             collapse = " ")
  }else{
    taxa_names_clean = paste(taxa_split[-indices][1:3], # The [1:3] ensures that only the first three words corresponding to genus, species, subspecies are used
                             collapse = " ")
  }

  # Remove any NAs in strings derived from last step:
  taxa_names_clean = gsub(
    pattern = "NA",
    replacement = "",
    x = taxa_names_clean)

  # Remove any extra whitespace just in case:
  taxa_names_clean = gsub(pattern = "\\s+", replacement = " ", x = taxa_names_clean)
  taxa_names_clean = trimws(taxa_names_clean)

  return(taxa_names_clean)

}

split_by_second_capital <- function(text) {
  # Find all capitalized words containing only letters
  matches <- str_locate_all(text, "\\b[A-Z][a-zA-Z]+\\b")[[1]]

  # If fewer than 2 matches, just return the original text
  if (nrow(matches) < 2) {
    return(c(str_trim(text)))
  }

  # Find the start of the second match
  split_point <- matches[2, "start"]

  # Split and trim whitespace
  part1 <- str_trim(substr(text, 1, split_point - 1))
  part2 <- str_trim(substr(text, split_point, nchar(text)))

  return(c(part1, part2))
}

get_species <- function(string){
  s <- eidos_clean_whitespaces(string)
  ss = unlist(strsplit(s, " "))
  sss = ss[-which(grepl("[^a-zA-Z]", ss))]

  g_i <- which(grepl("[A-Z]", sss))
  sss[g_i] <- paste0("_", sss[g_i])
  sss <- paste(sss, collapse = " ")

  # sps <- list()
  # for (i in seq_along(g_i)) {
  #   start <- i
  #   end <- ifelse(i >= length(g_i),
  #                 length(sss),
  #                 i+1)
  #   sps[[i]] <- paste(sss[g_i[start]:end], collapse = " ")
  # }
  return(sss)
}

remove_doublespaces <- function(text){
  a1 <- unlist(strsplit(text, split = " "))
  b1 <- unlist(lapply(a1, trimws))
  c1 <- b1[b1 != ""]
  d1 <- paste(c1, collapse = " ")
  d1 <- trimws(d1)
  return(d1)
}

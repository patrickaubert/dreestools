#' Récupère la liste de tous les fichiers data.drees
#'
#' La fonction retourne un tableau contenant le nom et les métadonnées de tous les fichiers data.drees
#' diffusés par la DREES. Ces fichiers sont plus nombreux que les jeux de données, car un même
#' jeu de données peut contenir plusieurs fichiers (.xlsx, .csv, .zip, etc.)
#'
#' Si une liste de rubriques ou de producteurs est indiquée, seuls les fichiers de ces rubriques ou producteurs seront retenus.
#'
#' @param rubriques une liste de rubriques (par défaut toutes les rubriques disponibles)
#' @param producteurs une liste de producteurs (par défaut tous les producteurs disponibles)
#'
#' @return
#' @export
#'
#' @examples liste_datadrees(rubriques = c("retraite"))
#' @examples liste_datadrees(producteurs = c("OSOL"))
liste_datadrees <- function(rubriques = c(), producteurs = c()) {

  # récupère la liste des jeux de données de l'entrepot data.drees
  jdd <- read.csv2(file = "https://data.drees.solidarites-sante.gouv.fr/api/v2/catalog/exports/csv?delimiter=%3B&lang=fr",
                   fileEncoding = "UTF8")
  # restreint aux rubriques ou producteurs souhaités
    # on définit d'abord une fonction auxiliaire décomposant les thèmes multiples en un vecteur des thèmes
  scinde <- function(var) {
    var %>%
      strsplit(split=" & | et |, |,|/" ) %>%
      unlist() %>%
      tolower() %>%
      str_replace("[[:punct:]]"," ")
  }
    # on filtre la table si une liste limitative de producteurs ou de thèmes a été paramétrée par l'utilisateur
  if (NROW(rubriques)>=1) {
    jdd <- jdd %>%
      group_by(datasetid) %>%
      filter(NROW(intersect(scinde(rubriques),scinde(default.theme)))>=1) %>%
      ungroup()
  }
  if (NROW(producteurs)>=1) {
    jdd <- jdd %>%
      group_by(datasetid) %>%
      filter(NROW(intersect(scinde(producteurs),scinde(default.publisher)))>=1) %>%
      ungroup()
  }

  getodsfile <- function(i) {
    path <- paste0("https://data.drees.solidarites-sante.gouv.fr/explore/dataset/",jdd$datasetid[i],"/information/")
    page <- read_html( url( path ) )
    chemins <- page %>%
      html_nodes(".main--dataset .ods-app-explore-dataset") %>%
      html_attrs()
    elements <- strsplit(chemins[[1]][["ctx-dataset-schema"]], split="\\\"") %>%  unlist()
    elements <- elements[grepl("^odsfile",elements)]
    elements <- gsub("odsfile://","",elements)
    return(
      jdd[i,] %>%
        slice(rep(1:n(), each = NROW(elements) )) %>%
        mutate(fichiersdatadrees = elements)
    )
  }

  if (nrow(jdd)<1) {stop("Aucun fichier data.drees ne correspond aux critères")}

  tab <- do.call("bind_rows",lapply(1:nrow(jdd),getodsfile))

  tab <- tab %>%
    mutate(typefichier = str_extract(fichiersdatadrees,"(?<=\\.)[[:alnum:]]+$"))

  return(tab)
}

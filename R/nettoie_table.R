#' Met en forme une table de donnée
#'
#' Cette fonction traite et nettoie une table de données lue dans un onglet d'un fichier Excel
#' diffusé sous data.drees. Elle repère les informations correspondant aux intitulés des colonnes
#' et celle correspondant à des variables de catégories.
#'
#' Cette fonction auxiliaire est notamment utilisée dans la fonction extrait_onglet.
#'
#' @param tab une table de données extraites d'un onglet d'un fichier excel data.drees
#' @param forme_longue indique si les intitulés de variables en colonnes doivent être remis en lignes
#'
#' @return une table de données plus propre
#' @export
nettoie_table <- function(tab,
                          forme_longue = TRUE) {

  # == nombre de lignes du titres :
  # par défaut, on considère que seule la première ligne correspond à des titres de colonne,
  # mais si les lignes suivantes ne contiennent pas de données chiffrées, ou bien si elles contiennent uniquement des années ou des classes d'âge, on considère qu'il s'agit aussi de compléments de titre

  # une fonction qui détermine ce qu'on considère comme pouvant être un "titre" (ie un intitulé) de colonne
  posstitre <- function(x){
    !grepl("^[[:digit:]]+",x) |              # valeur non numérique
      grepl("^(19|20)[[:digit:]]{2}",x)  |   # année
      grepl("^[[:digit:]]+ (à |a |an)",x)    # classe d'âge
  }

  # calcule du nombre de lignes du titre
  nlignetitre <- 1
  while ((sum(posstitre(tab[nlignetitre+1,])) == ncol(tab)) &
         (nlignetitre+1 <= nrow(tab))) {
    nlignetitre <- nlignetitre+1
  }

  # == extraction et traitements des lignes de titre de colonnes

  # une fonction auxiliaire pour traiter les valeurs manquantes issues de la (non) lecture des cases fusionnées dans Excel
  completetitres <- function(t) {
    if (ncol(t)==1) {return(t)}
    t <- as.data.frame(t)
    for (i in 2:ncol(t)) {
      t[,i] <- ifelse(is.na(t[,i]) & !is.na(t[,i-1]),"Ensemble",t[,i])
    }
    for (i in 1:(ncol(t)-1)) {
      for (j in 2:nrow(t)) {
        t[j,i] <- ifelse(is.na(t[j,i]),t[j-1,i],t[j,i])
      }
    }
    return(t)
  }

  # traitements des noms de colonnes
  namesinit <- names(tab)
  titres <- tab[1:nlignetitre,]
  if (nlignetitre>1) {
    #for (i in 1:(nlignetitre-1)) {
    #  rempl <- NA
    #  for (j in 1:ncol(titres)) {
    #    rempl <- ifelse(!is.na(titres[i,j]),titres[i,j],rempl)
    #    titres[i,j] <- ifelse(is.na(titres[i,j]),rempl,titres[i,j])
    #  }
    #}
    titres <- as.data.frame(t(completetitres(t(titres))))
    titres <- t(titres %>% summarise_all(function(x){paste(x,collapse="###")}))
  }
  titres <- as.character(titres) %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_replace_all("###NA","") %>%
    stringr::str_replace_all("\\\n","") %>%
    trimws() %>%
    stringr::str_replace_all("[[:space:]]+","_") %>%
    tolower()
  titres <- ifelse(!is.na(titres) & titres!="na",titres,namesinit)
  titres <- str_replace_all(titres,"^X(?=[[:digit:]])","categorie")
  # recherche d'éventuels doublons
  for (j in 2:NROW(titres)) {if (titres[j] %in% titres[1:(j-1)]) titres[j] <- paste0(titres[j],j)}
  # assigne les titres comme noms de colonnes
  tab <- tab[(nlignetitre+1):nrow(tab),]
  names(tab) <- titres

  # == on détermine maintenant le nombre de colonnes d'intitules

  ncoltitre <- 1
  while (( (sum(posstitre(tab[,ncoltitre+1])) == nrow(tab))  |
           (grepl("d[ée]partement|r[ée]gion|ann[ée]e|cat[ée]gori",names(tab)[ncoltitre+1])) )&
         (ncoltitre+1 <= ncol(tab))) {
    ncoltitre <- ncoltitre+1
  }

  # ... et on complète les intitulés si des valeurs sont manquantes (du fait de la non lecture des cases fusionnées en Excel)
  if (ncoltitre>1) {tab[,1:ncoltitre] <- completetitres(tab[,1:ncoltitre])}

  # == si c'est pertinent, on concatène les colonnes de type "categorie"

  varcateg <- names(tab)[grepl("^categorie",names(tab))]
  if (NROW(varcateg) == 1) { names(tab)[names(tab) %in% varcateg] <- "categorie" }
  if (NROW(varcateg) > 1) {
    if (identical(as.vector(rowSums(!is.na(tab[,varcateg]))),rep(1,nrow(tab)))) {
      categ <- tab[,varcateg] %>%
        mutate(rownum = 1:n()) %>%
        pivot_longer(cols=varcateg,names_to="typecat",values_to="categorie") %>%
        group_by(rownum) %>% mutate(niveau.categorie = 1:n() ) %>% ungroup() %>%
        filter(!is.na(categorie))
      tab <- tab %>%
        select(-c(varcateg)) %>%
        mutate(categorie = categ$categorie,
               niveau.categorie = categ$niveau.categorie)
    }
  }

  # == transforme les valeurs manquantes (de type "N.S.") en NA

  tab <- tab %>%
    mutate_all(function(x){ifelse(grepl("^([Nn][DdRrSsCc]|/|\\-)[[:space:]]*$",x),NA,x)}) %>%
    distinct()

  # == identifie les colonnes de valeurs

  ischiffre <- function(x) { is.na(x) | grepl("^[[:digit:]]+",x)}
  colvaleurs <- names(tab[,(colSums(tab %>% mutate_all(ischiffre)) == nrow(tab))])

  # on exclut des colonnes considérées comme contenant des valeurs la première colonne du tableau, et celles qui s'intitulent "catégorie", "département" ou "région"
  colvaleurs <- colvaleurs[!grepl("departement|region|categorie",colvaleurs)]
  colvaleurs <- colvaleurs[colvaleurs != names(tab)[1]]

  autrescols <- names(tab)[!(names(tab) %in% colvaleurs)]

  # == mise du tableau en forme longue

  numerique <- function(x) {as.numeric(ifelse(grepl("^[[:digit:]]*,[[:digit:]]*$",x),gsub(",",".",x),x))}

  tab <- tab %>%  mutate_at(colvaleurs,numerique)

  if (forme_longue) {
    tab <- tab %>%
      pivot_longer(cols=names(tab)[(ncoltitre+1):NROW(names(tab))], names_to="intitules", values_to = "valeurs")
  }
  #if (sum(grepl("[^[:digit:]]",tab$valeurs))>0) {
  #  tab <- tab %>%
  #    mutate(info.valeurs = str_extract(valeurs,"[^[:digit:][:space:][:punct:]]*"),
  #           info.valeurs = ifelse(info.valeurs == "",NA,info.valeurs),
  #           valeurs = str_replace_all(valeurs,"[^[:digit:]]",""))
  #  if (sum(is.na(tab$info.valeurs)) == nrow(tab)) { tab <- tab %>% select(-info.valeurs)}
  #}
  #tab <- tab %>% mutate(valeurs = as.numeric(valeurs))

  # == reconnait certains types d'intitulés

  # années
  #if (sum(grepl("^(19|20)[[:digit:]]{2}",tab$intitules)) == nrow(tab)) {
  #  tab <- tab %>%  dplyr::rename(Annee = intitules)
  #  if (sum(grepl("[^[:digit:]]",tab$Annee))>0) {
  #    tab <- tab %>%
  #      mutate(info.annee = str_extract(Annee,"[^[:digit:][:space:][:punct:]]*"),
  #             info.annee = ifelse(info.annee == "",NA,info.annee),
  #             Annee = str_replace_all(Annee,"[^[:digit:]]",""))
  #    if (sum(is.na(tab$info.annee)) == nrow(tab)) { tab <- tab %>% select(-info.annee)}
  #  }
  #  tab <- tab %>% mutate(Annee = as.numeric(Annee))
  #}
  # sexe

  return(tab)
}

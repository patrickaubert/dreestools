#' Fonction extrayant le contenu d'un onglet d'un fichier Excel diffusé sur data.drees
#'
#' Cette fonction sert à extraire le contenu d'un onglet d'un fichier Excel diffusé sous data.Drees.
#' Le produit est une liste contenant un élément "tab" correspondant au tableau de données,
#' et divers éléments correspondant aux métadonnées ("source","note","champ", etc.)
#'
#' Nota : Cette fonction a initialement été dupliquée à partir de la fonction readSheetExcel du package asdep, mais elle intègre quelques améliorations.
#'
#' @param file nom du fichier Excel
#' @param sheet nom de l'onglet
#'
#' @return une liste contenant un tableau de donnée (élément "tab") et des métadonnées (éléments "intitule","numtab","source","champ", etc.)
#' @export
#'
#' @examples tab_et_infos <- extrait_onglet(file=pathdatadrees("549_la-prestation-de-compensation-du-handicap-pch","pch_et_actp_beneficiaires_par_sexe_et_age_montants_verses_donnees_2016.xlsx"), sheet="Tableau 1")
#' @examples tab_et_infos2 <- extrait_onglet(file=pathdatadrees("376_les-depenses-d-aide-sociale-departementale","les_depenses_daide_sociale_departementale_series_longues_1999_2018.xlsx"), sheet="Données nationales")
#' @examples tab_et_infos3 <- extrait_onglet(file=pathdatadrees("336_minima-sociaux-rsa-et-prime-d-activite","rsa_et_prime_d_activites_donnees_par_sexe_et_configuration_familiale_xlsx"), sheet="Tableau 2")
#' @examples tab4 <- extrait_onglet(file=pathdatadrees("619_indicateurs-financiers","fi09_fi10_isd_depenses_d_aide_a_l_hebergement_des_personnes_handicapees_en_etablissement_xlsx"), sheet="2017")$tab
#' @examples tab5 <- extrait_onglet(file=pathdatadrees("donnees-mensuelles-sur-les-prestations-de-solidarite","donnees_mensuelles_prestations_solidarite_janvier2021_mm_xlsx"), sheet="Tableau 1")$tab
extrait_onglet <- function(file,
                           sheet
                           ) {

  # ========================================
  # différents exemples pour les essais

  # file <- pathdatadrees("549_la-prestation-de-compensation-du-handicap-pch","pch_et_actp_beneficiaires_par_sexe_et_age_montants_verses_donnees_2017.xlsx")
  # sheet <- "Graphique 1"
  # file <- pathdatadrees("376_les-depenses-d-aide-sociale-departementale","les_depenses_daide_sociale_departementale_series_longues_1999_2018.xlsx")
  # sheet <- "Données nationales"
  # file <- pathdatadrees("336_minima-sociaux-rsa-et-prime-d-activite","rsa_et_prime_d_activites_donnees_par_sexe_et_configuration_familiale_xlsx")
  # sheet <- "Tableau 2"
  # file <- pathdatadrees("619_indicateurs-financiers","fi09_fi10_isd_depenses_d_aide_a_l_hebergement_des_personnes_handicapees_en_etablissement_xlsx")
  # sheet <- "2017"

  # ========================================
  # vérifications préliminaires
  #if (!(sheet %in% getSheetNames(file))) { stop("Erreur : onglet absent du fichier") }

  # ========================================
  # extraction des données de l'onglet
  tabcompl <- read.xlsx(xlsxFile = file, sheet = sheet,
                        colNames = FALSE,
                        skipEmptyRows = TRUE, skipEmptyCols = TRUE)
  if (ncol(tabcompl)<=1) { return(NULL) }

  # ========================================
  # initialisation de la liste de résultats
  result <- list(
    "fichiersource" = file,
    "ongletsource" = sheet
  )

  # ========================================
  # suppression des infos inutiles
  inutile <- c("^[[:punct:][:space:]]*([Rr]etour au s|[Rr]etour s|S)ommaire$",
               "^(R|r)etour en haut de page$")
  for (i in 1:NROW(inutile)) { tabcompl <- tabcompl %>% mutate_all(function(x){ifelse(grepl(inutile[i],tolower(x)),NA,x)}) }

  # ========================================
  # séparation des données : si une seule colonne est remplie, on considère qu'il s'agit de métadonnées ; sinon des données d'un tableau

  lignesremplies <- rowSums(!is.na(tabcompl))

  info <- tabcompl[(lignesremplies == 1),]
  tab <- tabcompl[(lignesremplies > 1),]

  # ========================================
  # lecture et traitement de la table de données

  tab <- tab[,(colSums(is.na(tab))<nrow(tab))]

  # == nombre de lignes du titres :
  # par défaut, on considère que seule la première ligne correspond à des titres de colonne,
  # mais si les lignes suivantes ne contiennent pas de données chiffrées, on considère qu'il s'agit aussi de compléments de titre

  nlignetitre <- 1
  while ((sum(!grepl("^[[:digit:]]+",tab[nlignetitre+1,])) == ncol(tab)) & (nlignetitre+1 <= nrow(tab))) {nlignetitre <- nlignetitre+1}

  # == extraction et traitements des lignes de titre de colonnes

  namesinit <- names(tab)
  titres <- tab[1:nlignetitre,]
  if (nlignetitre>1) {
    for (i in 1:(nlignetitre-1)) {
      rempl <- NA
      for (j in 1:ncol(titres)) {
        rempl <- ifelse(!is.na(titres[i,j]),titres[i,j],rempl)
        titres[i,j] <- ifelse(is.na(titres[i,j]),rempl,titres[i,j])
      }
    }
    titres <- t(titres %>% summarise_all(function(x){paste(x,collapse="###")}))
  }
  titres <- as.character(titres) %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_replace_all("###NA","") %>%
    stringr::str_replace_all("\\\n","") %>%
    stringr::str_replace_all("[[:space:]]+","_") %>%
    tolower()
  titres <- ifelse(!is.na(titres) & titres!="na",titres,namesinit)
  titres <- str_replace_all(titres,"^X(?=[[:digit:]])","categorie")
  # recherche d'éventuels doublons
  for (j in 2:NROW(titres)) {if (titres[j] %in% titres[1:(j-1)]) titres[j] <- paste0(titres[j],j)}
  # assigne les titres comme noms de colonnes
  tab <- tab[(nlignetitre+1):nrow(tab),]
  names(tab) <- titres

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

  tab <- tab %>%
    mutate_at(colvaleurs,numerique) %>%
    pivot_longer(cols=colvaleurs, names_to="intitules", values_to = "valeurs")
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

  # ========================================
  # lecture des métadonnées
  info <- info[,(colSums(is.na(info))<nrow(info))]
  info <- as.vector(t(info))
  info <- info[!is.na(info)]
  info <- unique(info) %>% trimws()

  rubriques <- data.frame(
    rubrique = c("intitule","note","source","champ","lecture"),
    txt = c("[Tt]ableau [[:alnum:]]+","[Nn]ote(s|)","[Ss]ource(s|)","[Cc]hamp","[Ll]ecture"),
    stringsAsFactors = FALSE
  )
  debut <- paste0("^(",paste(rubriques$txt,collapse="|"),")")
  left <- str_extract(info, paste0(debut,"(?=[[:space:]]*(\\:|\\-)*[[:space:]]*)"))
  right <- gsub(paste0(debut,"[[:space:]]*(\\:|\\-)*[[:space:]]*"),"",info)
  cat <- "info"
  for (i in 1:NROW(info)) {
    if (grepl(rubriques$txt[1],info[i])) { result[["tabnum"]] <- left[i] }
    for (j in 1:nrow(rubriques)) {
      if (grepl(rubriques$txt[j],left[i])) { left[i] <- rubriques$rubrique[j] }
      if (left[i] %in% rubriques[rubriques$rubrique != "intitule","rubrique"]) { cat <- left[i] }
    }
    if (is.na(left[i])) { left[i] <- cat }
  }
  infosdispo <- unique(left)
  for (k in 1:NROW(infosdispo)) {
    result[[infosdispo[k]]] <- paste(right[left==infosdispo[k]],collapse="\n")
  }

  # ========================================
  result$tab <- tab
  #result$containstab <- (nrow(result$tab)>0)
  return(result)
}

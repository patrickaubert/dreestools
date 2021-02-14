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
#' @examples tab6 <- extrait_onglet(file=pathdatadrees("4230_indicateurs-de-pauvrete-avant-et-apres-redistribution-de-niveau-de-vie-et-d","series_longues_d_indicateurs_de_pauvrete_xlsx"), sheet="Tableau 1a")$tab
extrait_onglet <- function(file,
                           sheet
                           ) {

  # ========================================
  # différents exemples pour les essais

  # file <- pathdatadrees("549_la-prestation-de-compensation-du-handicap-pch","pch_et_actp_beneficiaires_par_sexe_et_age_montants_verses_donnees_2017.xlsx")
  # sheet <- "Graphique 1"
  # sheet <- "Tableau 1"
  # file <- pathdatadrees("376_les-depenses-d-aide-sociale-departementale","les_depenses_daide_sociale_departementale_series_longues_1999_2018.xlsx")
  # sheet <- "Données nationales"
  # file <- pathdatadrees("336_minima-sociaux-rsa-et-prime-d-activite","rsa_et_prime_d_activites_donnees_par_sexe_et_configuration_familiale_xlsx")
  # sheet <- "Tableau 2"
  # file <- pathdatadrees("619_indicateurs-financiers","fi09_fi10_isd_depenses_d_aide_a_l_hebergement_des_personnes_handicapees_en_etablissement_xlsx")
  # sheet <- "2017"
  # file <- pathdatadrees("4230_indicateurs-de-pauvrete-avant-et-apres-redistribution-de-niveau-de-vie-et-d","series_longues_d_indicateurs_de_pauvrete_xlsx")
  # sheet <- "Tableau 1a"

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

  tab <- tab[,(colSums(is.na(tab))<nrow(tab))] %>%
    nettoie_table()

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

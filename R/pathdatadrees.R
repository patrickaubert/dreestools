#' Fonction auxiliaire retournant le chemin d'accès d'un fichier à télécharger sous data.drees
#'
#' @param jdd le nom du jeu de données
#' @param fichier le nom du fichier (.xlsx, .csv, etc.)
#'
#' @return le chemin d'accès du fichier sous data.Drees
#' @export
#'
#' @examples pathdatadrees("549_la-prestation-de-compensation-du-handicap-pch","pch_et_actp_beneficiaires_par_sexe_et_age_montants_verses_donnees_2016.xlsx")
pathdatadrees <- function(jdd,fichier) {
  paste0(
    "https://data.drees.solidarites-sante.gouv.fr/api/datasets/1.0/",
    jdd,
    "/attachments/",
    gsub("\\.","",fichier),
    "/)"
    )
}


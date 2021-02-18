#' Une fonction auxiliaires qui reconnait certains types de variables dans une table, et les renomme en conséquence
#'
#' A partir d'une table de données en entrée, cette fonction repère si les variables de la table
#' correspondent à certains types de donnée en particulier (année, sexe, territoire, etc) et,
#' si c'est bien le cas, change le nom de ces variables pour correspondre à ce type.
#'
#' @param tab une table de données
#' @param vars un vecteur de noms de variables à analyser (par défaut, toutes les variables de la table 'tab')
#'
#' @return une table analogue à celle entrée en input, mais dont certaines variables auront éventuellement été renommées
#' @export
identifie_type_variable <- function(tab,
                                    vars = names(tab)) {

  # === informations auxiliaires pour l'analyse

  # patterns de données correspondant à certes variables titres
  # RQ : les noms de variables auront au préalable été mis en minuscules
  patterntypes <- list(
    "annee" = "^(19|20)[[:digit:]]{2}",
    "sexe"  = "^(femme|homme)",
    "tr_age" = "[[:digit:]]{1,2} (à|a) [[:digit:]]{1,2} ans|(moins|plus) de [[:digit:]]{1,2} ans|[[:digit:]]{1,2} ans (et|ou) plus"
  )

}

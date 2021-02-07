# =============================================================================
# crée table listant les data.Drees de la sous-direction OSOL
# =============================================================================

library(tidyverse)
library(rvest)
library(stringr)

OSOLdatadrees <- liste_datadrees(producteurs = c("OSOL"))


# ===================================================================================
usethis::use_data(OSOLdatadrees,
                  overwrite = T)

chemin_fichier <- "C:/Users/XB5AFJ/Documents/ENSAI_2A_stage_comptnat/"
source(paste0(chemin_fichier,"00_fonctions.R"))
source(paste0(chemin_fichier,"01_chargement_packages_chemins.R"))
source(paste0(chemin_fichier,"02_chargement_retropolee.R"))
# Puisque la rétropolation donne une consommation finale négative pour le produit NB1
# Nous avons fait le choix de le joindre à ND2 (grande branche avec aggrégation commune)
source(paste0(chemin_fichier,"03_arbitrage_NB1_ND2.R"))
source(paste0(chemin_fichier,"04_cibles_et_TEI_ref.r"))
source(paste0(chemin_fichier,"05_1_export_TEI_input.R"))
source(paste0(chemin_fichier,"05_0_RAS_TEI.R"))


source(paste0(chemin_fichier,"06_chargement_TEI_RAS.R"))
source(paste0(chemin_fichier,"07_calcul_CF.R"))
source(paste0(chemin_fichier,"08_matrice_ventilation.R"))
source(paste0(chemin_fichier,"09_conversion_TEI_PB.R"))
source(paste0(chemin_fichier,"10_tableau_production.R"))
source(paste0(chemin_fichier,"11_calcul_VA_et_composantes.R"))
source(paste0(chemin_fichier,"12_export_TES.R"))

# Version technologie unique
source(paste0(chemin_fichier,"13_Technologie_unique.R"))

# ==============================================================
# ÉTAPE 1 : CHARGEMENT DES PACKAGES ET CONFIGURATION
# ==============================================================
# Objectif : Charger les librairies nécessaires et définir
#            l'environnement de travail.
# ==============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   ÉTAPE 1 : CHARGEMENT DES PACKAGES ET CONFIGURATION\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# ---- 1.1 Packages ----
cat("  > Chargement des packages...\n")
library(dplyr)
library(readxl)
library(tidyr)        # Transformer format long en format large
library(writexl)
library(openxlsx)
library(tibble)
# library(ioanalysis)  # modèle input output (non utilisé)
# library(janitor)     # Nettoyer les noms des colonnes (non utilisé)

# ---- 1.2 Configuration des chemins ----
chemin <- "Z:/Pacte_de_responsabilite/Pacte_responsabilite/propre/base/"
setwd(chemin)
annee_ref <- 2016
technologie_unique = "branche"

cat("  > Année de référence :", annee_ref, "\n")
cat("  > Répertoire de travail :", getwd(), "\n\n")
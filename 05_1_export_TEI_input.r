# ==============================================================
# ÉTAPE 5 : EXPORT DU FICHIER POUR L'ALGORITHME RAS
# ==============================================================
# Objectif : Exporter le fichier TEI_input.xlsx qui sera utilisé
#            par l'algorithme RAS (RAS_TEI.R).
# ==============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   ÉTAPE 5 : EXPORT DU FICHIER POUR L'ALGORITHME RAS\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# ---- 5.1 Préparation des données pour l'export ----
cat("  > Préparation du fichier d'entrée pour RAS...\n")

feuille_xl <- list(
  "TEI" = TEI_ref,
  "Cibles" = Cibles
)

# ---- 5.2 Export du fichier ----
write_xlsx(feuille_xl, "TEI_input.xlsx")
cat("  > Fichier exporté : TEI_input.xlsx\n")
cat("  > Feuilles :", paste(names(feuille_xl), collapse = ", "), "\n")

# ---- 5.3 Message d'instruction ----
cat("\n", paste(rep("-", 60), collapse = ""), "\n")
cat("  INSTRUCTION : Exécuter maintenant RAS_TEI.R\n")
cat("  Ce script produira le fichier TEI_RAS_output.xlsx\n")
cat(paste(rep("-", 60), collapse = ""), "\n\n")
# ==============================================================
# ÉTAPE 12 : EXPORT DU TES FINAL
# ==============================================================
# Objectif : Exporter le Tableau Entrées-Sorties (TES)
#            complet en prix de base.
# ==============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   ÉTAPE 12 : EXPORT DU TES FINAL\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# ---- 12.1 Export du TES ----
cat("  > Export du TES...\n")

exporter_TES(
  CI = TEI_PB_dom,
  CF = CF_PB,
  VA = CT,
  Production = Production,
  sbv_imp = Subv_impot,
  fichier = paste0("TES_", annee_ref, ".xlsx")
)

cat("  > Fichier exporté : TES_", annee_ref, ".xlsx\n", sep = "")

# ---- 12.2 Synthèse finale ----
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   SYNTHÈSE FINALE\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\n  Données produites :\n")
cat("    - TEI_input.xlsx        : entrée pour l'algorithme RAS\n")
cat("    - TEI_RAS_output.xlsx   : TEI équilibré par RAS\n")
cat("    - TES_", annee_ref, ".xlsx  : Tableau Entrées-Sorties final\n", sep = "")

cat("\n  Indicateurs clés :\n")
cat("    - Année de référence      :", annee_ref, "\n")
cat("    - Nombre de secteurs      :", nrow(TEI_PA), "\n")
cat("    - Production totale       :", format(sum(Production$Total_produit[1:nrow(Production)-1], na.rm = TRUE), big.mark = " "), "\n")
cat("    - VA totale               :", format(sum(va, na.rm = TRUE), big.mark = " "), "\n")
cat("    - TEI_PA total            :", format(sum(tei_tot_branche, na.rm = TRUE), big.mark = " "), "\n")

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   TERMINÉ AVEC SUCCÈS !\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
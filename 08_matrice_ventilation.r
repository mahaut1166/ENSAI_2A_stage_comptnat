# ==============================================================
# ÉTAPE 8 : CRÉATION DE LA MATRICE DE VENTILATION
# ==============================================================
# Objectif : Calculer les ratios de ventilation à partir du TEI_PA
#            pour répartir les marges entre produits.
# ==============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   ÉTAPE 8 : CRÉATION DE LA MATRICE DE VENTILATION\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# ---- 8.1 Calcul des ratios de ventilation ----
cat("  > Calcul de la matrice de ventilation...\n")

Ventilation <- TEI_PA %>%
  mutate(
    "Total lignes" = rowSums(select(., -c(Secteur)), na.rm = TRUE)
  ) %>%
  mutate(
    across(
      -c(Secteur, `Total lignes`),
      ~ .x / `Total lignes`
    )
  ) %>%
  select(-`Total lignes`) %>%
  arrange(Secteur)

# ---- 8.2 Vérification ----
# Vérifier que la somme de chaque ligne est égale à 1 (ou NA)
sommes_lignes <- rowSums(Ventilation[, 2:ncol(Ventilation)], na.rm = TRUE)
cat("  > Vérification : les sommes des lignes doivent être égales à 1\n")
cat("  > Sommes des lignes (10 premiers) :", paste(round(head(sommes_lignes), 4), collapse = ", "), "\n")

if (all(abs(sommes_lignes - 1) < 1e-10 | is.na(sommes_lignes))) {
  cat("  > ✅ Toutes les lignes somment à 1\n")
} else {
  cat("  > ⚠️ Des lignes ne somment pas à 1\n")
}

cat("  > Matrice de ventilation :", nrow(Ventilation), "x", ncol(Ventilation), "\n\n")
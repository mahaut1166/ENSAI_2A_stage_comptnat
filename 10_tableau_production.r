# ==============================================================
# ÉTAPE 10 : CONSTRUCTION DU TABLEAU DE PRODUCTION
# ==============================================================
# Objectif : Construire le tableau de production (P1)
#            pour calculer la valeur ajoutée.
# ==============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   ÉTAPE 10 : CONSTRUCTION DU TABLEAU DE PRODUCTION\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# ---- 10.1 Extraction de la production (P1) ----
cat("  > Extraction de la production (P1)...\n")

Production <- retro_long %>%
  filter(substr(id_operation, 1, 2) == "P1" & annee == annee_ref) %>%
  group_by(id_branche, id_produit) %>%
  summarise(valeur = sum(valeur, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = id_produit,
    names_from = id_branche,
    values_from = valeur,
    values_fill = 0
  ) %>%
  filter(!is.na(id_produit)) %>%
  select(order(names(.))) %>%
  arrange(id_produit) %>%
  mutate(Total_produit = rowSums(select(., -id_produit), na.rm = TRUE))

cat("  > Production calculée pour", nrow(Production), "produits\n")

# ---- 10.2 Ajout de la ligne total ----
ligne_total <- as.data.frame(t(colSums(Production[, -1], na.rm = TRUE)))
ligne_total$id_produit <- "Total branche"
ligne_total <- ligne_total[, names(Production)]

Production <- bind_rows(Production, ligne_total)

cat("  > Production dimensions :", nrow(Production), "x", ncol(Production), "\n")

# ---- 10.3 Vérification ----
verif_prod <- sum(Production[Production$id_produit == "Total branche", 2:25], na.rm = TRUE) -
  sum(Production$Total_produit[1:24], na.rm = TRUE)

cat("  > Vérification (total branches - total produits) :", round(verif_prod, 2), "\n")

if (abs(verif_prod) < 1) {
  cat("  > ✅ Production équilibrée\n")
} else {
  cat("  > ⚠️ Production non équilibrée\n")
}

cat("\n")


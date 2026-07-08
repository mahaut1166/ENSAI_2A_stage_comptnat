# ==============================================================
# ÉTAPE 7 : CALCUL DE LA CONSOMMATION FINALE (CF)
# ==============================================================
# Objectif : Calculer la consommation finale (CF) par produit
#            à partir des opérations P3 et P5 de la base rétropolée.
# ==============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   ÉTAPE 7 : CALCUL DE LA CONSOMMATION FINALE (CF)\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# ---- 7.1 Extraction des opérations P3 et P5 ----
cat("  > Extraction de la CF (P3) et FBCF (P5)...\n")

CF <- retro_long %>%
  filter(annee == annee_ref) %>%
  filter(substr(id_operation, 1, 2) %in% c("P3", "P5")) %>%
  filter(id_attrib_methode == 1) %>%
  mutate(type = case_when(
    substr(id_operation, 1, 2) == "P3" ~ "CF",
    substr(id_operation, 1, 2) == "P5" ~ "FBCF"
  )) %>%
  group_by(id_produit, type) %>%
  summarise(valeur = sum(valeur, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = id_produit,
    names_from = type,
    values_from = valeur,
    values_fill = 0
  )

cat("  > CF calculée pour", nrow(CF), "produits\n")

# ---- 7.2 Ajout des produits manquants ----
cat("  > Ajout des produits manquants (NG1, NA4)...\n")

CF <- CF %>%
  add_row(id_produit = "NG1", CF = 0, FBCF = 0) %>%
  add_row(id_produit = "NA4", !!!setNames(as.list(rep(0, ncol(CF) - 1)), names(CF)[-1])) %>%
  arrange(id_produit)

cat("  > CF dimensions finales :", nrow(CF), "x", ncol(CF), "\n")
cat("  > CF totale (somme) :", sum(CF[CF$id_produit != "Total branche", "CF"], na.rm = TRUE), "\n\n")



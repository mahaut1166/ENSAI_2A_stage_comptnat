# ==============================================================
# ÉTAPE 3 : ARBITRAGE NB1 / ND2 (FUSION)
# ==============================================================
# Objectif : Fusionner les codes NB1 et ND2 dans les dimensions
#            branche et produit car la CF de NB1 est négative.
# ==============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   ÉTAPE 3 : ARBITRAGE NB1 / ND2 (FUSION)\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

cat("  > Fusion de NB1 et ND2 dans les branches...\n")

# ---- 3.1 Fusion au niveau branche ----
retro_NB1_ND2_branche <- retro %>%
  filter(id_branche == "NB1" | id_branche == "ND2") %>%
  group_by(id_operation, id_attrib_methode, id_produit) %>%
  summarise(across(all_of(cols_numeriques), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(id_branche = "NB1+ND2")

# ---- 3.2 Fusion au niveau produit ----
retro_NB1_ND2_produit <- retro %>%
  filter(id_produit == "NB1" | id_produit == "ND2") %>%
  group_by(id_operation, id_attrib_methode, id_branche) %>%
  summarise(across(all_of(cols_numeriques), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(id_produit = "NB1+ND2")

# ---- 3.3 Assemblage final ----
retro <- bind_rows(retro, retro_NB1_ND2_branche) %>%
  bind_rows(retro_NB1_ND2_produit) %>%
  filter(!id_branche %in% c("NB1", "ND2")) %>%
  filter(!id_produit %in% c("NB1", "ND2")) %>%
  arrange(id_branche, id_produit, id_operation, id_attrib_methode)

cat("  > Dimensions après fusion :", nrow(retro), "x", ncol(retro), "\n")

# ---- 3.4 Passage en format long ----
retro_long <- retro %>%
  pivot_longer(
    cols = starts_with("1") | starts_with("2"),
    names_to = "annee",
    values_to = "valeur",
    names_prefix = "tab"
  ) %>%
  mutate(valeur = as.numeric(valeur))

cat("  > Format long créé :", nrow(retro_long), "lignes\n")
cat("  > Années disponibles :", paste(unique(retro_long$annee), collapse = ", "), "\n\n")
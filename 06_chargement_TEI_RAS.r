# ==============================================================
# ÉTAPE 6 : CHARGEMENT DU TEI APRÈS ALGORITHME RAS
# ==============================================================
# Objectif : Charger le TEI équilibré par la méthode RAS
#            et le préparer pour la conversion en prix de base.
# ==============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   ÉTAPE 6 : CHARGEMENT DU TEI APRÈS ALGORITHME RAS\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# ---- 6.1 Chargement du TEI RAS ----
cat("  > Chargement du TEI après RAS...\n")
TEI_PA <- read_excel("TEI_RAS_output.xlsx", sheet = 2, range = "A2:Z27") %>%
  filter(!Secteur %in% c("Total colonnes", "Cible colonne")) %>%
  select(-`Total lignes`, -`Cible ligne`) %>%
  arrange(Secteur)

cat("  > TEI_PA dimensions :", nrow(TEI_PA), "x", ncol(TEI_PA), "\n")
cat("  > Secteurs :", paste(head(TEI_PA$Secteur, 5), "..."), "\n")

# ---- 6.2 Chargement de l'ERE de référence ----
cat("  > Chargement de l'ERE de référence...\n")
ERE_ref <- read_excel("Synthèse EREPrix courant.xls", range = "A20:V48")

# ---- 6.3 Fusion NB1/ND2 dans ERE_ref ----
cat("  > Fusion NB1/ND2 dans ERE_ref...\n")

ERE_ref_NB1_ND2 <- ERE_ref %>%
  filter(`Pro- duit` %in% c("NB1", "ND2")) %>%
  summarise(
    across(all_of(names(ERE_ref)[-1]), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(`Pro- duit` = "NB1+ND2")

ERE_ref <- ERE_ref %>%
  bind_rows(ERE_ref_NB1_ND2) %>%
  filter(!`Pro- duit` %in% c("NB1", "ND2")) %>%
  arrange(`Pro- duit`)

cat("  > ERE_ref dimensions :", nrow(ERE_ref), "x", ncol(ERE_ref), "\n\n")
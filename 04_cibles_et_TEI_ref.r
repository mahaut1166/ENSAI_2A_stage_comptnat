# ==============================================================
# ÉTAPE 4 : CRÉATION DES CIBLES ET CHARGEMENT DU TEI DE RÉFÉRENCE
# ==============================================================
# Objectif : Construire les cibles marginales pour l'algorithme RAS
#            et charger le TEI de référence pour l'arbitrage.
# ==============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   ÉTAPE 4 : CRÉATION DES CIBLES ET CHARGEMENT DU TEI DE RÉFÉRENCE\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")


# ---- 4.1 Création des cibles CB et ERE ----
cat("  > Création des cibles (CB et ERE)...\n")

# Chargement des données CB (colonnes) et ERE (lignes)
CB <- charger_ere_cb(Annee = annee_ref, attribut = "CB", col_attribut = "id_attrib_methode")
ERE <- charger_ere_cb(Annee = annee_ref, attribut = "ERE", col_attribut = "id_attrib_methode")

# Assemblage des cibles
Cibles <- CB %>%
  full_join(ERE, by = c("id_branche" = "id_produit")) %>%
  rename(
    Secteur = id_branche,
    Cible_colonne = P2_._.x,
    Cible_ligne = P2_._.y
  ) %>%
  select(Secteur, Cible_ligne, Cible_colonne) %>%
  filter(!Secteur %in% c("NZ1", "NZ2")) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%
  arrange(Secteur)

cat("  >", nrow(Cibles), "secteurs dans les cibles\n")

# ---- 4.2 Vérification et ajustement manuel ----
ecart <- sum(Cibles$Cible_ligne, na.rm = TRUE) - sum(Cibles$Cible_colonne, na.rm = TRUE)
cat("  > Écart initial (lignes - colonnes) :", round(ecart, 2), "\n")

if (abs(ecart) > 0.1) {
  cat("  > Ajustement manuel : ajout de", -ecart, "à NB1+ND2 (ligne)\n")
  Cibles <- Cibles %>%
    mutate(
      Cible_ligne = ifelse(Secteur == "NB1+ND2", Cible_ligne - ecart, Cible_ligne)
    )
}

# ---- 4.3 Chargement du TEI de référence ----
cat("  > Chargement du TEI de référence...\n")
TEI_ref <- read_excel("P:/Materiel Comptable_2/TEI_2019.xlsx",
                      sheet = "TEI_NIV1",
                      range = "B2:AA27") %>%
  rename(id_branche = MAR)

cat("  > TEI référencé :", nrow(TEI_ref), "branches\n")

# ---- 4.4 Fusion NB1/ND2 dans le TEI de référence ----
cat("  > Fusion NB1/ND2 dans le TEI de référence...\n")

cols_numeriques_tei <- TEI_ref %>%
  select(-id_branche, -TOTAL) %>%
  select(where(is.numeric)) %>%
  names()

# Fusion colonne
TEI_ref <- TEI_ref %>%
  mutate(`NB1+ND2` = as.numeric(unlist(TEI_ref[, "NB1"] + TEI_ref[, "ND2"]))) %>%
  select(id_branche, sort(c(cols_numeriques_tei, "NB1+ND2")), TOTAL) %>%
  select(-NB1, -ND2)

# Fusion ligne
cols_numeriques_tei <- TEI_ref %>%
  filter(id_branche == "NB1+ND2") %>%
  select(-id_branche) %>%
  select(where(is.numeric)) %>%
  names()

TEI_ref_NB1_ND2_ligne <- TEI_ref[TEI_ref$id_branche == "NB1", cols_numeriques_tei] + 
  TEI_ref[TEI_ref$id_branche == "ND2", cols_numeriques_tei]

TEI_ref <- TEI_ref %>%
  add_row(id_branche = "NB1+ND2", TEI_ref_NB1_ND2_ligne) %>%
  filter(!id_branche %in% c("NB1", "ND2")) %>%
  arrange(id_branche)

# ---- 4.5 Sélection des branches communes ----
branches_communes <- sort(intersect(unique(TEI_ref$id_branche), unique(retro$id_branche)))

TEI_ref <- TEI_ref %>%
  filter(id_branche %in% branches_communes) %>%
  select("id_branche", all_of(branches_communes))

cat("  >", length(branches_communes), "branches communes identifiées\n\n")
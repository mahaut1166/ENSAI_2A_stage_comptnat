# ==============================================================
# ÉTAPE 9 : CONVERSION EN PRIX DE BASE (TEI_PB)
# ==============================================================
# Objectif : Utiliser la fonction calculer_TES_PB pour convertir
#            le TEI en prix d'acquisition en prix de base.
# ==============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   ÉTAPE 9 : CONVERSION EN PRIX DE BASE (TEI_PB)\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# ---- 9.1 Exécution de la conversion ----
cat("  > Conversion du TEI en prix de base...\n")

res_PB <- calculer_TES_PB(
  TEI_pa = TEI_PA,
  ERe = ERE,
  ERe_ref = ERE_ref,
  Cf = CF,
  VENTILATION = Ventilation
)

# ---- 9.2 Récupération des résultats ----
TEI_PB <- res_PB$TEI_PB
TEI_PB[is.na(TEI_PB)] <- 0

TEI_PB_dom <- res_PB$TEI_PB_dom
TEI_PB_dom[is.na(TEI_PB_dom)] <- 0

CF_PB <- res_PB$CF_PB %>%
  mutate(EXP = res_PB$taux_ERE$EXP)


# Totaux
tot_cf_PB_dom <- unlist(sum(CF_PB$CF_PB, na.rm = TRUE))
tot_fbcf <- unlist(sum(CF_PB$FBCF, na.rm = TRUE))
tot_exp <- unlist(sum(CF_PB$EXP, na.rm=T))

# Ajouter les lignes
CF_PB <- CF_PB %>%
  add_row(id_produit = "Total PB dom", CF_PB = tot_cf_PB_dom, FBCF = tot_fbcf, EXP = tot_exp) %>%
  add_row(id_produit = "IMP", CF_PB = sum(res_PB$IMP_CF, na.rm = TRUE), FBCF = 0, EXP = 0)

# Recalculer le total après ajout d'IMP
tot_cf_PB <- unlist(sum(CF_PB[!CF_PB$id_produit %in% c("Total PB dom"),"CF_PB"], na.rm = TRUE))

CF_PB <- CF_PB %>%
  add_row(id_produit = "Total PB", CF_PB = tot_cf_PB, FBCF = tot_fbcf, EXP = tot_exp) %>%
  add_row(id_produit = "TVA", CF_PB = sum(res_PB$TVA_CF, na.rm = TRUE), FBCF = 0, EXP = 0) %>%
  add_row(id_produit = "TN", CF_PB = sum(res_PB$TN_CF, na.rm = TRUE), FBCF = 0, EXP = 0)

# Total PA
tot_cf_PA <- unlist(sum(CF_PB[!CF_PB$id_produit %in% c("Total PB", "Total PB dom"),"CF_PB"], na.rm = TRUE))

CF_PB <- CF_PB %>%
  add_row(id_produit = "Total PA", CF_PB = tot_cf_PA, FBCF = tot_fbcf, EXP = tot_exp) %>%
  filter(!id_produit %in% c("NZ1", "NZ2"))

# Garder seulement les colonnes utiles
CF_PB <- CF_PB %>%
  select(id_produit,EXP, CF_PB, FBCF)

cat("  > TEI_PB dimensions :", nrow(TEI_PB), "x", ncol(TEI_PB), "\n")
cat("  > TEI_PB_dom dimensions :", nrow(TEI_PB_dom), "x", ncol(TEI_PB_dom), "\n")

# ---- 9.3 Vérification de l'équilibre ----
# Vérifier que la somme des lignes (hors total) égale la somme des colonnes
lignes_produits <- TEI_PB$Secteur[!TEI_PB$Secteur %in% c("Total colonnes", "Total lignes")]
colonnes <- names(TEI_PB)[!names(TEI_PB) %in% c("Secteur", "Total lignes")]

somme_lignes <- sum(TEI_PB[TEI_PB$Secteur %in% lignes_produits, colonnes], na.rm = TRUE)
somme_colonnes <- sum(TEI_PB[TEI_PB$Secteur == "Total colonnes", colonnes], na.rm = TRUE)
ecart <- somme_lignes - somme_colonnes

cat("  > Écart (lignes - colonnes) :", round(ecart, 2), "\n")

if (abs(ecart) < 1) {
  cat("  > ✅ TEI_PB équilibré\n")
} else {
  cat("  > ⚠️ TEI_PB non équilibré\n")
}

cat("\n")
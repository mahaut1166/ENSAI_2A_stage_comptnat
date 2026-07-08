# ==============================================================
# ÉTAPE 11 : CALCUL DE LA VALEUR AJOUTÉE ET DE SES COMPOSANTES
# ==============================================================
# Objectif : Calculer la valeur ajoutée et les composantes
#            (TVA, TN, IMP) pour chaque branche.
# ==============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   ÉTAPE 11 : CALCUL DE LA VALEUR AJOUTÉE ET COMPOSANTES\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# ---- 11.1 Valeur ajoutée (VA) ----
cat("  > Calcul de la valeur ajoutée...\n")

prod_tot_branche <- as.numeric(Production[Production$id_produit == "Total branche", 2:(ncol(Production) - 1)])
tei_tot_branche <- colSums(TEI_PA[, -1])
names(tei_tot_branche) <- TEI_PA$Secteur

va <- prod_tot_branche - tei_tot_branche
names(va) <- colnames(TEI_PA[, -1])
va <- va[order(names(va))]

cat("  > VA calculée pour", length(va), "branches\n")

# ---- 11.2 Extraction des taux d'imposition ----
cat("  > Calcul des impôts sur la CI...\n")

# S'assurer que NP1 a des taux nuls
res_PB$taux_ERE$TVA["NP1"] <- 0
res_PB$taux_ERE$TN["NP1"] <- 0
res_PB$taux_ERE$IMP["NP1"] <- 0

TVA_CI <- res_PB$TVA_CI
TN_CI <- res_PB$TN_Ci
IMP_CI <- res_PB$IMP_CI

# Aggrégation par branche
TVA <- colSums(TVA_CI, na.rm = TRUE)
names(TVA) <- colnames(TVA_CI)

TN <- colSums(TN_CI, na.rm = TRUE)
names(TN) <- colnames(TN_CI)

IMP <- colSums(IMP_CI, na.rm = TRUE)
names(IMP) <- colnames(IMP_CI)

TEI_PB_col <- TEI_PB[TEI_PB$Secteur == "Total colonnes",-c(1,ncol(TEI_PB))]
TEI_PA_col <- colSums(TEI_PA[,-1], na.rm=T)

cat("  > TVA totale :", sum(TVA, na.rm = TRUE), "\n")
cat("  > TN totale :", sum(TN, na.rm = TRUE), "\n")
cat("  > IMP totale :", sum(IMP, na.rm = TRUE), "\n")

# ---- 11.3 Assemblage du tableau des composantes ----
CT <- rbind(IMP) %>%
  rbind(TEI_PB_col) %>%
  rbind(TVA) %>%
  rbind(TN) %>%
  rbind(TEI_PA_col)%>%
  rbind(va)%>%
  mutate(
    operation = c("IMP", "CI PB", "TVA", "TN", "CI PA", "VA")
  )%>%
  relocate(operation, .before = everything())
CT <- CT %>%
  mutate(Total_ligne = rowSums(CT[,-1]))

cat("  > Composantes assemblées :", nrow(CT), "lignes\n\n")

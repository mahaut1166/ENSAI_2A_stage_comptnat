# ==============================================================
# ÉTAPE 13 : DIAGONALISATION ET TRANSFORMATION EN TECHNOLOGIE
#                UNIQUE DE LA PRODUCTION
# ==============================================================
# Objectif : Appliquer les calculs du manuel de STATEC
# ==============================================================

V <- as.matrix(Production[Production$id_produit != "Total branche", -c(1,ncol(Production))])

g <- rbind(Production[Production$id_produit == "Total branche", -c(1,ncol(Production))]) %>%
  as.numeric()
q <- Production %>%
  filter(id_produit != "Total branche") %>%
  pull(Total_produit) %>%
  as.numeric()

Y <- as.matrix(CT[!CT$operation %in% c("CI PB", "CI PA"),-c(1,ncol(CT))])
U <- as.matrix(TEI_PB_dom[TEI_PB_dom$Secteur!="Total colonnes",-c(1, ncol(TEI_PB_dom))])

C <- V %*% solve(diag(g))
D <- t(V) %*% solve(diag(q))

if(technologie_unique=="branche"){
  CI_tech <- U %*% t(C)
  VA_tech <- Y %*% t(C)
} else{
  CI_tech <- U %*% D
  VA_tech <- Y %*% D
}

# Production "diagonalisée"
ifelse(
  technologie_unique == "produit",
  Production_diago <- diag(q),
  Production_diago <- diag(g)
)
colnames(Production_diago) <- rownames(Production_diago) <- colnames(Production[,-c(1, ncol(Production))])

# Mise en page
CI_tech <- CI_tech %>%
  as.data.frame()%>%
  mutate(
    Total_produit = rowSums(CI_tech, na.rm = T),
    Secteur = rownames(CI_tech)
    )%>%
  relocate(Secteur, .before = everything())
CI_tech <- CI_tech %>%
  add_row(Secteur = "Total_PB_dom", !!!setNames(colSums(CI_tech[,-1], na.rm = T),colnames(CI_tech[, -1])))


VA_tech <- VA_tech %>%
  as.data.frame()%>%
  mutate(
    operation = c("IMP", "TVA", "TN", "VA")
  )%>%
  relocate(operation, .before = everything())

CI_PB_branche <- CI_tech[CI_tech$Secteur == "Total_PB_dom",-c(1, ncol(CI_tech))] + VA_tech[VA_tech$operation == "IMP",-c(1)]
CI_PA_branche <- CI_PB_branche + 
  VA_tech[VA_tech$operation == "TVA",-1] +
  VA_tech[VA_tech$operation == "TN",-1]

VA_tech <- VA_tech %>%
  add_row(operation = "CI PB", !!!setNames(CI_PB_branche,colnames(CI_tech[, -c(1, ncol(CI_tech))])))%>%
  add_row(operation = "CI PA", !!!setNames(CI_PA_branche,colnames(CI_tech[, -c(1, ncol(CI_tech))])))
  
VA_tech$ordre <- factor(VA_tech$operation, levels = c(
  "IMP",
  "CI PB", 
  "TVA", "TN",
  "CI PA",
  "VA"
))
VA_tech <- VA_tech %>%
  arrange(ordre) %>%
  select(-ordre)

Production_diago <- Production_diago %>%
  as.data.frame() %>%
  mutate(
    Total_produit = rowSums(Production_diago),
    Secteur = colnames(Production_diago)
  ) %>%
  relocate(Secteur, .before = everything())
Production_diago <- Production_diago %>%
  add_row(Secteur = "Total branche",
          !!!setNames(
              object = colSums(Production_diago[,-1]),
              nm = colnames(Production_diago)[-1]
          )
  )

# Exportrer
exporter_TES_tech(
  CI = CI_tech,
  CF = CF_PB,
  VA = VA_tech,
  Production = Production_diago,
  fichier = paste0("TES_tech_", annee_ref, ".xlsx")
)






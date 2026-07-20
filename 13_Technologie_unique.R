# ==============================================================
# ÉTAPE 13 : DIAGONALISATION ET TRANSFORMATION EN TECHNOLOGIE
#                UNIQUE DE LA PRODUCTION
# ==============================================================
# Objectif : Appliquer les calculs du manuel de STATEC
# ==============================================================

TES_arbitr <- read_excel(paste0(chemin,"TES_2016.xlsx"), sheet = 2)
id_produit <- read_excel(paste0(chemin,"TES_2016.xlsx"), sheet = 2, range = "B2:B25")

V <- as.matrix(read_excel(paste0(chemin,"TES_2016.xlsx"), sheet = 2, range = "C2:Y25"))
row.names(V) <- unlist(id_produit)


g <- as.numeric(read_excel(paste0(chemin,"TES_2016.xlsx"), sheet = 2, range = "C25:Y26"))
names(g) <- unlist(id_produit)


q <- as.numeric(unlist(read_excel(paste0(chemin,"TES_2016.xlsx"), sheet = 2, range = "Z2:Z25")))
names(q) <- unlist(id_produit)


Y <- as.matrix(read_excel(paste0(chemin,"TES_2016.xlsx"), sheet = 2, range = "AB26:AX32"))
colnames(Y) <- unlist(id_produit)
row.names(Y) <- unlist(read_excel(paste0(chemin,"TES_2016.xlsx"), sheet = 2, range = "AA26:AA32"))

Y <- CT %>%
  filter(operation %in% c("IMP", "TVA", "TN", "VA"))%>%
  select(-operation,-Total_ligne) %>%
  as.matrix()
colnames(Y) <- unlist(id_produit)
row.names(Y) <- c("IMP", "TVA", "TN", "VA")

U <- as.matrix(read_excel(paste0(chemin,"TES_2016.xlsx"), sheet = 2, range = "AB2:AX25"))
row.names(U) <- unlist(id_produit)


B <- U %*% solve(diag(g))

C <- V %*% solve(diag(g))
rownames(C) <- rownames(V)
colnames(C) <- colnames(V)

D <- t(V) %*% solve(diag(q))
rownames(D) <- colnames(V)
colnames(D) <- rownames(V)

L <- Y %*% solve(diag(g))

if(technologie_unique=="branche"){
  CI_tech <- U %*% t(C)
  VA_tech <- Y %*% t(C)
} else{
  CI_tech <- U %*% solve(V) %*% diag(q)
  VA_tech <- Y %*% solve(V) %*% diag(q)
}

# Production "diagonalisée"
ifelse(
  technologie_unique == "produit",
  Production_diago <- diag(q),
  Production_diago <- diag(g)
)
colnames(Production_diago) <- rownames(Production_diago) <- unlist(id_produit)

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

# Exportrer
exporter_TES_tech(
  ci = CI_tech,
  cf =  read_excel(paste0(chemin,"TES_2016.xlsx"), sheet = 2, range = "AZ2:BC31"),
  va = VA_tech,
  production = Production_diago,
  fichier = paste0("TES_tech_", annee_ref, ".xlsx")
)

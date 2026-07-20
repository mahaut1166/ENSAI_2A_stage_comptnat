# ==============================================================
# ÉTAPE 14 : Détailler la répartition de la VA
# ==============================================================
# Objectif : Faire appraitre les cotisations sociales
# ==============================================================

D11 <- retro_long %>%
  filter(annee == annee_ref,
    substr(id_operation,0,3) == "D11",
         id_attrib_methode == 1)%>%
  group_by(id_branche) %>%
  summarise(valeur = sum(valeur))

D12 <- retro_long %>%
  filter(annee == annee_ref,
    substr(id_operation,0,3) == "D12",
         id_attrib_methode == 1)%>%
  group_by(id_branche) %>%
  summarise(valeur = sum(valeur))

VA_tech <- VA_tech %>%
  add_row(operation = "D11", !!!setNames(D11$valeur, D11$id_branche))%>%
  add_row(operation = "D12", !!!setNames(D12$valeur, D11$id_branche)) %>%
  add_row(operation = "D29-D39", !!!setNames((CB$D29._+CB$D39._), CB$id_branche))%>%
  # Calcul l'EBE par égalité comptable : 
  # VA = D1 + D29 - D39 + EBE
  add_row(operation = "EBE", !!!setNames(
    (
      VA_tech[VA_tech$operation == "VA",-1] - (CB$D29._+CB$D39._+D11$valeur+D12$valeur)
     ),
                                         CB$id_branche))

# Exportrer
exporter_TES_tech(
  ci = CI_tech,
  cf =  read_excel(paste0(chemin,"TES_2016.xlsx"), sheet = 2, range = "AZ2:BC31"),
  va = VA_tech,
  production = Production_diago,
  fichier = paste0("TES_tech_", annee_ref, ".xlsx")
)

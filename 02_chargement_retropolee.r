# ==============================================================
# ÉTAPE 2 : CHARGEMENT ET FILTRAGE DE LA BASE RÉTROPOLÉE
# ==============================================================
# Objectif : Charger la base rétropolée et la préparer pour
#            les étapes suivantes (agrégation NB1/ND2).
# ==============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("   ÉTAPE 2 : CHARGEMENT ET FILTRAGE DE LA BASE RÉTROPOLÉE\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# ---- 2.1 Chargement de la base ----
cat("  > Chargement de la base rétropolée...\n")
retro <- read_excel(paste0(chemin, "Base_complete2025-10-01.xlsx"),
                    sheet = 1,
                    col_types = "text")

cat("  > Dimensions initiales :", nrow(retro), "x", ncol(retro), "\n")

# ---- 2.2 Filtrage et formatage ----
# Format N1 : on garde seulement MAR avec la série 0_retropolation_25_09
# et on raccourcit les codes à 3 caractères
retro <- retro %>%
  filter(
    id_territoire == "MAR" &
      serie == "0_retropolation_25_09"
  ) %>%
  mutate(
    id_produit = substr(id_produit, 1, 3),
    id_branche = substr(id_branche, 1, 3)
  )

cat("  > Dimensions après filtrage :", nrow(retro), "x", ncol(retro), "\n")

# ---- 2.3 Conversion des colonnes numériques ----
cols_numeriques <- retro %>%
  select(starts_with("1") | starts_with("2")) %>%
  names()

for(col in cols_numeriques){
  retro[[col]] <- as.numeric(retro[[col]])
}

cat("  >", length(cols_numeriques), "colonnes numériques converties\n")
cat("  > Colonnes numériques :", paste(head(cols_numeriques, 3), "..."), "\n\n")
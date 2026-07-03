### CHARGEMENT DES FONCTIONS 

######################################
# Pour chargement des données

charger_ere_cb <- function(Annee = annee_ref,
                           tableau = retro_long,
                           col_operation = "id_operation",
                           col_produit = "id_produit",
                           col_branche = "id_branche",
                           col_annee = "annee",
                           col_valeur = "valeur",
                           col_attribut = "id_attribut_methode",
                           attribut) {
  
  if(!attribut %in% c("CB", "ERE")) {
    stop(paste("L'attribut doit être 'CB' ou 'ERE'"))
  }
  
  # Filtrer
  data_annee <- tableau %>%
    filter(.data[[col_annee]] == Annee)
  
  # Déterminer la colonne d'identification
  col_id <- if(attribut == "ERE") col_produit else col_branche
  
  # ====  TABLEAU PRINCIPAL (P2_._) ====
  df <- data_annee %>%
    filter(.data[[col_attribut]] == ifelse(attribut == "ERE", 1, 2)) %>%
    filter(.data[[col_operation]] == "P2_._") %>%
    group_by(.data[[col_id]], .data[[col_operation]]) %>%
    summarise(valeur = sum(.data[[col_valeur]], na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      id_cols = all_of(col_id),
      names_from = all_of(col_operation),
      values_from = valeur,
      values_fill = 0
    )
  
  # ====  AJOUT D'AUTRES ELEMENTS  ====
  if(attribut == "ERE"){
    operations <- c("MT_._", "MC_._", "D21.1","D21.2","D21.4", "D31._", "P7_._")
  } else {
    operations <- c("D39._", "D29._")
  }
  
  for(op in operations) {
    somme_op <- data_annee %>%
      filter(.data[[col_operation]] == op) %>%
      filter(.data[[col_attribut]] == ifelse(
        substr(op,1,2) %in% c("MT", "MC","P7"),
        1,
        2
      )) %>%
      group_by(.data[[col_id]]) %>%
      summarise(!!paste0(op) := sum(.data[[col_valeur]], na.rm = TRUE), .groups = "drop")

    df <- df %>%
      full_join(somme_op, by = col_id) %>%
      mutate(!!paste0(op) := ifelse(is.na(.data[[paste0(op)]]), 0, .data[[paste0(op)]]))
  }
  
  df <- df %>%
    arrange(!!sym(col_id))
  
  return(df)
}
######################################
### Récupère les valeurs dans l'ERE en PA

tableau_taux <- function(ere=ERE,
                         tei=TEI_PA,
                         ventilation = Ventilation,
                         taux,
                         col_produit = "id_produit",
                         col_branche = "id_branche",
                         col_valeur = "valeur",
                         mc = "MC_._", tva = "D21.1") {
  
  if(!taux %in% c("MC", "MT", "TVA", "TN","IMP")) {
    stop(paste("taux doit être 'MC', 'MT', 'TVA', 'IMP' ou 'TN'"))
  }
  
  if(taux == "MC") {
    valeur <- ere[[mc]]
  } else if(taux == "MT") {
    valeur <- ere[["MT_._"]]
  } else if(taux == "TVA") {
    valeur <- ere[["D21.1"]]
  } else if(taux == "IMP"){
    valeur <- ere[["P7_._"]]
  }
  else if(taux == "TN") {
    # === TAXES NETTES = Somme des impôts - Somme des subventions ===
    id_impots <- grep("^D2", names(ere), value = TRUE)
    id_impots <- id_impots[id_impots != "D21.1"] # Ne comprend pas la TVA
    
    id_subv <- grep("^D3", names(ere), value = TRUE)
    
    TN <- ere %>%
      group_by(!!sym(col_produit)) %>%
      summarise(
        impots = sum(rowSums(across(all_of(id_impots)), na.rm = TRUE), na.rm = TRUE),
        subventions = sum(rowSums(across(all_of(id_subv)), na.rm = TRUE), na.rm = TRUE),
        .groups = "drop"
      )%>%
      mutate(
        taxes_nettes = impots + subventions
      )
    
    valeur <- TN$taxes_nettes
    names(valeur) <- TN[[col_produit]]
    
  }
  
  # Assigner les noms pour les autres taux
  names(valeur) <- ere[[col_produit]]
  
  return(valeur = valeur)
}

######################################
### CALCUL DE LA MARGE DE COMMERCE A RETIREE DU TEI PA

# --- 1. Préparation des matrices CI ---
preparer_CI <- function(TEI_pa, col_secteur = "Secteur", exclure_produits = c("NZ1", "NZ2")) {
  
  CI_tot <- TEI_pa %>%
    filter(!.data[[col_secteur]] %in% exclure_produits) %>%
    arrange(.data[[col_secteur]])
  
  CI_mat <- as.matrix(CI_tot[, -1])
  rownames(CI_mat) <- CI_tot[[col_secteur]]
  
  CI_par_produit <- rowSums(CI_mat, na.rm = TRUE)
  
  return(list(
    CI_tot = CI_tot,
    CI_mat = CI_mat,
    CI_par_produit = CI_par_produit,
    produits_CI = rownames(CI_mat)
  ))
}

# --- 2. Préparation de CF par produit ---
preparer_CF <- function(cf, col_produit = "id_produit", col_valeur = "CF", exclure_produits = c("NZ1", "NZ2")) {
  
  CF_filtre <- cf %>%
    filter(!.data[[col_produit]] %in% exclure_produits) %>%
    arrange(.data[[col_produit]])
  
  CF_par_produit <- CF_filtre[[col_valeur]]
  names(CF_par_produit) <- CF_filtre[[col_produit]]
  
  return(list(
    CF_filtre = CF_filtre,
    CF_par_produit = CF_par_produit,
    produits_CF = names(CF_par_produit)
  ))
}

# --- 3. Calcul de la consommation totale (CI + CF) ---
calculer_conso_totale <- function(ci_par_produit, produits_ci,
                                  cf_par_produit, produits_cf) {
  
  conso_tot <- rep(0, length(produits_ci))
  names(conso_tot) <- produits_ci
  
  produits_communs <- intersect(produits_ci, produits_cf)
  for(prod in produits_communs) {
    conso_tot[prod] <- ci_par_produit[prod] + cf_par_produit[prod]
  }
  
  return(list(
    conso_totale = conso_tot,
    produits_communs = produits_communs,
    produits_uniquement_CI = setdiff(produits_ci, produits_cf),
    produits_uniquement_CF = setdiff(produits_cf, produits_ci)
  ))
}

# --- 4. Récupération des taux (MC, TVA, TN) ---
recuperer_taux <- function(Ere, exclure_produits = c("NZ1", "NZ2")) {
  # MC = ERE$MC
  
  MC <- tableau_taux(taux = "MC", ere = Ere )
  TVA <- tableau_taux(taux = "TVA", ere = Ere)
  TN <- tableau_taux(taux = "TN", ere = Ere)
  IMP <- tableau_taux(taux = "IMP", ere = Ere)
  
  MC <- MC[!names(MC) %in% exclure_produits]
  TVA <- TVA[!names(TVA) %in% exclure_produits]
  TN <- TN[!names(TN) %in% exclure_produits]
  IMP <- IMP[!names(IMP) %in% exclure_produits]
  return(list(MC = MC, TVA = TVA, TN = TN, IMP=IMP))
}

# --- 5. Calcul des taux de marge ---
calculer_taux_marge <- function(ci_mat, cf_par_produit, conso_tot, 
                                produits_ci, produits_cf, produits_comm) {
  
  taux_CI <- ci_mat
  
  taux_CF <- rep(0, length(produits_cf))
  names(taux_CF) <- produits_cf
  
  # Taux pour CI (par branche)
  for(prod in produits_ci) {
    if(prod %in% produits_comm) {
      if(conso_tot[prod] > 0) {
        taux_CI[prod,] <- ci_mat[prod,] / conso_tot[prod]
      }
    }
  }
  
  # Taux pour CF
  for(prod in produits_cf) {
    if(prod %in% produits_comm) {
      if(conso_tot[prod] > 0) {
        taux_CF[prod] <- cf_par_produit[prod] / conso_tot[prod]
      }
    }
  }
  
  return(list(taux_CI = taux_CI, taux_CF = taux_CF))
}

# --- 6. Clé de répartition CI/CF ---
creer_cle_repartition <- function(ere_ref,
                                  produits_ci,
                                  exclure_produits = c("NZ1", "NZ2"),
                                  col_produit = "Pro- duit",
                                  col_MC_ci = "Marges de commerce",
                                  col_MC_cf = "CF Menages Commerc.") {
  
  cle <- ere_ref %>%
    filter(.data[[col_produit]] %in% produits_ci) %>%
    filter(!.data[[col_produit]] %in% exclure_produits) %>%
    arrange(.data[[col_produit]]) %>%  
    select(all_of(c(col_produit, col_MC_ci, col_MC_cf))) %>%
    mutate(
      total_marge = coalesce(.data[[col_MC_ci]], 0) + coalesce(.data[[col_MC_cf]], 0)
    ) %>%
    mutate(
      ratio_CI = ifelse(total_marge > 0, coalesce(.data[[col_MC_ci]] / total_marge, 0), 0),
      ratio_CF = ifelse(total_marge > 0, coalesce(.data[[col_MC_cf]] / total_marge, 0), 0)
    )
  
  return(cle)
}

# --- 7. Calcul des marges par produit ---
calculer_marges_produit <- function(cle_repartition,
                                    MC_ere,
                                    col_produit = "Pro- duit") {
  
  produits <- cle_repartition[[col_produit]]
  
  marge_CI <- rep(0, length(produits))
  marge_CF <- rep(0, length(produits))
  names(marge_CI) <- produits
  names(marge_CF) <- produits
  
  # Matrices de marges CI par branche (pour TVA et TN)
  # On suppose que taux_CI est un vecteur avec les taux par produit
  # et que les taux sont constants sur toutes les branches
  
  for(i in seq_along(produits)) {
    prod <- produits[i]
    if(prod %in% names(MC_ere)) {
      marge_CI[prod] <- MC_ere[prod] * cle_repartition$ratio_CI[i]
      marge_CF[prod] <- MC_ere[prod] * cle_repartition$ratio_CF[i]
    }
  }
  
  # Matrice des marges CI par branche
  # On répartit la marge CI proportionnellement à la consommation CI de chaque branche
  
  return(list(
    produits_cle = produits,
    MC_CI = marge_CI,
    MC_CF = marge_CF
  ))
}

# --- 8. Calcul du TEI en prix de base ---
calculer_TEI_PB <- function(TEI_pa,
                            ci_mat,
                            MC_ci, 
                            TVA_ci,
                            TN_ci,
                            IMP_ci,
                            ventilation,
                            col_secteur = "Secteur",
                            exclure_produits = c("NZ1", "NZ2")) {
  TEI_PB_mat <- ci_mat
  TEI_PB_dom_mat <- ci_mat
  
  ventilation_mat <-ventilation %>%
    filter(.data[[col_secteur]] %in% rownames(TEI_PB_mat)) %>%
    arrange(.data[[col_secteur]]) %>%
    select(-all_of(col_secteur)) %>%
    as.matrix()
  rownames(ventilation_mat) <- ventilation %>%
    filter(.data[[col_secteur]] %in% rownames(TEI_PB_mat)) %>%
    arrange(.data[[col_secteur]]) %>%
    pull(.data[[col_secteur]])
  
  if(!all(rownames(TEI_PB_mat) == rownames(ventilation_mat))) {
    stop("Les produits de TEI_PB_mat et ventilation ne correspondent pas")
  }
  
  MC_ci_aligned <- MC_ci[match(rownames(ventilation_mat), names(MC_ci))]   # réordonne par nom
  MC_CI <- sweep(ventilation_mat, MARGIN = 1, STATS = MC_ci_aligned, FUN = "*")  

  # Aligner les matrices
  TVA_ci <- TVA_ci[match(rownames(TEI_PB_mat), rownames(TVA_ci)), ]
  TN_ci <- TN_ci[match(rownames(TEI_PB_mat), rownames(TN_ci)), ]
  IMP_ci <- IMP_ci[match(rownames(TEI_PB_mat), rownames(IMP_ci)), ]
  MC_CI <- MC_CI[match(rownames(TEI_PB_mat), rownames(MC_CI)), ]
  
  for(prod in rownames(TEI_PB_mat)) {
      TEI_PB_mat[prod, ] <- TEI_PB_mat[prod, ] - 
        MC_CI[prod,]- TVA_ci[prod, ] - TN_ci[prod, ]
  }
  
  for(prod in rownames(TEI_PB_dom_mat)) {
      TEI_PB_dom_mat[prod, ] <- TEI_PB_dom_mat[prod, ] - 
        MC_CI[prod, ] - TVA_ci[prod, ] - TN_ci[prod, ] - IMP_ci[prod, ]
  }
  
  TEI_PB <- data.frame(
    Secteur = rownames(TEI_PB_mat),
    TEI_PB_mat,
    check.names = FALSE
  )
  TEI_PB[TEI_PB[[col_secteur]]=="NG1",-1] <-
    as.list(colSums(MC_CI, na.rm=T))
  TEI_PB <- TEI_PB %>%
    bind_rows(
      data.frame(
        Secteur = "Total colonnes",
        t(colSums(select(., -Secteur), na.rm = TRUE)),
        check.names = FALSE
      )
    ) %>%
    mutate(
      "Total lignes" = rowSums(select(., -Secteur), na.rm = TRUE)
    )
  
  TEI_PB_dom <- data.frame(
    Secteur = rownames(TEI_PB_dom_mat),
    TEI_PB_dom_mat,
    check.names = FALSE
  )
  TEI_PB_dom[TEI_PB_dom[[col_secteur]]=="NG1",-1] <-
    as.list(colSums(MC_CI, na.rm=T))
  
  TEI_PB_dom <- TEI_PB_dom %>%
    bind_rows(
      data.frame(
        Secteur = "Total colonnes",
        t(colSums(select(., -Secteur), na.rm = TRUE)),
        check.names = FALSE
      )
    ) %>%
    mutate(
      "Total lignes" = rowSums(select(., -Secteur), na.rm = TRUE)
    )
  
  return(list(
    TEI_PB = TEI_PB,
    TEI_PB_dom = TEI_PB_dom,
    ventilation_mat = ventilation_mat
      )
    )
}

# --- 9. Calcul de CF avec marges ---
calculer_CF_PB <- function(cf_filtre, MC_cf, TVA_cf, TN_cf, IMP_cf,
                           col_produit = "id_produit",
                           col_valeur = "CF",
                           exclure_produits = c("NZ1", "NZ2")) {
  
  CF_PB <- cf_filtre %>%
    filter(!.data[[col_produit]] %in% exclure_produits) %>%
    mutate(
      CF_PB = .data[[col_valeur]] - 
        coalesce(MC_cf[.data[[col_produit]]], 0) -
        coalesce(TVA_cf[.data[[col_produit]]], 0) -
        coalesce(TN_cf[.data[[col_produit]]], 0) - 
        coalesce(IMP_cf[.data[[col_produit]]], 0)
    )
  
  return(CF_PB)
}

# --- 10. Vérifications ---
verifier_TEI_PB <- function(TEI_pb) {
  
  lignes_produits <- TEI_pb$Secteur[!TEI_pb$Secteur %in% c("Total colonnes", "Cible colonne", "Cible ligne")]
  colonnes_branches <- names(TEI_pb)[!names(TEI_pb) %in% c("Secteur", "Total lignes", "Cible ligne")]
  
  somme_lignes <- sum(TEI_pb[TEI_pb$Secteur %in% lignes_produits, colonnes_branches], na.rm = TRUE)
  somme_colonnes <- sum(TEI_pb[TEI_pb$Secteur == "Total colonnes", colonnes_branches], na.rm = TRUE)
  equilibre <- somme_lignes - somme_colonnes
  
  return(list(
    equilibre = equilibre,
    somme_lignes = somme_lignes,
    somme_colonnes = somme_colonnes
  ))
}

# --- 11. Fonction principale (orchestrateur) ---
calculer_TES_PB <- function(TEI_pa,
                                 ERe,
                                 ERe_ref,
                                 Cf,
                                 VENTILATION,
                                 
                                 col_secteur = "Secteur",
                                 
                                 col_cf_produit = "id_produit",
                                 col_cf_valeur = "CF",
                                 
                                 col_eref_produit = "Pro- duit",
                                 col_eref_MC_ci = "Marges de commerce",
                                 col_eref_MC_cf = "CF Menages Commerc.",
                                 
                                 exclure_produits = c("NZ1", "NZ2"),
                                 verbose = TRUE) {
  
  if(verbose) {
    cat("\n" , paste(rep("=", 60), collapse = ""), "\n")
    cat("   CALCUL DU TES EN PRIX DE BASE\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }
  # 1. Préparer CI
  CI <- preparer_CI(TEI_pa, col_secteur, exclure_produits)
  CI_par_produit <- CI$CI_par_produit
  CI_mat <- CI$CI_mat
  produits_CI <- CI$produits_CI
  
  # 2. Préparer CF
  CF <- preparer_CF(cf = Cf,
                    col_produit = col_cf_produit,
                    col_valeur  = col_cf_valeur,
                    exclure_produits)
  CF_par_produit <- CF$CF_par_produit
  CF_filtre <- CF$CF_filtre
  produits_CF <- CF$produits_CF
  
  # 3. Calculer consommation totale
  conso <- calculer_conso_totale(ci_par_produit = CI_par_produit,
                                 cf_par_produit = CF_par_produit,
                                 produits_cf = produits_CF,
                                 produits_ci = produits_CI)
  conso_totale <- conso$conso_totale
  produits_communs <- conso$produits_communs
  
  # 4. Récupérer les taux
  taux_ERE <- recuperer_taux(Ere = ERe, exclure_produits)
  MC_ERE <- taux_ERE$MC
  MC_ERE[["NP1"]] <- 0
  TVA_ERE <- taux_ERE$TVA
  TVA_ERE[["NP1"]] <- 0
  TN_ERE <- taux_ERE$TN
  TN_ERE[["NP1"]] <- 0
  IMP_ERE <- taux_ERE$IMP
  IMP_ERE[["NP1"]] <- 0
  
  # 5. Calculer les taux de marge
  taux <- calculer_taux_marge(ci_mat = CI_mat,
                              cf_par_produit = CF_par_produit,
                              conso_tot = conso_totale, 
                              produits_ci = produits_CI,
                              produits_cf = produits_CF,
                              produits_comm = produits_communs)
  taux_CI <- taux$taux_CI
  taux_CF <- taux$taux_CF
  
  # 6. Créer la clé de répartition
  
  cle <- creer_cle_repartition(ere_ref = ERe_ref, 
                               produits_ci = produits_CI,
                               col_produit = col_eref_produit,
                               col_MC_ci = col_eref_MC_ci,
                               col_MC_cf = col_eref_MC_cf)
  
  # 7. Calculer les marges
  marges <- calculer_marges_produit(cle_repartition  = cle,
                                    MC_ere = MC_ERE)
  
  # 8. Calculer TEI_PB
  res_TEI_PB <- calculer_TEI_PB(TEI_pa = TEI_pa,
                            ci_mat = CI_mat,
                            MC_ci = marges$MC_CI,
                            ventilation = VENTILATION,
                            TVA_ci = sweep(taux_CI, 1, TVA_ERE, "*"),
                            TN_ci = sweep(taux_CI, 1, TN_ERE, "*"),
                            IMP_ci = sweep(taux_CI, 1, IMP_ERE, "*"),
                            col_secteur = col_secteur,
                            exclure_produits)
  TEI_PB <- res_TEI_PB$TEI_PB
  TEI_PB_dom <- res_TEI_PB$TEI_PB_dom
  ventilation_mat  <- res_TEI_PB$ventilation_mat
  
  # 9. Calculer CF_PB
  TN_CF <- TN_ERE
  TVA_CF <- TVA_ERE
  IMP_CF <- IMP_ERE
  for(prod in produits_communs){
    TN_CF[prod] = TN_ERE[prod] * taux_CF[prod]
    TVA_CF[prod] = TVA_ERE[prod] * taux_CF[prod]
    IMP_CF[prod] = IMP_ERE[prod] * taux_CF[prod]
  }
  CF_PB <- calculer_CF_PB(cf = CF_filtre,
                          MC_cf = marges$MC_CF,
                          TVA_cf = TVA_CF,
                          TN_cf = TN_CF,
                          IMP_cf = IMP_CF,
                          col_produit = col_cf_produit,
                          col_valeur = col_cf_valeur,
                          exclure_produits)
  
  # 10. Vérifications
  verif <- verifier_TEI_PB(TEI_PB)
  
  if(verbose) {
    cat("\n🔍 VÉRIFICATIONS\n")
    cat("  - Équilibre TEI_PB :", format(round(verif$equilibre, 0), big.mark = " "),
        if(abs(verif$equilibre) < 1) " ✅" else " ⚠️", "\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }
  
  return(list(
    TEI_PB = TEI_PB,
    TEI_PB_dom = TEI_PB_dom,
    CF_PB = CF_PB,
    CI_mat = CI_mat,
    CI_par_produit = CI_par_produit,
    CF_par_produit = CF_par_produit,
    conso_totale = conso_totale,
    taux_ERE = taux_ERE,
    taux_CI = taux_CI,
    taux_CF = taux_CF,
    cle_repartition = cle,
    marges = marges,
    marge_CI_produit = marges$MC_CI,
    marge_CF_produit = marges$MC_CF_,
    ventilation_mat = ventilation_mat,
    verification = verif,
    produits_communs = produits_communs
  ))
}

######################################
### EXPORTE LE TES
exporter_TES <- function(CI, CF, VA, fichier = "export_r.xlsx") {
  
  wb <- createWorkbook()
  addWorksheet(wb, "Tableau_r")
  
  # Définir les couleurs
  style_CI <- createStyle(fgFill = "#D6E4F0", border = "TopBottomLeftRight", borderColour = "#1F3864")
  style_CF <- createStyle(fgFill = "#E2EFDA", border = "TopBottomLeftRight", borderColour = "#2E75B6")
  style_VA <- createStyle(fgFill = "#FCE4D6", border = "TopBottomLeftRight", borderColour = "#7030A0")
  style_titre <- createStyle(fontSize = 12, textDecoration = "bold", halign = "center")
  
  # === 1. CI (en haut à gauche) ===
  writeData(wb, "Tableau_r", CI, startRow = 2, startCol = 2)
  addStyle(wb, "Tableau_r", style_CI, rows = 2:(2 + nrow(CI)), cols = 2:(2 + ncol(CI) - 1), gridExpand = TRUE)
  
  # === 2. CF (en haut à droite) ===
  decalage_col <- ncol(CI) + 3
  writeData(wb, "Tableau_r", CF, startRow = 2, startCol = decalage_col)
  addStyle(wb, "Tableau_r", style_CF, rows = 2:(2 + nrow(CF)), cols = decalage_col:(decalage_col + ncol(CF) - 1), gridExpand = TRUE)
  
  # === 3. VA (en bas à gauche, sous CI) ===
  decalage_ligne <- nrow(CI) + 4
  writeData(wb, "Tableau_r", VA, startRow = decalage_ligne, startCol = 2)
  addStyle(wb, "Tableau_r", style_VA, rows = decalage_ligne:(decalage_ligne + nrow(VA) - 1), cols = 2:(2 + ncol(VA) - 1), gridExpand = TRUE)
  
  # Titres
  writeData(wb, "Tableau_r", "CI", startRow = 1, startCol = 2)
  writeData(wb, "Tableau_r", "CF", startRow = 1, startCol = decalage_col)
  writeData(wb, "Tableau_r", "VA", startRow = decalage_ligne - 1, startCol = 2)
  addStyle(wb, "Tableau_r", style_titre, rows = 1, cols = c(2, decalage_col), gridExpand = TRUE)
  addStyle(wb, "Tableau_r", style_titre, rows = decalage_ligne - 1, cols = 2)
  
  # Ajuster les largeurs de colonnes
  setColWidths(wb, "Tableau_r", cols = 1:(decalage_col + ncol(CF)), widths = 12)
  
  saveWorkbook(wb, fichier, overwrite = TRUE)
  cat("Fichier exporté :", fichier, "\n")
}

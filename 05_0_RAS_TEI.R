# ==============================================================
#  RAS_TEI.R
#  Mise à jour d'un Tableau des Entrées Intermédiaires (TEI)
#  par la méthode RAS (Biproportion / Stone-Brown)
# ==============================================================
#
#  Structure du fichier Excel d'entrée (TEI_input.xlsx) :
#
#    Feuille "TEI" :
#      Ligne 1       : en-têtes (cellule A1 vide, B1..Nn = noms secteurs)
#      Lignes 2..n+1 : col. A = nom secteur, col. B..N = valeurs
#
#    Feuille "Cibles" :
#      Ligne 1       : en-têtes (Secteur | Cible_ligne | Cible_colonne)
#      Lignes 2..n+1 : cibles par secteur (mêmes noms que dans "TEI")
#
#  Sorties :
#    TEI_RAS_output.xlsx  avec 4 feuilles :
#      - TEI_Base      : matrice d'origine + cibles
#      - TEI_RAS       : matrice équilibrée par RAS
#      - Variations    : différences absolues et relatives
#      - Convergence   : historique des itérations
#
# ==============================================================


# ── 0. Packages ────────────────────────────────────────────────

for (pkg in c("openxlsx", "readxl")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}


# ── 1. Paramètres utilisateur ──────────────────────────────────

#setwd("C:/Users/P1NM9G/Documents/R&D")
FICHIER_ENTREE <- "TEI_input.xlsx"       # Fichier d'entrée
FICHIER_SORTIE <- "TEI_RAS_output.xlsx"  # Fichier de sortie
TOLERANCE      <- 1e-7                   # Seuil de convergence
MAX_ITER       <- 2000                   # Nombre max d'itérations


# ── 2. Lecture des données ──────────────────────────────────────

lire_donnees <- function(fichier) {
  
  if (!file.exists(fichier))
    stop("Fichier introuvable : ", fichier)
  
  # --- TEI de base ---
  tei_raw  <- as.data.frame(
    readxl::read_excel(fichier, sheet = "TEI", col_names = TRUE)
  )
  labels <- as.character(tei_raw[, 1])
  Z      <- as.matrix(tei_raw[, -1, drop = FALSE])
  Z      <- apply(Z, 2, as.numeric)
  rownames(Z) <- labels
  colnames(Z) <- labels
  
  if (any(is.na(Z)))
    warning("Des valeurs NA ont été trouvées dans le TEI de base. Remplacées par 0.")
  Z[is.na(Z)] <- 0
  
  if (any(Z < 0))
    warning("Valeurs négatives dans le TEI de base. La méthode RAS suppose des valeurs >= 0.")
  
  # --- Cibles marginales ---
  cibles <- as.data.frame(
    readxl::read_excel(fichier, sheet = "Cibles", col_names = TRUE)
  )
  u <- as.numeric(cibles[, 2])   # Cibles lignes (totaux ligne cibles)
  v <- as.numeric(cibles[, 3])   # Cibles colonnes (totaux colonne cibles)
  names(u) <- as.character(cibles[, 1])
  names(v) <- as.character(cibles[, 1])
  
  if (length(u) != nrow(Z) || length(v) != ncol(Z))
    stop("Le nombre de cibles ne correspond pas aux dimensions du TEI.")
  
  if (any(u < 0) || any(v < 0))
    warning("Des cibles négatives ont été détectées.")
  
  list(Z = Z, u = u, v = v, labels = labels)
}


# ── 3. Algorithme RAS ──────────────────────────────────────────

methode_ras <- function(Z, u, v, tol = 1e-7, max_iter = 2000) {
  
  # Vérification de la contrainte de cohérence (somme u ≈ somme v)
  ecart_sommes <- abs(sum(u) - sum(v))
  if (ecart_sommes > tol * max(sum(u), 1))
    warning(sprintf(
      "Somme cibles lignes (%.6g) ≠ somme cibles colonnes (%.6g) — écart : %.4g",
      sum(u), sum(v), ecart_sommes
    ))
  
  A   <- Z
  hst <- data.frame(
    Iteration    = integer(max_iter),
    Erreur_ligne = numeric(max_iter),
    Erreur_col   = numeric(max_iter),
    Erreur_max   = numeric(max_iter)
  )
  convergence <- FALSE
  iter_finale <- max_iter
  
  for (iter in seq_len(max_iter)) {
    
    # ── Étape R : mise à l'échelle des lignes ──
    ri     <- rowSums(A)
    r      <- ifelse(ri == 0, 0, u / ri)
    A      <- diag(r) %*% A
    
    # ── Étape S : mise à l'échelle des colonnes ──
    sj     <- colSums(A)
    s      <- ifelse(sj == 0, 0, v / sj)
    A      <- A %*% diag(s)
    
    # ── Calcul de l'erreur (norme infinie sur les marges) ──
    err_r  <- max(abs(rowSums(A) - u))
    err_c  <- max(abs(colSums(A) - v))
    err    <- max(err_r, err_c)
    
    hst[iter, ] <- c(iter, err_r, err_c, err)
    
    if (err < tol) {
      convergence <- TRUE
      iter_finale <- iter
      break
    }
  }
  
  if (!convergence) {
    warning(sprintf(
      "Convergence non atteinte après %d itérations. Erreur finale = %.4e",
      max_iter, err
    ))
  } else {
    cat(sprintf(
      "  ✓ Convergence en %d itération(s) — Erreur max finale = %.4e\n",
      iter_finale, err
    ))
  }
  
  rownames(A) <- rownames(Z)
  colnames(A) <- colnames(Z)
  
  list(
    matrice     = A,
    iterations  = iter_finale,
    erreur      = err,
    convergence = convergence,
    historique  = hst[seq_len(iter_finale), ]
  )
}


# ── 4. Helpers export Excel ─────────────────────────────────────

# Convertit un numéro de colonne (entier) en lettre(s) Excel (ex : 28 → "AB")
num_vers_col <- function(n) {
  res <- ""
  while (n > 0) {
    r   <- (n - 1L) %% 26L
    res <- paste0(LETTERS[r + 1L], res)
    n   <- (n - 1L) %/% 26L
  }
  res
}

palette <- list(
  bleu_fonce  = "#1F3864",
  bleu_moyen  = "#2E75B6",
  bleu_clair  = "#D6E4F0",
  vert_clair  = "#E2EFDA",
  violet      = "#7030A0",
  violet_bg   = "#F3E9FF",
  rouge_bg    = "#FCE4D6",
  orange_bg   = "#FCE4D6"
)

creer_styles <- function() {
  list(
    titre = createStyle(
      fontSize = 13, fontName = "Arial", fontColour = "#FFFFFF",
      fgFill = palette$bleu_fonce, textDecoration = "bold",
      halign = "center", valign = "center"
    ),
    header = createStyle(
      fontSize = 10, fontName = "Arial", fontColour = "#FFFFFF",
      fgFill = palette$bleu_moyen, textDecoration = "bold",
      halign = "center", valign = "center", wrapText = TRUE,
      border = "TopBottomLeftRight", borderColour = "#BDD7EE"
    ),
    label = createStyle(
      fontSize = 10, fontName = "Arial",
      textDecoration = "bold", fgFill = palette$bleu_clair,
      border = "TopBottomLeftRight", borderColour = "#BDD7EE"
    ),
    data = createStyle(
      fontSize = 10, fontName = "Arial",
      numFmt = "#,##0.00", halign = "right",
      border = "TopBottomLeftRight", borderColour = "#CCCCCC"
    ),
    total = createStyle(
      fontSize = 10, fontName = "Arial",
      numFmt = "#,##0.00", halign = "right",
      textDecoration = "bold", fgFill = palette$vert_clair,
      border = "TopBottomLeftRight", borderColour = "#A9D18E"
    ),
    cible = createStyle(
      fontSize = 10, fontName = "Arial",
      numFmt = "#,##0.00", halign = "right",
      fontColour = palette$violet, textDecoration = "bold",
      fgFill = palette$violet_bg,
      border = "TopBottomLeftRight", borderColour = "#C4A0E0"
    ),
    cible_hdr = createStyle(
      fontSize = 10, fontName = "Arial",
      fontColour = palette$violet, textDecoration = "bold",
      fgFill = palette$violet_bg,
      border = "TopBottomLeftRight", borderColour = "#C4A0E0"
    )
  )
}


# ── 5. Écriture d'une feuille TEI ──────────────────────────────

ecrire_feuille_tei <- function(wb, nom_feuille, Z_mat,
                               u = NULL, v = NULL, titre) {
  
  addWorksheet(wb, nom_feuille, gridLines = FALSE)
  
  n  <- nrow(Z_mat)
  nc <- ncol(Z_mat)
  st <- creer_styles()
  
  n_cols_total <- nc + 2 + (!is.null(u))   # data + Total + (Cible ligne)
  
  # ── Titre ──────────────────────────────────────────────────
  writeData(wb, nom_feuille, titre, startRow = 1, startCol = 1)
  mergeCells(wb, nom_feuille, cols = 1:n_cols_total, rows = 1)
  addStyle(wb, nom_feuille, st$titre, rows = 1, cols = 1:n_cols_total)
  setRowHeights(wb, nom_feuille, rows = 1, heights = 28)
  
  # ── En-têtes colonnes ──────────────────────────────────────
  writeData(wb, nom_feuille, "Secteur",       startRow = 2, startCol = 1)
  writeData(wb, nom_feuille, "Total lignes",  startRow = 2, startCol = nc + 2)
  for (j in seq_len(nc))
    writeData(wb, nom_feuille, colnames(Z_mat)[j], startRow = 2, startCol = j + 1)
  if (!is.null(u))
    writeData(wb, nom_feuille, "Cible ligne", startRow = 2, startCol = nc + 3)
  addStyle(wb, nom_feuille, st$header, rows = 2, cols = 1:n_cols_total)
  setRowHeights(wb, nom_feuille, rows = 2, heights = 40)
  
  # ── Données + totaux lignes ────────────────────────────────
  for (i in seq_len(n)) {
    ri <- i + 2
    
    writeData(wb, nom_feuille, rownames(Z_mat)[i], startRow = ri, startCol = 1)
    addStyle(wb, nom_feuille, st$label, rows = ri, cols = 1)
    
    for (j in seq_len(nc))
      writeData(wb, nom_feuille, Z_mat[i, j], startRow = ri, startCol = j + 1)
    addStyle(wb, nom_feuille, st$data, rows = ri, cols = 2:(nc + 1), gridExpand = TRUE)
    
    # Formule SUM pour le total ligne
    c_deb <- num_vers_col(2L)
    c_fin <- num_vers_col(nc + 1L)
    writeFormula(wb, nom_feuille,
                 sprintf("=SUM(%s%d:%s%d)", c_deb, ri, c_fin, ri),
                 startRow = ri, startCol = nc + 2)
    addStyle(wb, nom_feuille, st$total, rows = ri, cols = nc + 2)
    
    if (!is.null(u)) {
      writeData(wb, nom_feuille, u[i], startRow = ri, startCol = nc + 3)
      addStyle(wb, nom_feuille, st$cible, rows = ri, cols = nc + 3)
    }
  }
  
  # ── Ligne totaux colonnes ──────────────────────────────────
  r_tot <- n + 3
  writeData(wb, nom_feuille, "Total colonnes", startRow = r_tot, startCol = 1)
  addStyle(wb, nom_feuille, st$total, rows = r_tot, cols = 1)
  
  for (j in seq_len(nc)) {
    cj <- num_vers_col(j + 1L)
    writeFormula(wb, nom_feuille,
                 sprintf("=SUM(%s3:%s%d)", cj, cj, n + 2),
                 startRow = r_tot, startCol = j + 1)
  }
  c_tot <- num_vers_col(nc + 2L)
  writeFormula(wb, nom_feuille,
               sprintf("=SUM(%s3:%s%d)", c_tot, c_tot, n + 2),
               startRow = r_tot, startCol = nc + 2)
  addStyle(wb, nom_feuille, st$total, rows = r_tot, cols = 2:(nc + 2))
  
  # ── Ligne cibles colonnes ──────────────────────────────────
  if (!is.null(v)) {
    r_cib <- n + 4
    writeData(wb, nom_feuille, "Cible colonne", startRow = r_cib, startCol = 1)
    for (j in seq_len(nc))
      writeData(wb, nom_feuille, v[j], startRow = r_cib, startCol = j + 1)
    # Total des cibles (formule)
    c_deb2 <- num_vers_col(2L)
    c_fin2 <- num_vers_col(nc + 1L)
    writeFormula(wb, nom_feuille,
                 sprintf("=SUM(%s%d:%s%d)", c_deb2, r_cib, c_fin2, r_cib),
                 startRow = r_cib, startCol = nc + 2)
    addStyle(wb, nom_feuille, st$cible, rows = r_cib, cols = 1:(nc + 2))
    if (!is.null(u))
      addStyle(wb, nom_feuille, st$cible_hdr, rows = r_cib, cols = nc + 3)
  }
  
  # ── Mise en forme colonnes + volets ───────────────────────
  setColWidths(wb, nom_feuille, cols = 1,         widths = 22)
  setColWidths(wb, nom_feuille, cols = 2:(nc + 3), widths = 14)
  freezePane(wb, nom_feuille, firstActiveRow = 3, firstActiveCol = 2)
}


# ── 6. Feuille Variations ──────────────────────────────────────

ecrire_feuille_variations <- function(wb, Z_base, Z_ras) {
  
  addWorksheet(wb, "Variations", gridLines = FALSE)
  
  n  <- nrow(Z_base)
  nc <- ncol(Z_base)
  
  s_hdr <- createStyle(
    fontSize = 11, fontName = "Arial", fontColour = "#FFFFFF",
    fgFill = palette$bleu_fonce, textDecoration = "bold",
    halign = "left", border = "Bottom", borderColour = "#CCCCCC"
  )
  s_pos <- createStyle(
    fontName = "Arial", fontSize = 10, numFmt = "#,##0.00",
    halign = "right", fgFill = "#E2EFDA",
    border = "TopBottomLeftRight", borderColour = "#CCCCCC"
  )
  s_neg <- createStyle(
    fontName = "Arial", fontSize = 10, numFmt = "#,##0.00",
    halign = "right", fgFill = "#FCE4D6",
    border = "TopBottomLeftRight", borderColour = "#CCCCCC"
  )
  s_pct_pos <- createStyle(
    fontName = "Arial", fontSize = 10, numFmt = "0.00%",
    halign = "right", fgFill = "#E2EFDA",
    border = "TopBottomLeftRight", borderColour = "#CCCCCC"
  )
  s_pct_neg <- createStyle(
    fontName = "Arial", fontSize = 10, numFmt = "0.00%",
    halign = "right", fgFill = "#FCE4D6",
    border = "TopBottomLeftRight", borderColour = "#CCCCCC"
  )
  s_lbl <- createStyle(
    fontName = "Arial", fontSize = 10, textDecoration = "bold",
    fgFill = palette$bleu_clair,
    border = "TopBottomLeftRight", borderColour = "#BDD7EE"
  )
  s_col_hdr <- createStyle(
    fontName = "Arial", fontSize = 10, fontColour = "#FFFFFF",
    fgFill = palette$bleu_moyen, textDecoration = "bold",
    halign = "center", border = "TopBottomLeftRight", borderColour = "#BDD7EE"
  )
  
  ecrire_bloc <- function(titre, mat, styles_pos, styles_neg, row_offset, fmt_pct = FALSE) {
    writeData(wb, "Variations", titre, startRow = row_offset, startCol = 1)
    mergeCells(wb, "Variations", cols = 1:(nc + 1), rows = row_offset)
    addStyle(wb, "Variations", s_hdr, rows = row_offset, cols = 1:(nc + 1))
    setRowHeights(wb, "Variations", rows = row_offset, heights = 22)
    
    # En-têtes
    writeData(wb, "Variations", "Secteur", startRow = row_offset + 1, startCol = 1)
    for (j in seq_len(nc))
      writeData(wb, "Variations", colnames(mat)[j],
                startRow = row_offset + 1, startCol = j + 1)
    addStyle(wb, "Variations", s_col_hdr,
             rows = row_offset + 1, cols = 1:(nc + 1))
    
    for (i in seq_len(n)) {
      ri <- row_offset + 1 + i
      writeData(wb, "Variations", rownames(mat)[i], startRow = ri, startCol = 1)
      addStyle(wb, "Variations", s_lbl, rows = ri, cols = 1)
      for (j in seq_len(nc)) {
        val <- mat[i, j]
        writeData(wb, "Variations", val, startRow = ri, startCol = j + 1)
        s <- if (is.na(val) || val >= 0) styles_pos else styles_neg
        addStyle(wb, "Variations", s, rows = ri, cols = j + 1)
      }
    }
  }
  
  Z_abs <- Z_ras - Z_base
  Z_rel <- ifelse(Z_base == 0, NA_real_, (Z_ras - Z_base) / Z_base)
  
  ecrire_bloc("Variations absolues (TEI_RAS − TEI_Base)",
              Z_abs, s_pos, s_neg, row_offset = 1)
  
  ecrire_bloc("Variations relatives (TEI_RAS − TEI_Base) / TEI_Base",
              Z_rel, s_pct_pos, s_pct_neg,
              row_offset = n + 5, fmt_pct = TRUE)
  
  setColWidths(wb, "Variations", cols = 1,         widths = 22)
  setColWidths(wb, "Variations", cols = 2:(nc + 1), widths = 14)
}


# ── 7. Feuille Convergence ─────────────────────────────────────

ecrire_feuille_convergence <- function(wb, res, tol) {
  
  addWorksheet(wb, "Convergence", gridLines = FALSE)
  
  couleur_titre <- if (res$convergence) "#375623" else "#C00000"
  msg <- if (res$convergence) {
    sprintf("Convergence atteinte — %d itération(s)  |  Erreur finale : %.3e  |  Tolérance : %.3e",
            res$iterations, res$erreur, tol)
  } else {
    sprintf("ATTENTION : non convergence après %d itérations  |  Erreur finale : %.3e",
            res$iterations, res$erreur)
  }
  
  writeData(wb, "Convergence", msg, startRow = 1, startCol = 1)
  mergeCells(wb, "Convergence", cols = 1:4, rows = 1)
  addStyle(wb, "Convergence",
           createStyle(fontSize = 12, fontName = "Arial",
                       fontColour = "#FFFFFF", fgFill = couleur_titre,
                       textDecoration = "bold", halign = "left",
                       valign = "center"),
           rows = 1, cols = 1:4)
  setRowHeights(wb, "Convergence", rows = 1, heights = 26)
  
  s_hdr <- createStyle(
    fontSize = 10, fontName = "Arial", fontColour = "#FFFFFF",
    fgFill = palette$bleu_moyen, textDecoration = "bold",
    halign = "center", border = "TopBottomLeftRight", borderColour = "#BDD7EE"
  )
  s_iter <- createStyle(
    fontSize = 10, fontName = "Arial", halign = "center",
    border = "TopBottomLeftRight", borderColour = "#CCCCCC"
  )
  s_err  <- createStyle(
    fontSize = 10, fontName = "Arial", numFmt = "0.000E+00",
    halign = "right",
    border = "TopBottomLeftRight", borderColour = "#CCCCCC"
  )
  
  nr <- nrow(res$historique)
  writeData(wb, "Convergence", res$historique, startRow = 3, startCol = 1,
            rowNames = FALSE)
  addStyle(wb, "Convergence", s_hdr,  rows = 3,          cols = 1:4)
  addStyle(wb, "Convergence", s_iter, rows = 4:(nr + 3), cols = 1)
  addStyle(wb, "Convergence", s_err,  rows = 4:(nr + 3), cols = 2:4,
           gridExpand = TRUE)
  
  setColWidths(wb, "Convergence", cols = 1:4, widths = 22)
  freezePane(wb, "Convergence", firstActiveRow = 4, firstActiveCol = 1)
}


# ── 8. Export complet ──────────────────────────────────────────

exporter_resultats <- function(donnees, res, fichier_sortie) {
  
  wb <- createWorkbook()
  modifyBaseFont(wb, fontSize = 10, fontName = "Arial")
  
  # Feuille 1 : TEI de base avec cibles
  ecrire_feuille_tei(
    wb, "TEI_Base", donnees$Z,
    u      = donnees$u,
    v      = donnees$v,
    titre  = "TEI de base — Données d'origine"
  )
  
  # Feuille 2 : TEI équilibré RAS
  ecrire_feuille_tei(
    wb, "TEI_RAS", res$matrice,
    u      = donnees$u,
    v      = donnees$v,
    titre  = sprintf("TEI équilibré — Méthode RAS (%d itération(s))", res$iterations)
  )
  
  # Feuille 3 : Variations
  ecrire_feuille_variations(wb, donnees$Z, res$matrice)
  
  # Feuille 4 : Convergence
  ecrire_feuille_convergence(wb, res, TOLERANCE)
  
  saveWorkbook(wb, fichier_sortie, overwrite = TRUE)
  cat(sprintf("  ✓ Fichier exporté : %s\n", fichier_sortie))
}


# ── 9. Programme principal ─────────────────────────────────────

cat("============================================\n")
cat("  Mise à jour TEI — Méthode RAS\n")
cat("============================================\n\n")

cat(sprintf("Lecture de : %s\n", FICHIER_ENTREE))
donnees <- lire_donnees(FICHIER_ENTREE)

n_sec <- nrow(donnees$Z)
cat(sprintf("  Matrice chargée  : %d × %d secteurs\n", n_sec, n_sec))
cat(sprintf("  Somme TEI base   : %.4g\n", sum(donnees$Z)))
cat(sprintf("  Somme cibles u   : %.4g\n", sum(donnees$u)))
cat(sprintf("  Somme cibles v   : %.4g\n", sum(donnees$v)))

cat("\nExécution de l'algorithme RAS...\n")
cat(sprintf("  Tolérance : %.2e  |  Max itérations : %d\n\n", TOLERANCE, MAX_ITER))
res <- methode_ras(donnees$Z, donnees$u, donnees$v,
                   tol = TOLERANCE, max_iter = MAX_ITER)

cat(sprintf("\n  Contrôle des marges après RAS :\n"))
cat(sprintf("    Max |somme_ligne  - u| : %.4e\n", max(abs(rowSums(res$matrice) - donnees$u))))
cat(sprintf("    Max |somme_col   - v| : %.4e\n", max(abs(colSums(res$matrice) - donnees$v))))

cat(sprintf("\nExport des résultats dans : %s\n", FICHIER_SORTIE))
exporter_resultats(donnees, res, FICHIER_SORTIE)

cat("\n============================================\n")
cat("  Terminé avec succès !\n")
cat("============================================\n")


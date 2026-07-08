# Guide d'utilisation du script de construction du TES

## 📋 Table des matières

1.  [Présentation](#présentation)
2.  [Structure du projet](#structure-du-projet)
3.  [Prérequis](#prérequis)
4.  [Flux de traitement](#flux-de-traitement)
5.  [Exécution](#exécution)
6.  [Fichiers d'entrée](#fichiers-dentrée)
7.  [Fichiers de sortie](#fichiers-de-sortie)
8.  [Vérifications et contrôles](#vérifications-et-contrôles)
9.  [Dépannage](#dépannage)
10. [Annexes](#annexes)

------------------------------------------------------------------------

## Présentation {#présentation}

Ce projet implémente une chaîne de traitement complète pour la construction d'un **Tableau Entrées-Sorties (TES)** en prix de base à partir :

1.  D'une base rétropolée des comptes nationaux
2.  D'un TEI (Tableau des Entrées Intermédiaires) de référence
3.  Des données ERE (Emplois-Ressources) et de la Consommation Finale

L'approche utilise : - **L'algorithme RAS** (Biproportion / Stone-Brown) pour l'équilibrage du TEI - Une **méthode de désagrégation** pour le passage des prix d'acquisition aux prix de base - Une **ventilation sectorielle** des marges commerciales et des impôts

------------------------------------------------------------------------

## Structure du projet {#structure-du-projet}
```text
projet_TES/
│
├── 000_fonctions.R                      # Fonctions utilitaires
├── 00_run_all.R                         # Script d'exécution complet
├── 01_chargement_packages_chemins.R     # Étape 1 : Configuration
├── 02_chargement_retropolee.R           # Étape 2 : Chargement base rétropolée
├── 03_arbitrage_NB1_ND2.R               # Étape 3 : Fusion NB1/ND2
├── 04_cibles_et_TEI_ref.R               # Étape 4 : Cibles et TEI référence
├── 05_0_RAS_TEI.R                       # Algorithme RAS
├── 05_1_export_TEI_input.R              # Étape 5 : Export pour RAS
├── 06_chargement_TEI_RAS.R              # Étape 6 : Chargement TEI RAS
├── 07_calcul_CF.R                       # Étape 7 : Consommation finale
├── 08_matrice_ventilation.R             # Étape 8 : Matrice de ventilation
├── 09_conversion_TEI_PB.R               # Étape 9 : Conversion prix de base
├── 10_tableau_production.R              # Étape 10 : Tableau de production
├── 11_calcul_VA_et_composantes.R        # Étape 11 : Valeur ajoutée
├── 12_export_TES.R                      # Étape 12 : Export TES final
│
└── README.md                           # Ce fichier
```

## Prérequis {#prérequis}

### Logiciels requis

-   **R** (version 3.6 ou supérieure)
-   **RStudio** (recommandé)

### Packages R nécessaires

```r
install.packages(c( "dplyr", "readxl", "tidyr", "writexl", "openxlsx" ))
```
### Emplacement des bases de données
Toutes les tables dont vous aurez besoin doivent se trouver dans unique dossier. Vous aurez besoin de:
-  Une base rétropolée des comptes économiques qui contient : les identifiants des opérations, des produits, des branches, des attributs méthodologiques ; une colonne par année dont le contenu est un numérique.
-  Un TEI de référence afin de calculer via RAS le TEI en prix de base.
-  Un ERE de référence afin de calculer une clé de répartition entre consommation intermédiaire et consommation finale de la marge de commerce.

## Exécution du code
### Configuration
1.  Entrez le chemin qui mène aux fichiers code dans 00_run_all.R ("chemin_fichier")
2.  Entrez le chemin qui mène aux bases de données dans 01_chargement_packages_chemins.R ("chemin")

### Lancement
Exécutez tout le fichier 000_run_all.R, le TES sera enregistré à l'endroit où mène le chemin que vous avez saisi dans 01_chargement_packages_chemins.R.

### Remarque importante
Notons que certains arbitrages ont été menés afin de garantir la conhérence des données obtenues concernant les comptes de Martinique entre 1996 et 2019. Ces arbitrages ne sont pas nécessairement adaptés à d'autres régions.

## Flux de traitement du TES
```text
PIPELINE DE CONSTRUCTION DU TES
================================

+----------------------------+
| Etape 1-2 : Chargement     |
| des données                |
+------------+---------------+
             |
             v
+------------+---------------+
| Base retropolee            |
+------------+---------------+
             |
             v
+------------+---------------+
| Etape 3 : Fusion NB1/ND2   |
+------------+---------------+
             |
             v
+------------+---------------+
| retro_long                 |
| (format longitudinal)      |
+------------+---------------+
             |
             v
+------------+---------------+     +-------------------------+
| Etape 4 : Cibles CB/ERE    |     | TEI de référence        |
| et TEI référence           |     |                         |
+------------+---------------+     +-----------+-------------+
             |                                 |
             +---------------------------------+
                               |
                               v
+------------------------------+---------------+
| Etape 5 : Export TEI_input.xlsx             |
+------------------------------+---------------+
                               |
                               v
+------------------------------+---------------+
| RAS_TEI.R                                   |
+------------------------------+---------------+
                               |
                               v
+------------------------------+---------------+
| TEI_RAS_output.xlsx                         |
+------------------------------+---------------+
                               |
                               v
+------------------------------+---------------+
| Etape 6 : Chargement TEI RAS                |
+------------------------------+---------------+
                               |
                               v
+------------------------------+---------------+
| Etape 9 : Conversion PB                     |
+------------------------------+---------------+
                               |
                               v
+--------------+---------------+-------------+
|              |                             |
v              v                             v
+--------+  +--------+  +-----------------------------------+
|Etape 7 |  |Etape 8 |  | Etape 10-11 : VA                 |
|CF      |  |Ventil. |  |                                   |
+---+----+  +----+---+  +-----------------+-----------------+
    |            |                        |
    +------------+------------------------+
                 |
                 v
+----------------+-----------------------+
| Etape 12 : Export TES_final.xlsx       |
+----------------------------------------+
```

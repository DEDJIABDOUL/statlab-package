# Campagne de tests sur corpus reel

Ce repertoire n'est pas une suite `testthat` : c'est un outil de validation
manuelle, a executer sur un corpus de fichiers reels (anonymises) pour
repondre a la question "le MVP est-il reellement livrable ?" au-dela des
jeux de test synthetiques du package.

## Utilisation

```r
setwd("tests/corpus")
source("run_corpus.R")
run_corpus_tests()
```

Ouvrir ensuite `corpus_report.html`.

## Etapes

1. **Deposer des fichiers reels** dans `corpus/` (voir `corpus/README.md`).
2. **Premiere execution**, sans annotation : le rapport indique deja les
   echecs de lecture, les lignes d'en-tete detectees, les natures
   inferees et les anomalies detectees, mais sans taux de concordance
   (colonnes "non annote").
3. **Remplir `attendu.csv`** a partir de ce que vous savez de chaque
   fichier (voir format ci-dessous), puis relancer : le rapport calcule
   alors les taux de concordance reels.
4. **Remplir `faux_positifs.csv`** en repérant, parmi les anomalies
   listees dans le rapport, celles qui sont des faux positifs, puis
   relancer : le rapport calcule alors le taux de faux positifs.

## Format de `attendu.csv`

CSV separe par `;`, colonnes :

| Colonne                  | Contenu                                                                 |
|---------------------------|--------------------------------------------------------------------------|
| `fichier`                 | Nom du fichier (`basename`, tel qu'il apparait dans `corpus/`).          |
| `variable`                | Nom de la variable annotee (vide/omise pour une ligne "en-tete seul").   |
| `nature_attendue`         | Nature correcte, parmi celles de `dictionnaire.nature` (voir le schema `inst/schema/config_schema.yml`). |
| `ligne_entete_attendue`   | Numero (1-based) de la vraie ligne d'en-tete du fichier.                 |

`ligne_entete_attendue` doit etre repete sur chaque ligne du meme fichier
(y compris les lignes qui n'annotent qu'une variable) : c'est une
redondance volontaire, plus simple a remplir dans un tableur qu'un format
a deux sections.

Exemple :

```
fichier;variable;nature_attendue;ligne_entete_attendue
inclusion_2023.xlsx;age;continue;3
inclusion_2023.xlsx;sexe;binaire;3
inclusion_2023.xlsx;;;3
```

(La derniere ligne, sans `variable`, sert uniquement a couvrir un fichier
dont on ne souhaite annoter que la ligne d'en-tete.)

## Format de `faux_positifs.csv`

CSV separe par `;`, colonnes `fichier;check_id;variable;faux_positif`
(`faux_positif` : `TRUE`/`VRAI`/`1`/`oui` pour un faux positif). Les
identifiants `check_id` sont ceux du rapport (`missing_codes`,
`duplicate_rows`, `impossible_values`, etc. — voir `R/anomalies.R`).

## Ce que mesure le rapport

- Lecture reussie oui/non, et la cause de l'echec le cas echeant.
- Ligne d'en-tete detectee, et concordance avec `attendu.csv`.
- Natures inferees, taux de concordance avec `attendu.csv`.
- Anomalies detectees, taux de faux positifs (annotes dans
  `faux_positifs.csv`).
- Tableau 1 produit oui/non, sur un dictionnaire construit automatiquement
  a partir du profilage (pas d'un `config.yml` reel : cette campagne
  mesure la robustesse de la chaine face a des fichiers varies, pas la
  pertinence d'une analyse).
- Temps d'execution par etape (lecture, profilage, anomalies, tableau 1).

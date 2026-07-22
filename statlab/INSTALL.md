# Installation de statlab sur une machine neuve

`statlab` est un outil **local** : aucune installation serveur, aucun compte,
aucun appel reseau a l'execution. Cette page couvre l'installation complete,
de R nu jusqu'a la commande `statlab` disponible dans le terminal.

## 1. R

`statlab` necessite R **4.3 ou plus recent**.

- **Windows** : telecharger et installer R depuis <https://cran.r-project.org/bin/windows/base/>.
- **macOS** : telecharger depuis <https://cran.r-project.org/bin/macosx/>, ou
  `brew install r`.
- **Linux (Debian/Ubuntu)** : `sudo apt install r-base`.

Verifier l'installation :

```sh
Rscript --version
```

## 2. Dependances systeme (rendu des rapports)

La generation des rapports (`statlab auditer`, `statlab rapporter`) delegue
le rendu a **Quarto**, et le format PDF necessite en plus une distribution
**LaTeX**. Le format Word (docx) n'a besoin d'aucune dependance
supplementaire au-dela de Quarto.

### Quarto

- **Windows** : `winget install --id Posit.Quarto`, ou installeur depuis
  <https://quarto.org/docs/get-started/>.
- **macOS** : `brew install quarto`.
- **Linux** : suivre <https://quarto.org/docs/get-started/> (paquet `.deb`/`.rpm`).

Verifier :

```sh
quarto --version
```

### LaTeX (necessaire uniquement pour le format PDF)

La distribution **TinyTeX** (legere, geree par Quarto/R) est recommandee
plutot qu'une installation TeX Live complete :

```sh
quarto install tinytex
```

Verifier :

```sh
quarto check
```

## 3. Dependances R du package

Le depot fournit un fichier `renv.lock` figeant les versions exactes de
toutes les dependances (reproductibilite du developpement, pas seulement de
l'analyse). Depuis la racine du depot :

```r
install.packages("renv")
renv::restore()
```

Cette etape installe notamment `checkmate`, `cli`, `yaml`, `readr`,
`readxl`, `dplyr`, `tidyr`, `forcats`, `powerjoin`, `ggplot2`, `gtsummary`,
`flextable`, `quarto`, `sessioninfo`, `optparse`, et les autres packages
publics du CRAN utilises par `statlab` (voir `DESCRIPTION`, champ
`Imports`).

## 4. Installation du package statlab

Depuis la racine du depot :

```r
install.packages(".", repos = NULL, type = "source")
```

ou, de maniere equivalente, en ligne de commande :

```sh
R CMD INSTALL .
```

Verifier que le package est visible (dans une **nouvelle** session R, sans
`devtools::load_all()`) :

```r
requireNamespace("statlab", quietly = TRUE)
```

Doit retourner `TRUE`. Si `FALSE` alors que l'installation ci-dessus a
reussi sans erreur, verifier que la bibliotheque R cible de
`R CMD INSTALL` fait bien partie de `.libPaths()` de la session utilisee
(cas frequent si `renv` est actif pour un *autre* projet : desactiver
`renv` pour cette session, ou installer `statlab` dans la bibliotheque
utilisateur standard avec `R CMD INSTALL --library=<chemin>`).

## 5. Mise en place de la commande `statlab` dans le PATH

Le point d'entree de la ligne de commande est le fichier
`inst/bin/statlab`, qui devient, apres installation du package,
`<bibliotheque_R>/statlab/bin/statlab` (et `.../statlab/bin/statlab.bat`
sur Windows). Retrouver ce chemin :

```r
system.file("bin", package = "statlab")
```

### Windows

Le fichier `statlab.bat` du meme repertoire est le point d'entree a
utiliser. Deux options :

1. **Ajouter le repertoire au PATH** (recommande) : Parametres Windows →
   *Variables d'environnement* → variable `Path` (utilisateur) → *Nouveau*
   → coller le chemin retourne par `system.file("bin", package = "statlab")`.
   Ouvrir un nouveau terminal pour que le changement prenne effet.
2. **Copier `statlab.bat` et `statlab`** dans un repertoire deja present
   dans le PATH (ex : un dossier personnel deja ajoute au PATH).

### macOS / Linux

Creer un lien symbolique executable vers un repertoire deja present dans
le PATH (`/usr/local/bin` ou `~/.local/bin`) :

```sh
BIN_DIR=$(Rscript -e 'cat(system.file("bin", package = "statlab"))')
chmod +x "$BIN_DIR/statlab"
ln -s "$BIN_DIR/statlab" /usr/local/bin/statlab
```

(`sudo` peut etre necessaire selon les droits sur `/usr/local/bin`. Pour
une installation sans droits administrateur, utiliser `~/.local/bin` a la
place, en s'assurant que ce repertoire figure dans le `PATH` du shell —
`export PATH="$HOME/.local/bin:$PATH"` dans `~/.bashrc`/`~/.zshrc`.)

## 6. Verification finale

```sh
statlab --help
statlab regles
```

La premiere commande doit afficher la liste des commandes disponibles
(`auditer`, `config`, `analyser`, `rapporter`, `valider`, `regles`) ; la
seconde doit afficher le referentiel methodologique embarque dans le
package, sans necessiter de projet ni de fichier `config.yml`.

Pour un premier essai complet sur un projet reel :

```sh
statlab config --sources=donnees_brutes/inclusion.xlsx --sortie=config.yml
statlab valider --config=config.yml
statlab auditer --config=config.yml
statlab rapporter --config=config.yml --formats=docx,pdf
```

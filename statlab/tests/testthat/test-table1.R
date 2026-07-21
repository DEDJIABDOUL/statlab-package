# Helpers -----------------------------------------------------------------

.projet_table1 <- function() {
  repertoire <- tempfile("statlab_table1_")
  dir.create(repertoire)
  st_log_init(repertoire)
  repertoire
}

.fake_config_table1 <- function(dictionnaire, tableau_1, decimales = 1L) {
  structure(
    list(
      dictionnaire = dictionnaire,
      analyse = list(tableau_1 = tableau_1),
      rendu = list(decimales = decimales)
    ),
    class = "statlab_config",
    valid = TRUE
  )
}

# Tests : construction de base --------------------------------------------

test_that("st_table1 construit un tableau sans stratification", {
  repertoire <- .projet_table1()
  set.seed(1)
  data <- data.frame(
    age = stats::rnorm(40, 50, 5),
    sexe = sample(c("H", "F"), 40, TRUE),
    stringsAsFactors = FALSE
  )
  config <- .fake_config_table1(
    list(age = list(nature = "continue", libelle = "Age", unite = "annees"), sexe = list(nature = "binaire", libelle = "Sexe")),
    list(variables = c("age", "sexe"))
  )

  tbl <- st_table1(data, config)
  expect_s3_class(tbl, "gtsummary")

  corps <- as.data.frame(tbl$table_body)
  expect_true("Age (annees)" %in% corps$label)
  expect_false("p.value" %in% names(corps))
})

test_that("st_table1 utilise la moyenne/ecart-type pour une variable normale", {
  repertoire <- .projet_table1()
  set.seed(2)
  data <- data.frame(age = stats::rnorm(60, 50, 5), stringsAsFactors = FALSE)
  config <- .fake_config_table1(
    list(age = list(nature = "continue", libelle = "Age")),
    list(variables = "age")
  )

  tbl <- st_table1(data, config)
  corps <- as.data.frame(tbl$table_body)
  ligne <- corps[corps$variable == "age" & corps$row_type == "label", ]
  expect_match(ligne$stat_0, "\\(")
  expect_false(grepl("\\[", ligne$stat_0))
})

test_that("st_table1 utilise la mediane [Q1-Q3] pour une variable non normale", {
  repertoire <- .projet_table1()
  set.seed(3)
  data <- data.frame(score = stats::rexp(60), stringsAsFactors = FALSE)
  config <- .fake_config_table1(
    list(score = list(nature = "continue", libelle = "Score")),
    list(variables = "score")
  )

  tbl <- st_table1(data, config)
  corps <- as.data.frame(tbl$table_body)
  ligne <- corps[corps$variable == "score" & corps$row_type == "label", ]
  expect_match(ligne$stat_0, "\\[")
})

test_that("st_table1 respecte l'ordre des modalites declare dans le dictionnaire", {
  repertoire <- .projet_table1()
  data <- data.frame(stade = sample(c("I", "II", "III", "IV"), 40, TRUE), stringsAsFactors = FALSE)
  config <- .fake_config_table1(
    list(stade = list(nature = "ordinale", libelle = "Stade", modalites = c("IV", "III", "II", "I"))),
    list(variables = "stade")
  )

  tbl <- st_table1(data, config)
  corps <- as.data.frame(tbl$table_body)
  niveaux <- corps$label[corps$variable == "stade" & corps$row_type == "level"]
  expect_equal(niveaux, c("IV", "III", "II", "I"))
})

# Tests : stratification et p-value ----------------------------------------

test_that("st_table1 ajoute une colonne de p-value coherente avec st_compare quand une stratification est declaree", {
  repertoire <- .projet_table1()
  set.seed(4)
  data <- data.frame(
    age = c(stats::rnorm(20, 50, 5), stats::rnorm(20, 60, 5)),
    groupe = rep(c("A", "B"), each = 20),
    stringsAsFactors = FALSE
  )
  config <- .fake_config_table1(
    list(age = list(nature = "continue", libelle = "Age"), groupe = list(nature = "binaire", libelle = "Groupe")),
    list(variables = "age", stratification = "groupe")
  )

  tbl <- st_table1(data, config)
  corps <- as.data.frame(tbl$table_body)
  ligne <- corps[corps$variable == "age", ]

  attendu <- st_compare(data, "age", "groupe", config)
  expect_equal(ligne$p.value[1], attendu$p_value)
  expect_true(all(c("stat_0", "stat_1", "stat_2") %in% names(corps)))
})

test_that("st_table1 s'arrete si la variable de stratification est introuvable", {
  repertoire <- .projet_table1()
  data <- data.frame(age = stats::rnorm(20), stringsAsFactors = FALSE)
  config <- .fake_config_table1(
    list(age = list(nature = "continue", libelle = "Age")),
    list(variables = "age", stratification = "absente")
  )

  expect_error(st_table1(data, config), "introuvable")
})

# Tests : denominateur des pourcentages -------------------------------------

test_that("st_table1 exclut les manquants du denominateur par defaut", {
  repertoire <- .projet_table1()
  data <- data.frame(sexe = c(rep("H", 8), rep("F", 2), rep(NA_character_, 10)), stringsAsFactors = FALSE)
  config <- .fake_config_table1(
    list(sexe = list(nature = "binaire", libelle = "Sexe")),
    list(variables = "sexe", denominateur = "exclure_manquants")
  )

  tbl <- st_table1(data, config)
  corps <- as.data.frame(tbl$table_body)
  ligne_h <- corps[corps$variable == "sexe" & corps$label == "H", ]
  expect_match(ligne_h$stat_0, "80%")
})

test_that("st_table1 inclut les manquants dans le denominateur si declare", {
  repertoire <- .projet_table1()
  data <- data.frame(sexe = c(rep("H", 8), rep("F", 2), rep(NA_character_, 10)), stringsAsFactors = FALSE)
  config <- .fake_config_table1(
    list(sexe = list(nature = "binaire", libelle = "Sexe")),
    list(variables = "sexe", denominateur = "inclure_manquants")
  )

  tbl <- st_table1(data, config)
  corps <- as.data.frame(tbl$table_body)
  ligne_h <- corps[corps$variable == "sexe" & corps$label == "H", ]
  expect_match(ligne_h$stat_0, "40%")
})

# Tests : conversions ---------------------------------------------------------

test_that("st_table1_flextable convertit en flextable natif", {
  repertoire <- .projet_table1()
  data <- data.frame(age = stats::rnorm(20, 50, 5), stringsAsFactors = FALSE)
  config <- .fake_config_table1(list(age = list(nature = "continue", libelle = "Age")), list(variables = "age"))

  tbl <- st_table1(data, config)
  ft <- st_table1_flextable(tbl)
  expect_s3_class(ft, "flextable")
})

test_that("st_table1_csv retourne un data.frame exploitable", {
  repertoire <- .projet_table1()
  data <- data.frame(age = stats::rnorm(20, 50, 5), stringsAsFactors = FALSE)
  config <- .fake_config_table1(list(age = list(nature = "continue", libelle = "Age")), list(variables = "age"))

  tbl <- st_table1(data, config)
  csv <- st_table1_csv(tbl)
  expect_s3_class(csv, "data.frame")
  expect_true(nrow(csv) > 0)
})

# Tests : erreurs ------------------------------------------------------------

test_that("st_table1 s'arrete si 'analyse.tableau_1' n'est pas declare", {
  repertoire <- .projet_table1()
  data <- data.frame(age = stats::rnorm(20), stringsAsFactors = FALSE)
  config <- structure(
    list(dictionnaire = list(age = list(nature = "continue")), analyse = NULL),
    class = "statlab_config", valid = TRUE
  )

  expect_error(st_table1(data, config), "tableau_1")
})

test_that("st_table1 s'arrete si une variable declaree est introuvable dans les donnees", {
  repertoire <- .projet_table1()
  data <- data.frame(age = stats::rnorm(20), stringsAsFactors = FALSE)
  config <- .fake_config_table1(
    list(age = list(nature = "continue", libelle = "Age")),
    list(variables = c("age", "absente"))
  )

  expect_error(st_table1(data, config), "introuvable")
})

test_that("st_table1 s'arrete si la configuration n'a pas ete validee", {
  data <- data.frame(age = stats::rnorm(20), stringsAsFactors = FALSE)
  config <- structure(list(dictionnaire = NULL, analyse = NULL), class = "statlab_config", valid = FALSE)

  expect_error(st_table1(data, config), "validee")
})

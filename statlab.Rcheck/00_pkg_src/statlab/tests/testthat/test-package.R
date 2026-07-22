test_that("le package se charge correctement", {
  expect_true(isNamespaceLoaded("statlab") || requireNamespace("statlab", quietly = TRUE))
})

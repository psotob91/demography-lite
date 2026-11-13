# Single source: standalone/tests/test-standalone-checks.R (copied to each
# package by tools/sync-standalone.R; do not edit the copies).

test_that("classed errors carry the package prefix and the family class", {
  err <- tryCatch(.dl_abort("boom"), error = function(e) e)
  expect_s3_class(err, "demolite_error_input")
  expect_s3_class(err, "demolite_error")
  expect_s3_class(err, paste0(.dl_prefix(), "_error_input"))
  expect_identical(conditionMessage(err), "boom")
  expect_error(.dl_not_implemented("f"), class = "demolite_error_not_implemented")
  expect_error(.dl_not_implemented("f"),
               class = paste0(.dl_prefix(), "_error_not_implemented"))
})

test_that("scalar validators accept valid input and return it", {
  expect_identical(.check_number(0.5, "x", lower = 0, upper = 1), 0.5)
  expect_null(.check_number(NULL, "x", null_ok = TRUE))
  expect_identical(.check_count(3, "n"), 3L)
  expect_identical(.check_seed(7), 7L)
  expect_identical(.check_string("a", "s"), "a")
  expect_true(.check_flag(TRUE, "f"))
  expect_identical(.check_choice(c("a", "b"), "x", c("a", "b")), "a")
  expect_identical(.check_choice(c("b", "a"), "x", c("a", "b"), several = TRUE),
                   c("b", "a"))
  expect_identical(.check_probs(c(0.1, 0.9)), c(0.1, 0.9))
})

test_that("scalar validators reject bad input with a classed error", {
  cls <- "demolite_error_input"
  expect_error(.check_number("a", "x"), class = cls)
  expect_error(.check_number(c(1, 2), "x"), class = cls)
  expect_error(.check_number(NA_real_, "x"), class = cls)
  expect_error(.check_number(2, "x", upper = 1), class = cls)
  expect_error(.check_number(NULL, "x"), class = cls)
  expect_error(.check_count(1.5, "n"), class = cls)
  expect_error(.check_count(0, "n"), class = cls)
  expect_error(.check_seed(1.5), class = cls)
  expect_error(.check_seed(NA), class = cls)
  expect_error(.check_seed(c(1, 2)), class = cls)
  expect_error(.check_seed(1e12), class = cls)
  expect_error(.check_string(1, "s"), class = cls)
  expect_error(.check_string("", "s"), class = cls)
  expect_error(.check_flag(NA, "f"), class = cls)
  expect_error(.check_choice("z", "x", c("a", "b")), class = cls)
  expect_error(.check_choice(c("a", "b"), "x", c("a", "b", "c")), class = cls)
  expect_error(.check_probs(c(0.9, 0.1)), class = cls)
  expect_error(.check_probs(c(0.1, 1.2)), class = cls)
})

test_that("vector and table validators say what is wrong", {
  cls <- "demolite_error_input"
  expect_identical(.check_numeric(c(1, 2), "x", len = 2L), c(1, 2))
  expect_error(.check_numeric(c(1, NA), "x"), class = cls)
  expect_error(.check_numeric(c(1, 2), "x", len = 3L), class = cls)
  expect_error(.check_numeric(c(1, -2), "x", lower = 0), class = cls)
  expect_error(.check_numeric(character(), "x"), class = cls)
  expect_error(.check_numeric(c(1, -2), "x", lower = 0), regexp = "position 2")
  expect_identical(.check_same_length(a = 1:3, b = 4:6, c = NULL), 3L)
  expect_error(.check_same_length(a = 1:3, b = 1:4), class = cls)
  df <- data.frame(a = 1, b = 2)
  expect_identical(.check_table(df, "df", cols = c("a", "b")), df)
  expect_error(.check_table(list(a = 1), "df"), class = cls)
  expect_error(.check_table(df, "df", cols = "z"), class = cls)
  expect_error(.check_table(df, "df", cols = "z"), regexp = "`z`")
  expect_error(.check_table(df[0, ], "df"), class = cls)
  expect_error(.check_draws(list(), "x"), class = cls)
})

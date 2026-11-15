# Single source: standalone/tests/test-standalone-draws.R (copied to each
# package by tools/sync-standalone.R; do not edit the copies).

cells <- data.frame(location_id = c(1, 2, 3), sex_id = c(1, 1, 2),
                    age_start = c(0, 7 / 365.25, 95), age_end = c(1, 1, Inf))

# Name of the package that carries this copy of the shared code.
.dl_test_package <- function() {
  environmentName(topenv(environment(draws_new)))
}

test_that("draws_new builds the container and stores identifiers as integers", {
  value <- matrix(seq_len(12) / 10, nrow = 3)
  x <- draws_new(cells, value, measure = "completeness", unit = "proportion",
                 seed = 5, stream = "s")
  expect_s3_class(x, "demolite_draws")
  expect_named(x, c("index", "value", "meta"))
  expect_true(is.integer(x$index$location_id))
  expect_true(is.integer(x$index$sex_id))
  expect_identical(dim(x$value), c(3L, 4L))
  expect_identical(x$value, value)
  expect_identical(x$meta$n_draws, 4L)
  expect_identical(x$meta$measure, "completeness")
  expect_identical(x$meta$seed, 5L)
  expect_identical(x$meta$version, "0.1.0")
  expect_output(print(x), "demolite_draws")
})

test_that("draws_new rejects malformed input with a classed error", {
  cls <- "demolite_error_input"
  value <- matrix(1, nrow = 3, ncol = 2)
  expect_error(draws_new(cells, value), class = cls)
  expect_error(draws_new(cells, value[1:2, ], measure = "m"), class = cls)
  expect_error(draws_new(cells, 1:3, measure = "m"), class = cls)
  expect_error(draws_new(cells, matrix("a", 3, 2), measure = "m"), class = cls)
  expect_error(draws_new(rbind(cells, cells[1, ]), matrix(1, 4, 2), measure = "m"),
               class = cls)
  bad <- cells
  bad$location_id <- c(1.5, 2, 3)
  expect_error(draws_new(bad, value, measure = "m"), class = cls)
  bad <- cells
  bad$age_end <- c(0, 1, Inf)
  expect_error(draws_new(bad, value, measure = "m"), class = cls)
  bad <- cells
  bad$draw <- 1
  expect_error(draws_new(bad, value, measure = "m"), class = cls)
  bad <- cells
  bad$sex_id <- c(1, NA, 2)
  expect_error(draws_new(bad, value, measure = "m"), class = cls)
  expect_error(draws_new(cells, value, measure = "m", seed = 1.5), class = cls)
})

test_that("long and wide conversions round-trip and keep draw k in place", {
  value <- matrix(c(1, 2, 3, 10, 20, 30, 100, 200, NA), nrow = 3)
  x <- draws_new(cells, value, measure = "deaths", unit = "count")
  long <- as_draws_long(x)
  expect_identical(nrow(long), 9L)
  expect_identical(long$draw, rep(1:3, each = 3))
  expect_identical(long$value, as.vector(value))
  wide <- as_draws_wide(x)
  expect_identical(names(wide)[5:7], c("draw_0", "draw_1", "draw_2"))
  expect_identical(wide$draw_1, c(10, 20, 30))
  back_long <- draws_new(long, measure = "deaths", unit = "count")
  back_wide <- draws_new(wide, measure = "deaths", unit = "count")
  expect_identical(back_long$value, x$value)
  expect_identical(back_wide$value, x$value)
  expect_equal(back_long$index, x$index)
  expect_equal(back_wide$index, x$index)
  # a shuffled long table gives the same container
  shuffled <- long[c(9, 2, 7, 4, 1, 6, 3, 8, 5), ]
  again <- draws_new(shuffled, measure = "deaths")
  ord <- match(x$index$location_id, again$index$location_id)
  expect_identical(again$value[ord, ], x$value)
})

test_that("incomplete or ambiguous tables of draws are rejected", {
  cls <- "demolite_error_input"
  x <- draws_new(cells, matrix(1:6 / 1, nrow = 3), measure = "m")
  long <- as_draws_long(x)
  expect_error(draws_new(long[-1, ], measure = "m"), class = cls)
  expect_error(draws_new(rbind(long, long[1, ]), measure = "m"), class = cls)
  wide <- as_draws_wide(x)
  names(wide)[names(wide) == "draw_1"] <- "draw_2"
  expect_error(draws_new(wide, measure = "m"), class = cls)
  expect_error(draws_new(cells, measure = "m"), class = cls)
})

test_that("draws_summarise returns the mean and the 2.5 and 97.5 percentiles", {
  v1 <- seq(0, 1, length.out = 41)
  v2 <- c(5, 1, 3, 2, 4, rep(3, 36))
  x <- draws_new(data.frame(location_id = 1:2), rbind(v1, v2), measure = "m")
  s <- draws_summarise(x)
  expect_named(s, c("location_id", "mean", "lower", "upper"))
  expect_equal(s$mean, c(0.5, mean(v2)))
  expect_equal(s$lower, c(0.025, unname(quantile(v2, 0.025))))
  expect_equal(s$upper, c(0.975, unname(quantile(v2, 0.975))))
  s80 <- draws_summarise(x, probs = c(0.1, 0.9))
  expect_equal(s80$lower[1], 0.1)
  expect_equal(s80$upper[1], 0.9)
  expect_error(draws_summarise(x, probs = c(0.9, 0.1)), class = "demolite_error_input")
  expect_error(draws_summarise(list()), class = "demolite_error_input")
})

test_that("draws_summarise handles missing draws explicitly", {
  x <- draws_new(data.frame(location_id = 1:3),
                 rbind(c(1, 2, 3), c(1, NA, 3), c(NA, NA, NA)), measure = "m")
  s <- draws_summarise(x)
  expect_equal(s$mean, c(2, NA, NA))
  expect_true(all(is.na(s$lower[2:3])))
  s2 <- draws_summarise(x, na_rm = TRUE)
  expect_equal(s2$mean, c(2, 2, NA))
  expect_equal(s2$upper[2], unname(quantile(c(1, 3), 0.975)))
  expect_true(is.na(s2$lower[3]))
})

test_that("the hash matches published and independently computed values", {
  # FNV-1a 32-bit test vectors: "" -> 0x811c9dc5, "a" -> 0xe40c292c,
  # "foobar" -> 0xbf9cf968
  expect_identical(.dl_fnv1a32(c("", "a", "foobar")),
                   c(2166136261, 3826002220, 3214735720))
  # MurmurHash3 fmix32, checked against an independent implementation
  expect_identical(.dl_fmix32(c(0, 1, 2, 4294967295)),
                   c(0, 1364076727, 821347078, 2180083513))
  expect_identical(.dl_mul32(4294967295, 4294967295), 1)
  expect_identical(.dl_xor32(4294967295, 1), 4294967294)
})

test_that("the state of a cell is frozen: changing it would change every draw", {
  # Written in another R session and pasted here: the same label must give the
  # same six words in any session, on any platform.
  expect_identical(
    cell_seed(2024, "census_error", cells[, 1:3]),
    matrix(c(1857579057L, 781404387L, 327483606L, 1904353086L, -346500527L,
             -1796578591L, -157479439L, 2077536727L, -32531684L, -163837812L,
             592522190L, -26886647L, -1451471773L, 296144898L, -737406288L,
             2017449601L, 1302336732L, 1670292682L),
           nrow = 3L, ncol = 6L, byrow = TRUE)
  )
  expect_identical(cell_seed(2024, "common"),
                   matrix(c(-1607432789L, 849290226L, -1288755409L, 943386678L,
                            -483569264L, 393260045L), nrow = 1L))
  # the same state gives the same numbers, another state gives others
  st <- cell_seed(2024, "census_error", cells[, 1:3])
  expect_identical(
    with_cell_seed(st[1, ], stats::rnorm(3)),
    c(-1.9837435362046558, 0.15626956364709027, -0.090313955165843771)
  )
  expect_false(identical(with_cell_seed(st[2, ], stats::rnorm(3)),
                         with_cell_seed(st[1, ], stats::rnorm(3))))
})

test_that("the state of a cell is one the generator accepts", {
  # R silently re-randomises a state it considers invalid, which would throw
  # reproducibility away without saying so: the first three words must stay
  # below m1, the last three below m2, neither trio all zero, and no word may
  # be 2^31, which an R integer cannot hold.
  grid <- expand.grid(location_id = 1:25, year_id = 2001:2010, sex_id = 1:2,
                      age_start = 0:9)
  st <- cell_seed(1, "grid", grid)
  expect_identical(dim(st), c(5000L, 6L))
  expect_type(st, "integer")
  expect_false(anyNA(st))
  u <- ifelse(st < 0, st + 4294967296, st)
  expect_true(all(u[, 1:3] < 4294967087))
  expect_true(all(u[, 4:6] < 4294944443))
  expect_true(all(rowSums(u[, 1:3]) > 0))
  expect_true(all(rowSums(u[, 4:6]) > 0))
  # and the whole state is unique cell by cell on this grid
  expect_identical(anyDuplicated(do.call(paste, c(as.data.frame(st), sep = "|"))), 0L)
  # the words of a cell are not copies of one another
  expect_identical(anyDuplicated(as.vector(st[1, ])), 0L)
  # a state with a trio of zeros, or a word out of range, is refused
  expect_error(with_cell_seed(c(0L, 0L, 0L, 1L, 2L, 3L), 1),
               class = "demolite_error_input")
  expect_error(with_cell_seed(c(1L, 2L, 3L, 0L, 0L, 0L), 1),
               class = "demolite_error_input")
  expect_error(with_cell_seed(c(1L, 2L, 3L, 4L, 5L), 1),
               class = "demolite_error_input")
  expect_error(with_cell_seed(2147483647L, 1), class = "demolite_error_input")
})

test_that("the state of a cell depends only on seed, stream and key", {
  s <- cell_seed(2024, "census_error", cells)
  expect_identical(dim(s), c(3L, 6L))
  # order of the cells
  expect_identical(cell_seed(2024, "census_error", cells[3:1, ]), s[3:1, ])
  # order of the columns
  expect_identical(cell_seed(2024, "census_error", cells[, 4:1]), s)
  # integer or double identifiers, data frame or list
  int <- cells
  int$location_id <- as.integer(int$location_id)
  expect_identical(cell_seed(2024, "census_error", int), s)
  expect_identical(cell_seed(2024, "census_error", as.list(cells)), s)
  # a subset of cells (as when the work is split) gets the same state
  expect_identical(cell_seed(2024, "census_error", cells[2, ]), s[2, , drop = FALSE])
  # another stream, seed or key gives another state
  expect_false(any(cell_seed(2024, "deaths_error", cells) == s))
  expect_false(any(cell_seed(2025, "census_error", cells) == s))
  expect_false(any(cell_seed(2024, "census_error", cells[, 1:2]) == s))
  expect_error(cell_seed(2024, "s", list(1:3)), class = "demolite_error_input")
  expect_error(cell_seed(2024, "s", list(a = 1:3, b = 1:2)),
               class = "demolite_error_input")
  expect_error(cell_seed(2024, "s", list(a = list(1))), class = "demolite_error_input")
  expect_error(cell_seed("a", "s"), class = "demolite_error_input")
})

test_that("with_cell_seed is reproducible and restores the generator", {
  state <- cell_seed(2024, "example", data.frame(location_id = 1))[1, ]
  other <- cell_seed(2024, "example", data.frame(location_id = 2))[1, ]
  set.seed(99)
  before <- get(".Random.seed", envir = globalenv())
  a <- with_cell_seed(state, stats::rnorm(4))
  b <- with_cell_seed(state, stats::rnorm(4))
  expect_identical(a, b)
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_false(identical(a, with_cell_seed(other, stats::rnorm(4))))
  # a matrix of one row is the same thing as its vector
  expect_identical(with_cell_seed(matrix(state, nrow = 1L), stats::rnorm(4)), a)
  # the state is restored even if the code fails
  expect_error(with_cell_seed(state, stop("x")))
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  # the kind of generator of the caller is restored too, L'Ecuyer-CMRG included
  old <- RNGkind("L'Ecuyer-CMRG")
  set.seed(5)
  kept <- get(".Random.seed", envir = globalenv())
  c1 <- with_cell_seed(state, stats::rnorm(4))
  expect_identical(c1, a)
  expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")
  expect_identical(get(".Random.seed", envir = globalenv()), kept)
  RNGkind(old[1], old[2], old[3])
  # no generator state before the call: none is left behind
  rm(".Random.seed", envir = globalenv())
  d <- with_cell_seed(state, stats::rnorm(4))
  expect_identical(d, a)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  set.seed(NULL)
})

test_that("draw k of a cell does not depend on the order or split of the cells", {
  idx <- data.frame(location_id = 1:6, sex_id = rep(1:2, 3))
  gen <- function(n, cell) stats::rnorm(n, mean = cell$location_id)
  a <- draws_generate(idx, 20, seed = 1, stream = "x", gen, measure = "m")
  b <- draws_generate(idx[6:1, ], 20, seed = 1, stream = "x", gen, measure = "m")
  la <- as_draws_long(a)
  lb <- as_draws_long(b)
  expect_equal(la[order(la$location_id, la$sex_id, la$draw), ],
               lb[order(lb$location_id, lb$sex_id, lb$draw), ],
               ignore_attr = TRUE)
  # split in two chunks, as two processes would do
  p1 <- draws_generate(idx[1:2, ], 20, seed = 1, stream = "x", gen, measure = "m")
  p2 <- draws_generate(idx[3:6, ], 20, seed = 1, stream = "x", gen, measure = "m")
  expect_identical(rbind(p1$value, p2$value), a$value)
  # other seed or stream: other numbers
  expect_false(identical(
    draws_generate(idx, 20, seed = 2, stream = "x", gen, measure = "m")$value, a$value
  ))
  expect_false(identical(
    draws_generate(idx, 20, seed = 1, stream = "y", gen, measure = "m")$value, a$value
  ))
  expect_identical(a$meta$seed, 1L)
  expect_identical(a$meta$stream, "x")
})

test_that("draws_generate leaves the caller's random stream untouched", {
  idx <- data.frame(location_id = 1:3)
  gen <- function(n, cell) stats::runif(n)
  set.seed(42)
  u1 <- stats::runif(1)
  set.seed(42)
  x <- draws_generate(idx, 5, seed = 1, stream = "x", gen, measure = "m")
  u2 <- stats::runif(1)
  expect_identical(u1, u2)
  expect_error(draws_generate(idx, 5, seed = 1, stream = "x", function(n, cell) 1,
                              measure = "m"), class = "demolite_error_input")
  expect_error(draws_generate(idx, 5, seed = 1, stream = "x", gen, measure = "m",
                              key_cols = "zz"), class = "demolite_error_input")
  expect_error(draws_generate(idx, 5, stream = "x", generator = gen, measure = "m"),
               class = "demolite_error_input")
})

test_that("the print methods are registered once and dispatch from outside", {
  # Look in the registry of the generic and dispatch from the global
  # environment: from inside the package its own copy is always visible and
  # proves nothing.
  registry <- get(".__S3MethodsTable__.", envir = baseenv())
  for (cls in c("demolite_draws", "demolite_country")) {
    name <- paste0("print.", cls)
    expect_true(exists(name, envir = registry, inherits = FALSE))
    registered <- get(name, envir = registry)
    own <- get(name, envir = asNamespace(.dl_test_package()), inherits = FALSE)
    # The same object, not merely the same code: that is what keeps a second
    # package of the family from overwriting the method of the first one, and
    # what makes the order of loading irrelevant.
    expect_identical(registered, own)
    expect_identical(environment(own), baseenv())
    # No loop in the method: R caches the compiled form of a loop in the
    # syntax tree once the function has run, and the copies would stop being
    # identical as soon as one of them had printed something.
    expect_false(any(grepl("\\b(for|while|repeat)\\b", deparse(body(own)))))
  }
  x <- draws_new(data.frame(location_id = 1L), matrix(1, 1, 2), measure = "m")
  out <- utils::capture.output(eval(call("print", x), envir = globalenv()))
  expect_match(out[1], "<demolite_draws> m", fixed = TRUE)
  expect_match(out[2], "cells: 1", fixed = TRUE)
  expect_length(out, 3L)
})

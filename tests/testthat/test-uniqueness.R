test_that("is candidate key - data.table", {

  df <- data.table::data.table(a = c(1, 2, 3), b = c(4, 4, 5), c = c(6, 7, 7))

  df[, as.integer("2"), with = FALSE]
  expect_true(is_candidate_key(df, cols = "a"))

  candidate_key <- c("a")
  expect_true(is_candidate_key(df, cols = candidate_key))

  #======================================================================

  expect_false(is_candidate_key(df, cols = "b"))

  candidate_key <- c("b")
  expect_false(is_candidate_key(df, cols = candidate_key))

  #======================================================================

  expect_true(is_candidate_key(df, cols = c("b", "c")))

  candidate_key <- c("b", "c")
  expect_true(is_candidate_key(df, cols = candidate_key))

})

test_that("is candidate key - data.frame", {

  df <- data.frame(a = c(1, 2, 3), b = c(4, 4, 5), c = c(6, 7, 7))


  expect_true(is_candidate_key(df, cols = "a"))

  candidate_key <- c("a")
  expect_true(is_candidate_key(df, cols = candidate_key))

  #======================================================================

  expect_false(is_candidate_key(df, cols = "b"))

  candidate_key <- c("b")
  expect_false(is_candidate_key(df, cols = candidate_key))

  #======================================================================

  expect_true(is_candidate_key(df, cols = c("b", "c")))

  candidate_key <- c("b", "c")
  expect_true(is_candidate_key(df, cols = candidate_key))

})

test_that("is candidate key - tibble", {

  df <- tibble::tibble(a = c(1, 2, 3), b = c(4, 4, 5), c = c(6, 7, 7))


  expect_true(is_candidate_key(df, cols = "a"))

  candidate_key <- c("a")
  expect_true(is_candidate_key(df, cols = candidate_key))

  #======================================================================

  expect_false(is_candidate_key(df, cols = "b"))

  candidate_key <- c("b")
  expect_false(is_candidate_key(df, cols = candidate_key))

  #======================================================================

  expect_true(is_candidate_key(df, cols = c("b", "c")))

  candidate_key <- c("b", "c")
  expect_true(is_candidate_key(df, cols = candidate_key))

})

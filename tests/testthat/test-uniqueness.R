test_that("is candidate key - data.table", {

  df <- data.table::data.table(a = c(1, 2, 3), b = c(4, 4, 5), c = c(6, 7, 7))

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


test_that("filter duplicated rows - data.table", {

  df <- data.table::data.table(a = c(1, 2, 3), b = c(4, 4, 5), c = c(6, 7, 7))


  expect_identical(filter_duplicated_rows(df, "a"),
                   data.table::data.table(a=numeric(), b=numeric(), c=numeric()))

  candidate_key <- c("a")
  expect_identical(filter_duplicated_rows(df, candidate_key),
                   data.table::data.table(a=numeric(), b=numeric(), c=numeric()))

  #======================================================================

  expect_identical(filter_duplicated_rows(df, "b"),
                   data.table::data.table(a=c(1, 2), b=c(4, 4), c=c(6, 7)))

  candidate_key <- c("b")
  expect_identical(filter_duplicated_rows(df, candidate_key),
                   data.table::data.table(a=c(1, 2), b=c(4, 4), c=c(6, 7)))

})

test_that("filter duplicated rows - data.frame", {

  df <- data.frame(a = c(1, 2, 3), b = c(4, 4, 5), c = c(6, 7, 7))


  expect_identical(filter_duplicated_rows(df, "a"),
                   data.frame(a=numeric(), b=numeric(), c=numeric()))

  candidate_key <- c("a")
  expect_identical(filter_duplicated_rows(df, candidate_key),
                   data.frame(a=numeric(), b=numeric(), c=numeric()))

  #======================================================================

  expect_identical(filter_duplicated_rows(df, "b"),
                   data.frame(a=c(1, 2), b=c(4, 4), c=c(6, 7)))

  candidate_key <- c("b")
  expect_identical(filter_duplicated_rows(df, candidate_key),
                   data.frame(a=c(1, 2), b=c(4, 4), c=c(6, 7)))

})

test_that("filter duplicated rows - tibble", {

  df <- tibble::tibble(a = c(1, 2, 3), b = c(4, 4, 5), c = c(6, 7, 7))


  expect_identical(filter_duplicated_rows(df, "a"),
                   tibble::tibble(a=numeric(), b=numeric(), c=numeric()))

  candidate_key <- c("a")
  expect_identical(filter_duplicated_rows(df, candidate_key),
                   tibble::tibble(a=numeric(), b=numeric(), c=numeric()))

  #======================================================================

  expect_identical(filter_duplicated_rows(df, "b"),
                   tibble::tibble(a=c(1, 2), b=c(4, 4), c=c(6, 7)))

  candidate_key <- c("b")
  expect_identical(filter_duplicated_rows(df, candidate_key),
                   tibble::tibble(a=c(1, 2), b=c(4, 4), c=c(6, 7)))

})

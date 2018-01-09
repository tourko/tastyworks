context("Multiline pattern - detect()")

test_that("0 lines and empty pattern", {
  lines <- c()

  patterns <- c()

  result   <- multiline$detect(lines, patterns)
  expected <- matrix(integer(length(patterns)), dimnames = list(names(patterns)))[, -1, drop = FALSE]

  expect_identical(result, expected)
})

test_that("0 lines and 1-line pattern", {
  lines <- c()

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% "#1" %R% END
  )

  result   <- multiline$detect(lines, patterns)
  expected <- matrix(integer(length(patterns)), dimnames = list(names(patterns)))[, -1, drop = FALSE]

  expect_identical(result, expected)
})

test_that("1 line and an empty pattern", {
  lines <- c("Test line #1")

  patterns <- c()

  result   <- multiline$detect(lines, patterns)
  expected <- matrix(integer(length(patterns)), dimnames = list(names(patterns)))[, -1, drop = FALSE]

  expect_identical(result, expected)
})

test_that("1 line and a matching 1-line pattern", {
  lines <- c("Test line #1")

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% "#1" %R% END
  )

  result   <- multiline$detect(lines, patterns)
  expected <- matrix(1L, nrow = length(patterns), dimnames = list(names(patterns)))

  expect_identical(result, expected)
})

test_that("1 line and a non-matching 1-line pattern", {
  lines <- c("Test line #1")

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% "#2" %R% END
  )

  result <- multiline$detect(lines, patterns)
  expected <- matrix(integer(length(patterns)),  dimnames = list(names(patterns)))[, -1, drop = FALSE]

  expect_identical(result, expected)
})

test_that("2 lines and a matching 2-lines pattern", {
  lines <- c(
    "Test line #1",
    "Test line #2"
  )

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% "#1" %R% END,
    pattern_2 = START %R% one_or_more(WRD %R% SPC) %R% "#2" %R% END
  )

  result   <- multiline$detect(lines, patterns)
  expected <- matrix(c(1L, 2L), nrow = length(patterns), dimnames = list(names(patterns)))

  expect_identical(result, expected)
})

test_that("2 lines and a matching 1-line pattern", {
  lines <- c(
    "Test line #1",
    "Test line #2"
  )

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% "#" %R% DGT %R% END
  )

  result   <- multiline$detect(lines, patterns)
  expected <- matrix(c(1L, 2L), nrow = length(patterns), dimnames = list(names(patterns)))

  expect_identical(result, expected)
})

test_that("2 lines and a non-matching 1-line pattern", {
  lines <- c(
    "Test line #1",
    "Test line #2"
  )

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% "#" %R% END
  )

  result   <- multiline$detect(lines, patterns)
  expected <- matrix(integer(length(patterns)), dimnames = list(names(patterns)))[, -1, drop = FALSE]

  expect_identical(result, expected)
})

test_that("5 lines and a matching 2-lines pattern", {
  lines <- c(
    "Test line #1",
    "Test line #2",
    "Test line #3",
    "Test line #1",
    "Test line #2"
  )

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% "#1" %R% END,
    pattern_2 = START %R% one_or_more(WRD %R% SPC) %R% "#2" %R% END
  )

  result   <- multiline$detect(lines, patterns)
  expected <- matrix(c(1L, 2L, 4L, 5L), nrow = length(patterns), dimnames = list(names(patterns)))

  expect_identical(result, expected)
})

test_that("2 out of 4 lines matching 2-lines pattern", {
  lines <- c(
    "Test line #1",
    "Test line #2",
    "Test line #2",
    "Test line #1"
  )

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% "#1" %R% END,
    pattern_2 = START %R% one_or_more(WRD %R% SPC) %R% "#2" %R% END
  )

  result   <- multiline$detect(lines, patterns)
  expected <- matrix(c(1L, 2L), nrow = length(patterns), dimnames = list(names(patterns)))

  expect_identical(result, expected)
})

test_that("2 out of 4 lines with overlapping matching 2-lines pattern", {
  lines <- c(
    "Test line #1",
    "Test line #1",
    "Test line #2",
    "Test line #1"
  )

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% "#1" %R% END,
    pattern_2 = START %R% one_or_more(WRD %R% SPC) %R% "#2" %R% END
  )

  result   <- multiline$detect(lines, patterns)
  expected <- matrix(c(2L, 3L), nrow = length(patterns), dimnames = list(names(patterns)))

  expect_identical(result, expected)
})

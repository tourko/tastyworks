context("Multiline pattern - match()")

test_that("5 lines and a matching 1-line pattern", {
  lines <- c(
    "test line #1",
    "test line #2",
    "Test Line  3",
    "TEST LINE #1",
    "TEST LINE #2"
  )

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% "#" %R% capture(DGT) %R% END
  )

  result   <- multiline$match(lines, patterns)
  expected <- tibble(v1 = c("1", "2", "1", "2"),
                     .lines = as.list(list(c(1L), c(2L), c(4L), c(5L))))

  expect_identical(result, expected)
})

test_that("5 lines and a matching 2-lines pattern", {
  lines <- c(
    "test line #1",
    "test line #2",
    "Test Line  3",
    "TEST LINE #1",
    "TEST LINE #2"
  )

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% capture("#1") %R% END,
    pattern_2 = START %R% one_or_more(WRD %R% SPC) %R% capture("#2") %R% END
  )

  result   <- multiline$match(lines, patterns, names = c("one", "two"))
  expected <- tibble(one = c("#1", "#1"), two = c("#2", "#2"),
                     .lines = as.list(list(c(1L, 2L), c(4L, 5L))))

  expect_identical(result, expected)
})

test_that("5 lines and a non-matching 1-line pattern", {
  lines <- c(
    "test line #1",
    "test line #2",
    "Test Line  3",
    "TEST LINE #1",
    "TEST LINE #2"
  )

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% "#" %R% capture(ALPHA) %R% END
  )

  result   <- multiline$match(lines, patterns)
  expected <- tibble()

  expect_identical(result, expected)
})

test_that("2 lines and a matching 2-line pattern", {
  lines <- c(
    "test line #1",
    "test line #2"
  )

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% capture("#1") %R% END,
    pattern_2 = START %R% one_or_more(WRD %R% SPC) %R% capture("#2") %R% END
  )

  result   <- multiline$match(lines, patterns)
  expected <- tibble(v1 = c("#1"), v2 = c("#2"),
                     .lines = as.list(list(c(1L, 2L))))

  expect_identical(result, expected)
})

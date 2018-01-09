context("Multiline pattern - subset()")

test_that("2 lines and a non-matching 1-line pattern", {
  lines <- c(
    "Test line #1",
    "Test line #2"
  )

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% "#" %R% END
  )

  result   <- multiline$subset(lines, patterns)
  expected <- matrix(character(length(patterns)), dimnames = list(names(patterns)))[, -1, drop = FALSE]

  expect_identical(result, expected)
})

test_that("5 lines and a matching 2-lines pattern", {
  lines <- c(
    "test line #1",
    "test line #2",
    "Test Line #3",
    "TEST LINE #1",
    "TEST LINE #2"
  )

  patterns <- c(
    pattern_1 = START %R% one_or_more(WRD %R% SPC) %R% "#1" %R% END,
    pattern_2 = START %R% one_or_more(WRD %R% SPC) %R% "#2" %R% END
  )

  result   <- multiline$subset(lines, patterns)
  expected <- matrix(c("test line #1", "test line #2", "TEST LINE #1", "TEST LINE #2"),
                     nrow = length(patterns), dimnames = list(names(patterns)))

  expect_identical(result, expected)
})

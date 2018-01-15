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
  expected <- matrix(character(2), dimnames = list(c(names(patterns), ".lines")))[, -1, drop = FALSE]

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
  #           [,1]           [,2]
  # pattern_1 "test line #1" "TEST LINE #1"
  # pattern_2 "test line #2" "TEST LINE #2"
  # .lines    "1,2"          "4,5"
  expected <- matrix(c("test line #1", "test line #2", "1,2",
                       "TEST LINE #1", "TEST LINE #2", "4,5"),
                     nrow = length(patterns) + 1, dimnames = list(c(names(patterns), ".lines")))

  expect_identical(result, expected)
})

test_that("No lines and no patterns", {
  lines <- c()
  patterns <- c()

  result   <- multiline$subset(lines, patterns)
  expected <- matrix(NA, nrow = 0, ncol = 0)

  expect_identical(result, expected)
})

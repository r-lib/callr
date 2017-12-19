
context("assertions")

strings <- list("foo", "", "111", "1", "-", "NA")
not_strings <- list(1, character(), NA_character_, NA,
                    c("foo", NA), c("1", "2"), NULL)

test_that("is_string", {
  for (p in strings) {
    expect_true(is_string(p))
    expect_silent(assert_that(is_string(p)))
  }

  for (n in not_strings) {
    expect_false(is_string(n))
    expect_error(assert_that(is_string(n)), "is not a string")
  }
})

test_that("is_string_or_null", {
  for (p in strings) {
    expect_true(is_string_or_null(p))
    expect_silent(assert_that(is_string_or_null(p)))
  }
  expect_true(is_string_or_null(NULL))
  expect_silent(assert_that(is_string_or_null(NULL)))

  for (n in not_strings) {
    if (!is.null(n)) {
      expect_false(is_string_or_null(n))
      expect_error(
        assert_that(is_string_or_null(n)),
        "must be a string .* NULL"
      )
    }
  }
})

flags <- list(TRUE, FALSE)
not_flags <- list(1, character(), NA_character_, NA,
                  c("foo", NA), c("1", "2"), NULL)

test_that("is_flag", {
  for (p in flags) {
    expect_true(is_flag(p))
    expect_silent(assert_that(is_flag(p)))
  }

  for (n in not_flags) {
    expect_false(is_flag(n))
    expect_error(assert_that(is_flag(n)), "is not a flag")
  }
})

ints <- list(1, 0, -1, 1L, 0L, -1L, 1.0, 42.0)
not_ints <- list(1.2, 0.1, "foo", numeric(), integer(), NULL,
                 NA_integer_, NA_real_)

test_that("is_integerish_scalar", {
  for (p in ints) {
    expect_true(is_integerish_scalar(p))
    expect_silent(assert_that(is_integerish_scalar(p)))
  }

  for (n in not_ints) {
    expect_false(is_integerish_scalar(n))
    expect_error(
      assert_that(is_integerish_scalar(n)),
      "is not a length 1 integer"
    )
  }
})

test_that("is_pid", {
  for (p in ints) {
    expect_true(is_pid(p))
    expect_silent(assert_that(is_pid(p)))
  }

  for (n in not_ints) {
    expect_false(is_pid(n))
    expect_error(assert_that(is_pid(n)), "is not a process id")
  }
})

test_that("is_flag_or_string", {
  for (p in c(flags, strings)) {
    expect_true(is_flag_or_string(p))
    expect_silent(assert_that(is_flag_or_string(p)))
  }

  for (n in intersect(not_flags, not_strings)) {
    expect_false(is_flag_or_string(n))
    expect_error(
      assert_that(is_flag_or_string(n)),
      "is not a flag or a string"
    )
  }
  
})

test_that("is_existing_file", {
  expect_false(is_existing_file(tempfile()))
  expect_error(
    assert_that(is_existing_file(tempfile())),
    "File .* does not exist"
  )

  cat("foo\n", file = tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)
  expect_true(is_existing_file(tmp))
  expect_silent(assert_that(is_existing_file(tmp)))
})

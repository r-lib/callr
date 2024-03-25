
test_that("load_client_lib", {
  lib <- load_client_lib()

  funcs <- c("base64_decode", "set_stderr", "write_fd")
  expect_true(
    all(funcs %in% names(lib))
  )

  lib <- load_client_lib(pxdir = find.package("processx"))
  expect_true(
    all(funcs %in% names(lib))
  )
})

test_that("errors", {
  skip_if_not_installed("mockery")
  mockery::stub(load_client_lib, "system.file", "")
  expect_error(
    load_client_lib(),
    "Cannot find client file"
  )
})

test_that("errors 2", {
  skip_if_not_installed("mockery")
  sofile <- system.file(
    "libs", .Platform$r_arch, paste0("client", .Platform$dynlib.ext),
    package = "processx"
  )
  mockery::stub(load_client_lib, "dyn.load", function(...) stop("ooops"))
  expect_error(load_client_lib(sofile))
})

test_that("base64", {
  lib <- load_client_lib()
  enc <- lib$base64_encode(charToRaw("hello world!"))
  expect_equal(lib$base64_decode(enc), charToRaw("hello world!"))
})

test_that("write_fd", {
  do <- function() {
    lib <- asNamespace("callr")$load_client_lib()
    f1 <- tempfile()
    c1 <- processx::conn_create_file(f1, write = TRUE)
    lib$write_fd(processx::conn_get_fileno(c1), "hello world!\n")
    close(c1)
    readLines(f1)
  }

  ret <- callr::r(do)
  expect_equal(ret, "hello world!")
})

test_that("set_stdout, set_stderr", {
  # TODO why does this not work on windows, if `set_std{out,err}_file` work?
  # Also, `set_std{out,err}` work in `r_session`.
  # But this test crashes at the `set_stdout()` call.
  skip_on_os("windows")

  do <- function() {
    lib <- asNamespace("callr")$load_client_lib()
    f1 <- tempfile()
    f2 <- tempfile()
    c1 <- processx::conn_create_file(f1, write = TRUE)
    c2 <- processx::conn_create_file(f2, write = TRUE)
    lib$set_stdout(processx::conn_get_fileno(c1))
    lib$set_stderr(processx::conn_get_fileno(c2))
    cat("this is output")
    message("this is error")
    close(c1)
    close(c2)
    c(readLines(f1), readLines(f2))
  }

  ret <- callr::r(do)
  expect_equal(ret, c("this is output", "this is error"))
})

test_that("set_stdout_file, set_setderr_file", {
  do <- function() {
    lib <- asNamespace("callr")$load_client_lib()
    f1 <- tempfile()
    f2 <- tempfile()
    lib$set_stdout_file(f1)
    lib$set_stderr_file(f2)
    cat("this is output")
    message("this is error")
    c(readLines(f1), readLines(f2))
  }

  ret <- callr::r(do)
  expect_equal(ret, c("this is output", "this is error"))
})

test_that("init function of client lib is run", {
  pxlib <- r(function() {
    as.environment("tools:callr")$`__callr_data__`$pxlib
  })

  # File name should be `client.SOEXT` so that R can match the init
  # function from the name
  expect_true(grepl("^client\\.", basename(pxlib$.path)))

  # In case R removes the `dynamicLookup` field
  skip_on_cran()

  # Should be `FALSE` since processx disables dynamic lookup in the
  # init function
  expect_false(unclass(pxlib$.lib)$dynamicLookup)
})

test_that("CALLR_NO_TEMP_DLLS", {
  skip_on_cran()
  if (.Platform$OS.type != "windows") skip("Windows only")

  # If not set, then it should come from the temporary location
  withr::local_envvar(CALLR_NO_TEMP_DLLS = NA_character_)
  dlls <- callr::r(function() ps::ps_shared_libs())$path
  px <- grep("processx.*client.dll", dlls)
  cr <- grep("callr.*client.dll", dlls)
  expect_true(length(px) == 0)
  expect_true(length(cr) >= 1)

  # If set, then it should come from processx
  withr::local_envvar(CALLR_NO_TEMP_DLLS = "true")
  dlls <- callr::r(function() ps::ps_shared_libs())$path
  px <- grep("processx.*client.dll", dlls)
  cr <- grep("callr.*client.dll", dlls)
  expect_true(length(px) >= 1)
  expect_true(length(cr) == 0)
})

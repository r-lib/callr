test_that("callr::r() produces a span when tracing is enabled", {
  out <- otelsdk::with_otel_record({
    r(function() 1 + 1)
  })
  expect_equal(out$value, 2)
  spans <- out$traces
  expect_length(spans, 1L)
  expect_equal(spans[[1]]$name, "callr::r")
  expect_equal(spans[[1]]$instrumentation_scope$name, "org.r-lib.callr")
  expect_equal(spans[[1]]$status, "ok")
})

test_that("callr::rscript() produces a span", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("invisible(NULL)", tmp)
  out <- otelsdk::with_otel_record(rscript(tmp, fail_on_status = FALSE))
  names <- vapply(out$traces, "[[", character(1), "name")
  expect_true("callr::rscript" %in% names)
})

test_that("callr::rcmd() produces a span", {
  out <- otelsdk::with_otel_record(rcmd("Rscript", c("-e", "1+1")))
  names <- vapply(out$traces, "[[", character(1), "name")
  expect_true("callr::rcmd" %in% names)
})

test_that("TRACEPARENT is propagated to subprocess", {
  out <- otelsdk::with_otel_record({
    r(function() Sys.getenv("TRACEPARENT"))
  })
  expect_match(
    out$value,
    "^00-[0-9a-f]{32}-[0-9a-f]{16}-[0-9a-f]{2}$"
  )
  span <- out$traces[[1]]
  expect_true(grepl(span$trace_id, out$value, fixed = TRUE))
  expect_true(grepl(span$span_id, out$value, fixed = TRUE))
})

test_that("subprocess errors generate an exception event", {
  out <- otelsdk::with_otel_record({
    tryCatch(r(function() stop("boom")), error = function(e) NULL)
  })
  expect_length(out$traces, 1L)
  events <- vapply(out$traces[[1]]$events, "[[", character(1), "name")
  expect_true("exception" %in% events)
})

test_that("r_session produces spans for the session lifecycle", {
  out <- otelsdk::with_otel_record({
    rs <- r_session$new()
    v <- rs$run(function() 1 + 1)
    rs$close()
    v
  })
  expect_equal(out$value, 2)

  names <- vapply(out$traces, "[[", character(1), "name")
  expect_true("callr::r_session" %in% names)
  expect_true("r_session$initialize() wait" %in% names)
  expect_true("r_session$call" %in% names)
  expect_true("r_session$close" %in% names)
  expect_true("r_session$read" %in% names)

  session_span <- out$traces[[which(names == "callr::r_session")]]
  expect_equal(session_span$parent, "0000000000000000")

  children <- out$traces[names != "callr::r_session"]
  for (ch in children) {
    expect_equal(ch$parent, session_span$span_id)
  }
})

test_that("r_session$read sets message and status_code attributes", {
  out <- otelsdk::with_otel_record({
    rs <- r_session$new()
    rs$run(function() 1 + 1)
    rs$close()
  })
  names <- vapply(out$traces, "[[", character(1), "name")
  read_spans <- out$traces[names == "r_session$read"]
  expect_gt(length(read_spans), 0L)
  with_message <- vapply(
    read_spans,
    function(s) isTRUE(s$attributes$message),
    logical(1)
  )
  expect_true(any(with_message))
  msg_span <- read_spans[with_message][[1]]
  expect_true("status_code" %in% names(msg_span$attributes))
})

test_that("r_process produces a span and records the get_result event", {
  out <- otelsdk::with_otel_record({
    rp <- r_bg(function() Sys.sleep(0.05))
    # Poll while alive to trigger the "Still alive" branch with the event
    while (rp$is_alive()) {
      tryCatch(rp$get_result(), error = function(e) NULL)
      Sys.sleep(0.01)
    }
    rp$get_result()
  })
  names <- vapply(out$traces, "[[", character(1), "name")
  expect_true("callr::r_process" %in% names)
  span <- out$traces[[which(names == "callr::r_process")[1]]]
  events <- vapply(span$events, "[[", character(1), "name")
  expect_true("get_result" %in% events)
})

test_that("tracing is disabled when no SDK is configured", {
  expect_false(otel::is_tracing_enabled("org.r-lib.callr"))
})

# OpenTelemetry

## Introduction

callr is instrumented with [OpenTelemetry](https://opentelemetry.io/),
the vendor-neutral observability standard for distributed traces,
metrics and logs. When tracing is enabled, callr emits spans for every R
subprocess it starts and propagates trace context into those
subprocesses, so spans created in the subprocess become children of the
parent span.

callr only depends on the [otel](https://otel.r-lib.org) API package.
When no SDK is loaded, all otel calls are cheap no-ops and callr behaves
exactly as before. To actually record telemetry you also need an SDK
such as [otelsdk](https://otelsdk.r-lib.org), which provides exporters
that send spans to a local file, an OTLP collector, or memory for
testing.

## Enabling tracing

The simplest way to turn tracing on for an R session is to set an
environment variable that picks an exporter, *before* loading callr. For
example, to write spans to a local JSON file:

``` sh
export OTEL_TRACES_EXPORTER=file
export OTEL_EXPORTER_OTLP_FILE_TRACES_PATH=/tmp/callr-traces.jsonl
R
```

See the [otelsdk documentation](https://otelsdk.r-lib.org) for the full
set of exporters (file, HTTP/OTLP, stdout/stderr, memory) and their
configuration variables. Once an SDK and exporter are configured, no
code changes are needed in callr-using code — the existing entry points
([`r()`](https://callr.r-lib.org/dev/reference/r.md), `r_session$new()`,
[`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md),
[`rscript()`](https://callr.r-lib.org/dev/reference/rscript.md)) start
emitting spans automatically.

## What callr emits

callr emits all spans under the instrumentation scope `org.r-lib.callr`.
The span hierarchy mirrors the API:

| Span name | Emitted by |
|----|----|
| [`callr::r`](https://callr.r-lib.org/dev/reference/r.md) | [`r()`](https://callr.r-lib.org/dev/reference/r.md) |
| [`callr::rscript`](https://callr.r-lib.org/dev/reference/rscript.md) | [`rscript()`](https://callr.r-lib.org/dev/reference/rscript.md) |
| [`callr::rcmd`](https://callr.r-lib.org/dev/reference/rcmd.md) | [`rcmd()`](https://callr.r-lib.org/dev/reference/rcmd.md) |
| [`callr::r_process`](https://callr.r-lib.org/dev/reference/r_process.md) | `r_process$new()` / [`r_bg()`](https://callr.r-lib.org/dev/reference/r_bg.md) (ends in `$get_result`) |
| [`callr::r_session`](https://callr.r-lib.org/dev/reference/r_session.md) | `r_session$new()` (ends in `$close()` or finalizer) |
| `r_session$initialize() wait` | blocking startup wait in `r_session$new()` |
| `r_session$call` | each `r_session$call()` / `$run()` |
| `r_session$read` | each `r_session$read()` |
| `r_session$close` | `r_session$close()` |
| `callr subprocess` | top-level span inside each subprocess (root of child) |

Attributes attached to the parent-side spans include the resolved
`options` for the subprocess (binary path, command-line args, working
directory, environment, etc.), which makes it easy to correlate a trace
with the exact callr invocation that produced it.

`r_session$read` spans additionally carry a `message` boolean (whether a
message was read) and a `status_code` matching the callr communication
protocol (e.g. `200` for a finished call, `301` for a subprocess message
— see the [Persistent R
Sessions](https://callr.r-lib.org/dev/articles/r-session.md) article for
the full list).

If the function or script evaluated in the subprocess errors, callr
records an `exception` event on the relevant parent span with the error
message, class and traceback as attributes.

## Subprocess context propagation

When tracing is active, callr injects the [W3C Trace
Context](https://www.w3.org/TR/trace-context/) headers into the
subprocess via environment variables:

- `TRACEPARENT` — the active parent span context
- `TRACESTATE` — vendor-specific trace state (if any)
- `BAGGAGE` — OpenTelemetry baggage (if any)

Inside the subprocess, callr’s startup hook reads these env vars,
extracts the parent span context, and opens a top-level
`callr subprocess` span as a child of it. Any spans you create in the
subprocess (with otel, or with another instrumented package) are then
automatically parented to that span, producing a single connected trace
that spans the process boundary.

You don’t have to do anything for this to work — it’s handled by the
load hook that callr injects into every subprocess.

## Tracer name

callr identifies itself as `org.r-lib.callr` to OpenTelemetry, set via
the `otel_tracer_name` package symbol. SDKs and backends use this name
as the instrumentation scope, so you can filter, group or route callr
spans separately from other instrumented packages in your trace viewer.

## Testing instrumentation

otelsdk ships a `with_otel_record()` helper that captures spans into an
in-memory buffer. This is useful for inspecting what callr emits, or for
writing tests of your own instrumentation that build on callr.

``` r

out <- otelsdk::with_otel_record({
  callr::r(function() 1 + 1)
})
out$value
```

    #> [1] 2

``` r

vapply(out$traces, "[[", character(1), "name")
```

    #>   callr::r 
    #> "callr::r"

The recorded span has the expected name and instrumentation scope:

``` r

out$traces[[1]]$name
```

    #> [1] "callr::r"

``` r

out$traces[[1]]$instrumentation_scope$name
```

    #> [1] "org.r-lib.callr"

``` r

out$traces[[1]]$status
```

    #> [1] "ok"

For an `r_session`, you get one span per lifecycle event, all parented
to the top-level
[`callr::r_session`](https://callr.r-lib.org/dev/reference/r_session.md)
span:

``` r

out <- otelsdk::with_otel_record({
  rs <- callr::r_session$new()
  v <- rs$run(function() 1 + 1)
  rs$close()
  v
})
data.frame(
  name   = vapply(out$traces, "[[", character(1), "name"),
  parent = vapply(out$traces, "[[", character(1), "parent")
)
```

    #>                          name           parent
    #> 1              r_session$read fad0d0d49c9ec97a
    #> 2 r_session$initialize() wait fad0d0d49c9ec97a
    #> 3              r_session$call fad0d0d49c9ec97a
    #> 4              r_session$read fad0d0d49c9ec97a
    #> 5            callr::r_session 0000000000000000
    #> 6             r_session$close fad0d0d49c9ec97a

A subprocess error is recorded as an `exception` event:

``` r

out <- otelsdk::with_otel_record({
  tryCatch(callr::r(function() stop("boom")), error = function(e) NULL)
})
vapply(out$traces[[1]]$events, "[[", character(1), "name")
```

    #> [1] "exception"

And you can verify trace context propagation by inspecting `TRACEPARENT`
inside the subprocess:

``` r

out <- otelsdk::with_otel_record({
  callr::r(function() Sys.getenv("TRACEPARENT"))
})
out$value
```

    #> [1] "00-c7acf62c274daf46679c1bc6e77a48dd-0da9759e45cc39ca-01"

``` r

sprintf(
  "00-%s-%s-01",
  out$traces[[1]]$trace_id,
  out$traces[[1]]$span_id
)
```

    #> [1] "00-c7acf62c274daf46679c1bc6e77a48dd-0da9759e45cc39ca-01"

The two strings match: the `TRACEPARENT` the subprocess sees encodes the
trace and span IDs of the parent’s
[`callr::r`](https://callr.r-lib.org/dev/reference/r.md) span.

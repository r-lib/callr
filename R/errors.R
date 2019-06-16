
# # Standalone file for better error handling ----------------------------
#
# If can allow package dependencies, then you are probably better off
# using rlang's functions for errors.
#
# The canonical location of this file is in the processx package:
# https://github.com/r-lib/processx/master/R/errors.R
#
# ## Features
#
# - Throw conditions and errors with the same API.
# - Automatically captures the right calls and adds them to the conditions.
# - Sets `.Last.error`, so you can easily inspect the errors, even if they
#   were not caught.
# - It only sets `.Last.error` for the errors that are not caught.
# - Hierarchical errors, to allow higher level error messages, that are
#   more meaningful for the users, while also keeping the lower level
#   details in the error object. (So in `.Last.error` as well.)
# - `.Last.error` always includes a stack trace. (The stack trace is
#   common for the whole error hierarchy.)
#
# ## API
# ```
# new_cond(..., call. = TRUE, domain = NULL)
# new_error(..., call. = TRUE, domain = NULL)
# throw(cond, parent = NULL)
# catch_rethrow(expr, ...)
# rethrow(expr, cond)
# rethrow_call(.NAME, ...)
# add_trace_back(cond)
# ```
#
# ## Roadmap:
# - better printing of the error
# - better programmatic trace API (so we can capture it in the subprocess,
#   copy it back to the main process, and create a nice error object).
# - print source references in the errors
# - print source references in the trace
#
# ## NEWS:
# - date of first release will be here

err <- local({

  # -- condition constructors -------------------------------------------

  #' Create a new condition
  #'
  #' @noRd
  #' @param ... Parts of the error message, they will be converted to
  #'   character and then concatenated, like in [stop()].
  #' @param call. A call object to include in the condition, or `TRUE`
  #'   or `NULL`, meaning that [throw()] should add a call object
  #'   automatically.
  #' @param domain Translation domain, see [stop()].
  #' @return Condition object. Currently a list, but you should not rely
  #'   on that.

  new_cond <- function(..., call. = TRUE, domain = NULL) {
    message <- .makeMessage(..., domain = domain)
    structure(
      list(message = message, call = call.),
      class = c("condition"))
  }

  #' Create a new error condition
  #'
  #' It also adds the `rlib_error` class.
  #'
  #' @noRd
  #' @param ... Passed to [new_cond()].
  #' @param call. Passed to [new_cond()].
  #' @param domain Passed to [new_cond()].
  #' @return Error condition object with classes `rlib_error`, `error`
  #'   and `condition`.

  new_error <- function(..., call. = TRUE, domain = NULL) {
    cond <- new_cond(..., call. = call., domain = domain)
    class(cond) <- c("rlib_error", "error", "condition")
    cond
  }

  # -- throwing conditions ----------------------------------------------

  #' Throw a condition
  #'
  #' If the condition is an error, it will also call [stop()], after
  #' signalling the condition first. This means that if the condition is
  #' caught by an exiting handler, then [stop()] is not called.
  #'
  #' @noRd
  #' @param cond Condition object to throw. If it is an error condition,
  #'   then it calls [stop()].
  #' @param parent Parent condition. Use this within [rethrow()] and
  #'   [catch_rethrow()].

  throw <- function(cond, parent = NULL) {
    if (!inherits(cond, "condition")) {
      throw(new_error("You can only throw conditions"))
    }
    if (!is.null(parent) && !inherits(parent, "condition")) {
      throw(new_error("Parent condition must be a condition object"))
    }

    if (is.null(cond$call) || isTRUE(cond$call)) cond$call <- sys.call(-1)

    # Eventually the nframe numbers will help us print a better trace
    # When a child condition is created, the child will use the parent
    # error object to make note of its own nframe. Here we copy that back
    # to the parent.
    if (is.null(cond$nframe)) cond$nframe <- sys.parent()
    if (!is.null(parent)) {
      cond$parent <- parent
      cond$call <- cond$parent$childcall
      cond$parent$childcall <- NULL
      cond$nframe <- cond$parent$childframe
      cond$parent$childframe <- NULL
    }

    signalCondition(cond)

    # If this is not an error, then we'll just return here. This allows
    # throwing interrupt conditions for example, with the same UI.
    if (! inherits(cond, "error")) return(invisible())

    # If we get here that means that the condition was not caught by
    # an exiting handler. That means that we need to create a trace.
    cond <- add_trace_back(cond)

    # Set up environment to store .Last.error, it will be just before
    # baseenv(), so it is almost as if it was in baseenv() itself, like
    # .Last.value. We save the print methos here as well, and then they
    # will be found automatically.
    if (! "org:r-lib" %in% search()) {
      do.call("attach", list(new.env(), pos = length(search()),
                             name = "org:r-lib"))
    }
    env <- as.environment("org:r-lib")
    env$print.rlib_error <- print_rlib_error
    env$print.rlib_trace <- print_rlib_trace
    env$.Last.error <- cond
    env$.Last.error.trace <- cond$trace

    # Top-level handler, this is intended for testing only for now,
    # and its design might change.
    if (!is.null(th <- getOption("rlib_error_handler")) &&
        is.function(th)) {
      th(cond)

    } else {
      # Dropping the classes and adding "duplicate_condition" is a workaround
      # for the case when we have non-exiting handlers on throw()-n
      # conditions. These would get the condition twice, because stop()
      # will also signal it. If we drop the classes, then only handlers
      # on "condition" objects (i.e. all conditions) get duplicate signals.
      # This is probably quite rare, but for this rare case they can also
      # recognize the duplicates from the "duplicate_condition" extra class.
      class(cond) <- c("duplicate_condition", "condition")
      stop(cond)
    }
  }

  # -- rethrowing conditions --------------------------------------------

  #' Catch and re-throw conditions
  #'
  #' See [rethrow()] for a simpler interface that handles `error`
  #' conditions automatically.
  #'
  #' @noRd
  #' @param expr Expression to evaluate.
  #' @param ... Condition handler specification, the same way as in
  #'   [withCallingHandlers()]. You are supposed to call [throw()] from
  #'   the error handler, with a new error object, setting the original
  #'   error object as parent. See examples below.
  #' @examples
  #' f <- function() {
  #'   ...
  #'   err$catch_rethrow(
  #'     ... code that potentially errors ...,
  #'     error = function(e) {
  #'       throw(new_error("This will be the child error"), parent = e)
  #'     }
  #'   )
  #' }

  catch_rethrow <- function(expr, ...) {
    realcall <- sys.call(-1)
    realframe <- sys.parent()
    parent <- parent.frame()

    cl <- match.call()
    cl[[1]] <- quote(withCallingHandlers)
    handlers <- list(...)
    for (h in names(handlers)) {
      cl[[h]] <- function(e) {
        # This will be NULL if the error is not throw()-n
        if (is.null(e$nframe)) e$nframe <- sys.parent()
        e$childcall <- realcall
        e$childframe <- realframe
        handlers[[h]](e)
      }
    }
    eval(cl, envir = parent)
  }

  #' Catch and re-throw conditions
  #'
  #' `rethrow()` is similar to [catch_rethrow()], but it has a simpler
  #' interface. It catches conditions with class `error`, and re-throws
  #' `cond` instead, using the original condition as the parent.
  #'
  #' @noRd
  #' @param expr Expression to evaluate.
  #' @param ... Condition handler specification, the same way as in
  #'   [withCallingHandlers()].

  rethrow <- function(expr, cond) {
    realcall <- sys.call(-1)
    realframe <- sys.parent()
    withCallingHandlers(
      expr,
      error = function(e) {
        # This will be NULL if the error is not throw()-n
        if (is.null(e$nframe)) e$nframe <- sys.parent()
        e$childcall <- realcall
        e$childframe <- realframe
        throw(cond, parent = e)
      }
    )
  }

  #' Version of .Call that throw()s errors
  #'
  #' It re-throws error from interpreted code. If the error had class
  #' `simpleError`, like all errors, thrown via `error()` in C do, it also
  #' adds the `c_error` class.
  #'
  #' @noRd
  #' @param .NAME Compiled function to call, see [.Call()].
  #' @param ... Function arguments, see [.Call()].
  #' @return Result of the call.

  rethrow_call <- function(.NAME, ...) {
    call <- sys.call()
    nframe <- sys.nframe()
    withCallingHandlers(
      # do.call to work around an R CMD check issue
      do.call(".Call", list(.NAME, ...)),
      error = function(e) {
        e$nframe <- nframe
        e$call <- call
        if (inherits(e, "simpleError")) {
          class(e) <- c("c_error", "rlib_error", "error", "condition")
        }
        throw(e)
      }
    )
  }

  # -- create traceback -------------------------------------------------

  #' Create a traceback
  #'
  #' [throw()] calls this function automatically if an error is not caught,
  #' so there is currently not much use to call it directly.
  #'
  #' @param cond Condition to add the trace to
  #'
  #' @return A condition object, with the trace added.

  add_trace_back <- function(cond) {
    idx <- seq_len(sys.parent(1L))
    frames <- sys.frames()[idx]

    parents <- sys.parents()[idx]
    calls <- as.list(sys.calls()[idx])
    envs <- lapply(frames, env_label)
    nframes <- if (!is.null(cond$nframe)) cond$nframe else sys.parent()
    messages <- list(conditionMessage(cond))

    if (is.null(cond$parent)) {
      # Nothing to do, no parent

    } else if (is.null(cond$parent$trace)) {
      # If the parent does not have a trace, that means that it is using
      # the same trace as us.
      parent <- cond
      while (!is.null(parent <- parent$parent)) {
        nframes <- c(nframes, parent$nframe)
        messages <- c(messages, list(conditionMessage(parent)))
      }

    } else {
      # If it has a trace, that means that it is coming from another
      # process or top level evaluation. In this case we'll merge the two
      # traces.
      pt <- cond$parent$trace
      parents <- c(parents, pt$parents + length(calls))
      nframes <- c(nframes, pt$nframes + length(calls))
      envs <- c(envs, pt$envs)
      calls <- c(calls, pt$calls)
      messages <- c(messages, pt$messages)
    }

    cond$trace <- new_trace(calls, parents, envs, nframes, messages)
    cond
  }

  new_trace <- function (calls, parents, envs, nframes, messages) {
    indices <- seq_along(calls)
    structure(
      list(calls = calls, parents = parents, envs = envs,
           indices = indices, nframes = nframes, messages = messages),
      class = "rlib_trace")
  }

  env_label <- function(env) {
    nm <- env_name(env)
    if (nzchar(nm)) {
      nm
    } else {
      env_address(env)
    }
  }

  env_address <- function(env) {
    class(env) <- "environment"
    sub("^.*(0x[0-9a-f]+)>$", "\\1", format(env), perl = TRUE)
  }

  env_name <- function(env) {
    if (identical(env, globalenv())) {
      return("global")
    }
    if (identical(env, baseenv())) {
      return("package:base")
    }
    if (identical(env, emptyenv())) {
      return("empty")
    }
    nm <- environmentName(env)
    if (isNamespace(env)) {
      return(paste0("namespace:", nm))
    }
    nm
  }

  # -- printing ---------------------------------------------------------

  print_rlib_error <- function(x, ...) {
    ## TODO: better printing
    NextMethod("print")
    if (!is.null(x$parent)) {
      cat("-->\n")
      print(x$parent)
    }
    invisible(x)
  }

  print_rlib_trace <- function(x, ...) {
    callstr <- vapply(x$calls, format_call, character(1))
    callstr[x$nframes] <-
      paste0(callstr[x$nframes], "\n--> ERROR: ", x$messages, "\n")

    # Drop the machinery to create parent errors. For both catch_rethrow()
    # and rethrow() we need to drop until the next withCallingHandlers call.
    wch <- vapply(
      x$calls,
      function(x) identical(x[[1]], quote(withCallingHandlers)), logical(1))
    wchidx <- which(wch)
    drop_from <- x$nframes + 1L
    drop_to <- vapply(x$nframes, function(x) wchidx[wchidx >= x][1], 1L)
    drop_from <- drop_from[!is.na(drop_to)]
    drop_to <- drop_to[!is.na(drop_to)]
    drop <- unlist(mapply(FUN = seq, drop_from, drop_to, SIMPLIFY = FALSE))

    # Drop the tail, that is usually not interesting (?), especially for
    # parent errors
    last_nframe <- x$nframes[length(x$nframes)]
    if (length(callstr) > last_nframe) {
      drop <- c(drop, seq(last_nframe + 1L, length(callstr)))
    }
    if (length(drop)) callstr <- callstr[-drop]

    cat(enumerate(callstr), sep = "\n")
    invisible(x)
  }

  format_call <- function(call) {
    width <- getOption("width")
    str <- format(call)
    if (length(str) > 1 || nchar(str[1]) > width) {
      paste0(substr(str[1], 1, width - 5), " ...")
    } else {
      str[1]
    }
  }

  enumerate <- function(x) paste0(seq_along(x), ". ", x)

  structure(
    list(
      .internal      = environment(),
      new_cond       = new_cond,
      new_error      = new_error,
      throw          = throw,
      rethrow        = rethrow,
      catch_rethrow  = catch_rethrow,
      rethrow_call   = rethrow_call,
      add_trace_back = add_trace_back
    ),
    class = c("standalone_errors", "standalone"))
})

# These are optional, and feel free to remove them if you prefer to
# call them through the `err` object.

new_cond  <- err$new_cond
new_error <- err$new_error
throw     <- err$throw
rethrow   <- err$rethrow
rethrow_call <- err$rethrow_call

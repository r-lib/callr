
os_type <- function() {
  .Platform$OS.type
}

is_windows <- function() {
  .Platform$OS.type == "windows"
}

is_linux <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "linux")
}

last_char <- function(x) {
  nc <- nchar(x)
  substring(x, nc, nc)
}

# Given a filename, return an absolute path to that file. This has two important
# differences from normalizePath(). (1) The file does not need to exist, and (2)
# the path is merely absolute, whereas normalizePath() returns a canonical path,
# which resolves symbolic links, gives canonical case, and, on Windows, may give
# short names.
#
# On Windows, the returned path includes the drive ("C:") or network server
# ("//myserver").
full_path <- function(path) {
  assert_that(is_string(path))

  # Try expanding "~"
  path <- path.expand(path)

  # If relative path, prepend current dir. On Windows, also record current
  # drive.
  if (is_windows()) {
    path <- gsub("\\", "/", path, fixed = TRUE)

    if (grepl("^[a-zA-Z]:", path)) {
      drive <- substring(path, 1, 2)
      path <- substring(path, 3)

    } else if (substring(path, 1, 2) == "//") {
      # Extract server name, like "//server", and use as drive.
      pos <- regexec("^(//[^/]*)(.*)", path)[[1]]
      drive <- substring(path, pos[2], attr(pos, "match.length", exact = TRUE)[2])
      path <- substring(path, pos[3])

      # Must have a name, like "//server"
      if (drive == "//")
        stop("Server name not found in network path.")

    } else {
      drive <- substring(getwd(), 1, 2)

      if (substr(path, 1, 1) != "/")
        path <- substring(file.path(getwd(), path), 3)
    }

  } else {
    if (substr(path, 1, 1) != "/")
      path <- file.path(getwd(), path)
  }

  parts <- strsplit(path, "/")[[1]]

  # Collapse any "..", ".", and "" in path.
  i <- 2
  while (i <= length(parts)) {
    if (parts[i] == "." || parts[i] == "") {
      parts <- parts[-i]

    } else if (parts[i] == "..") {
      if (i == 2) {
        parts <- parts[-i]
      } else {
        parts <- parts[-c(i-1, i)]
        i <- i-1
      }
    } else {
      i <- i+1
    }
  }

  new_path <- paste(parts, collapse = "/")
  if (new_path == "")
    new_path <- "/"

  if (is_windows())
    new_path <- paste0(drive, new_path)

  new_path
}

vcapply <- function (X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X, FUN, FUN.VALUE = character(1), ..., USE.NAMES = USE.NAMES)
}

do_echo_cmd <- function(command, args) {
  quoted <- sh_quote_smart(c("Running", command, args))

  out <- str_wrap_words(quoted, width = getOption("width") - 3)

  if ((len <- length(out)) > 1) {
    out[1:(len - 1)] <- paste0(out[1:(len - 1)], " \\")
  }
  cat(out, sep = "\n")
}

sh_quote_smart <- function(x) {
  if (!length(x)) return(x)
  ifelse(grepl("^[-a-zA-Z0-9/_\\.]*$", x), x, shQuote(x))
}

str_wrap_words <- function(words, width, indent = 0, exdent = 2) {
  word_widths <- nchar(words, type = "width")
  out <- character()

  current_width <- indent
  current_line <- strrep(" ", indent)
  first_word <- TRUE

  i <- 1
  while (i <= length(words)) {
    if (first_word) {
      current_width <- current_width + word_widths[i]
      current_line <- paste0(current_line, words[i])
      first_word <- FALSE
      i <- i + 1

    } else if (current_width + 1 + word_widths[i] <= width) {
      current_width <- current_width + word_widths[i] + 1
      current_line <- paste0(current_line, " ", words[i])
      i <- i + 1

    } else {
      out <- c(out, current_line)
      current_width <- exdent
      current_line <- strrep(" ", exdent)
      first_word <- TRUE
    }
  }

  if (!first_word) out <- c(out, current_line)

  out
}

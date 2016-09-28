# this does not handle LCOV_EXCL_START ect.
parse_gcov <- function(file, path = ".") {
  if (!file.exists(file)) {
    return(NULL)
  }

  lines <- readLines(file)
  source_file <- rex::re_matches(lines[1], rex::rex("Source:", capture(name = "source", anything)))$source

  # If the source file starts with a absolute path ignore it
  if (is_absolute_path(source_file)) {
    return(NULL)
  }

  # retrieve full path to the source files
  file <- normalize_path(file.path(path, source_file))

  if (!file.exists(file)) {
    is_root <- function(path) {
      identical(path, dirname(path))
    }
    while(!is_root(path) && !file.exists(file)) {
      path <- dirname(path)
      file <- normalize_path(file.path(path, source_file))
    }
    if (!file.exists(file)) {
      return(NULL)
    }
  }
  source_file <- file

  re <- rex::rex(any_spaces,
    capture(name = "coverage", some_of(digit, "-", "#", "=")),
    ":", any_spaces,
    capture(name = "line", digits),
    ":"
  )

  matches <- rex::re_matches(lines, re)
  # gcov lines which have no coverage
  matches$coverage[matches$coverage == "#####"] <- 0 # nolint

  # gcov lines which have parse error, so make untracked
  matches$coverage[matches$coverage == "====="] <- "-"

  coverage_lines <- matches$line != "0" & matches$coverage != "-"
  matches <- matches[coverage_lines, ]

  values <- as.numeric(matches$coverage)

  # create srcfile reference from the source file
  src_file <- srcfilecopy(source_file, readLines(source_file))

  line_lengths <- vapply(src_file$lines[as.numeric(matches$line)], nchar, numeric(1))

  if (any(is.na(values))) {
    stop("values could not be coerced to numeric ", matches$coverage)
  }

  res <- Map(function(line, length, value) {
    src_ref <- srcref(src_file, c(line, 1, line, length))
    res <- list(srcref = src_ref, value = value, functions = NA_character_)
    class(res) <- "line_coverage"
    res
  },
  matches$line, line_lengths, values)

  if (!length(res)) {
    return(NULL)
  }

  names(res) <- lapply(res, function(x) key(x$srcref))

  class(res) <- "line_coverages"
  res
}

clean_gcov <- function(path) {
  src_dir <- file.path(path, "src")

  gcov_files <- list.files(src_dir,
                    pattern = rex::rex(or(".gcda", ".gcno", ".gcov"), end),
                    full.names = TRUE,
                    recursive = TRUE)

  unlink(gcov_files)
}

run_gcov <- function(path, quiet = TRUE,
                      gcov_path = getOption("covr.gcov", ""),
                      gcov_args = getOption("covr.gcov_args", NULL)) {
  if (!nzchar(gcov_path)) {
    return()
  }

  src_path <- normalize_path(file.path(path, "src"))
  if (!file.exists(src_path)) {
     return()
  }

  res <- unlist(recursive = FALSE,
    lapply(list.dirs(src_path, recursive = TRUE),
    function(dir) {
      withr::with_dir(dir, {
        gcov_inputs <- list.files(dir, pattern = rex::rex(".gcno", end), recursive = TRUE, full.names = TRUE)
        if (length(gcov_inputs) > 0) {
          system_check(gcov_path, args = c(gcov_args, gcov_inputs), quiet = quiet, echo = !quiet)
          gcov_outputs <- list.files(dir, pattern = rex::rex(".gcov", end), recursive = TRUE, full.names = TRUE)
          unlist(recursive = FALSE, lapply(gcov_outputs, parse_gcov, path = dir))
      }
  })}))

  structure(res, class = "coverage")
}

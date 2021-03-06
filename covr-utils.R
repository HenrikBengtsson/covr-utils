#################################################################
# Test coverage
#
# * covr: https://github.com/jimhester/covr
# * Coveralls: https://coveralls.io/
#
# Henrik Bengtsson
#################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Filters
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Don't report on lines according to '# covr:' rules in file
## Exclusions applied if 'covr: <rule>' is detected in file:
##  skip=all : Skip all the lines in file
##  skip=2   : Skip the following two lines
covr_lines <- function(file) {
  lines <- read_lines(file)
  nlines <- length(lines)
  pattern <- ".*#[ ]*covr:[ ]*([^#]*).*"
  idxs <- grep(pattern, lines)
  excludes <- lapply(idxs, FUN=function(idx) {
    rule <- gsub(pattern, "\\1", lines[idx])
    rule <- unlist(strsplit(rule, split=",", fixed=TRUE))
    rule <- gsub("[ \t]", "", rule)
    rule <- strsplit(rule, split="=", fixed=TRUE)
    actions <- sapply(rule, FUN=`[`, 1L)
    stopifnot(!any(duplicated(actions)))
    values <- sapply(rule, FUN=`[`, 2L)
    names(values) <- actions
    excl <- integer(0L)
    skip <- values[actions == "skip"]
    if (length(skip) > 0) {
      if (skip == "all") {
        idxs <- seq_len(nlines)
      } else {
        idxs <- seq(from=idx+1L, to=min(nlines, idx + as.integer(skip)))
      }
      idxs <- idxs[idxs <= nlines]
      excl <- c(excl, idxs)
    }
    excl
  })
  excludes <- unlist(excludes)
  if (length(excludes) > 0) excludes <- unique(sort(excludes))
  if (is.null(excludes)) excludes <- integer(0)
  excludes
}

## Don't report on any line
all_lines <- function(file) {
  lines <- read_lines(file)
  seq_along(lines)
}

## Don't report on stop() lines
stop_lines <- function(file) {
  grep("(^|[ \t])(abort|stop|throw|.Defunct|.Deprecated)[(]", read_lines(file))
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Local functions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read_lines <- function(file, ..., encoding="latin1") {
  con <- file(file, encoding=encoding)
  on.exit(close(con))
  readLines(con=con, ...)
}

exclusions <- function(...) {
  structure(c(...), class="exclusions")
}

as.exclusions <- function(excl) {
  if (!inherits(excl, "exclusions")) excl <- exclusions(excl)
  excl
}

c.exclusions <- function(excl=list(), new) {
  excl <- as.exclusions(excl)
  for (name in names(new)) {
    excl[[name]] <- unique(sort(c(excl[[name]], new[[name]])))
  }
  as.exclusions(excl)
}

read.exclusion <- function(excl) {
  mapply(names(excl), excl, FUN=function(file, idxs) {
    read_lines(file)[idxs]
  })
}

excl_files <- function(files, ..., max_lines=10e3) {
  excl <- lapply(c(files, ...), FUN=function(...) seq_len(max_lines))
  names(excl) <- files
  as.exclusions(excl)
}

filter <- function(files, FUN, ...) {
  as.exclusions(sapply(files, FUN=FUN, ...))
}

files <- function(path, pattern, ...) {
  dir(path=path, pattern=pattern, full.names=TRUE, ignore.case=TRUE)
}

r_files <- function(...) {
  files(path="R", pattern="[.]R$", ...)
}

src_files <- function(...) {
  files(path="src", pattern="[.](h|c|cpp|hpp|f)$", ...)
}

all_files <- function(...) {
  c(r_files(), src_files())
}

use_covr <- function(quiet=TRUE) {
  repos <- c("jimhester/covr", "HenrikBengtsson/covr")[1]
  ## Install 'covr'?
  if (!requireNamespace("covr", quietly=TRUE)) {
    if (file_test("-f", "pkg-build.sh")) {
      system2("./pkg-build.sh", args=c("install_github", repos))
    } else if (file_test("-f", "travis-tool.sh")) {
      system2("./travis-tool.sh", args=c("install_github", repos))
    } else {
      source(sprintf("http://callr.org/install#%s", repos))
    }
  }

  ## Load package
  loadNamespace("covr")

  pkg <- packageDescription("covr")
  if (!quiet) print(pkg)

  invisible(pkg)
}

covr_package <- function(..., quiet=FALSE, target=c("codecov", "coveralls")[2], encoding="latin1", warn=1L) {
  oopts <- options(encoding=encoding, warn=warn)
  on.exit(options(oopts))

  use_covr()
  coverage <- covr::package_coverage(..., quiet=quiet)
  if (!quiet) {
    cat("\n\n")
    print(coverage)
    cat("\n\n")
  }

  if (!interactive()) {
    tryCatch({
      if (target == "codecov") {
        covr::codecov(coverage=coverage, quiet=TRUE, ...)
      } else if (target == "coveralls") {
        covr::coveralls(coverage=coverage, quiet=TRUE, ...)
      }
    }, error = function(ex) {
      warning("A non-critical error was detected: ", ex$msg)
    })
  }

  invisible(coverage)
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Main
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Load/install 'covr'
use_covr(quiet=FALSE)

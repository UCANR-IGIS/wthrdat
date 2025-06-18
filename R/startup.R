## Create an environment to use for caching smallish tables
cache_env <- new.env()

.onLoad <- function(lib, pkg) {
  # message("Welcome to the package")
}

.onAttach <- function(lib, pkg) {
  ver <- read.dcf(file.path(lib,pkg,"DESCRIPTION"), "Version")
  bug_reports <- read.dcf(file.path(lib,pkg,"DESCRIPTION"), "BugReports")
  msg <- paste0(sprintf("wthrdat (version %s)", as.character(ver)), "\n",
                "Bug reports: ", bug_reports)
  packageStartupMessage(msg)
}

#' Download weather station data
#'
#' @param src Weather data source / provider
#' @param stid Station name(s)
#' @param start_dt Start date-time
#' @param end_dt End date-time
#' @param var Standardized weather variables
#' @param per Period / interval (minutes)
#' @param units Units desired (imperial or metric)
#' @param key API key
#' @param tz Time Zone for the results
#' @param cache_dir A local directory for caching the results (optional)
#' @param session Shiny session (for showing a spinner)
#' @param spinner Show a spinner when fetching data,logical
#' @param quiet Suppress messages
#'
#' @details
#' This will query station data from supported APIs.
#'
#' If you pass a value for \code{cache_dir}, downloaded data will be saved in that location. The function however
#' does not clear the \code{cache_dir} upon closing, so it is recommended you use a temporary directory.
#'
#' @returns A weather data tibble (long format)
#'
#' @seealso \code{\link{wd_getdata_syn}}, \code{\link{wd_getdata_wwg}}
#'
#' @export

wd_getdata <- function(src = c("syn", "wwg")[1], stid, start_dt, end_dt, var, key, per = NULL, units = NULL,
                       tz = Sys.timezone(), cache_dir = NULL, session = NULL, spinner = FALSE, quiet = FALSE) {

  wd_getdata_checks(start_dt, end_dt, tz, src, units, var, cache_dir, session, spinner)

  if (tolower(src) == "wwg") {
    wd_getdata_wwg(stid = stid, start_dt = start_dt, end_dt = end_dt, var = var, per = per, units = units, key = key,
                   tz = tz, cache_dir = cache_dir, session = session, spinner = spinner, quiet = quiet)

  } else if (tolower(src) == "syn") {
    wd_getdata_syn(stid = stid, start_dt = start_dt, end_dt = end_dt, var = var, per = per, units = units, key = key,
                   tz = tz, cache_dir = cache_dir, session = session, spinner = spinner, quiet = quiet)
  }

}

#' Parameter checks
#'
#' @param src Weather data source / provider
#' @param start_dt Start date-time
#' @param end_dt End date-time
#' @param var Standardized weather variables
#' @param units Units desired (imperial or metric)
#' @param tz Time Zone for the results
#' @param cache_dir Directory for caching
#' @param session Shiny session (for showing a spinner)
#' @param spinner Show a spinner when fetching data,logical
#'
#' @details
#' This internal function checks the parameters passed to wd_getdata() and wd_getdata_xxx()
#'
#' @returns TRUE if all checks are passed
#'
#' @importFrom cli cli_abort
#' @importFrom curl has_internet
#' @importFrom rlang local_options

wd_getdata_checks <- function(start_dt, end_dt,
                              tz, src, units, var,
                              cache_dir, session, spinner) {

  ## Suppress ANSI escape codes from cli output if we're not in an interactive session
  if (!interactive()) {local_options(cli.num_colors = 1L)}

  if (!inherits(start_dt, "POSIXct")) cli_abort("{.var start_dt} must be a POSIXct object")
  if (!inherits(end_dt, "POSIXct")) cli_abort("{.var end_dt} must be a POSIXct object")
  if (start_dt >= end_dt) cli_abort("{.var end_dt} must be later than {.var start_dt}")
  if (!tz %in% OlsonNames()) cli_abort(c(
    "{.var {tz}} is not a recognized timezone",
    "i" = "To see valid timezones, run {.fn OlsonNames}"))

  ## Check the src argument for length and value
  if (length(src) != 1) cli_abort("{.var src} must be of length 1")
  src_valid_chr <- unique(srcs_tbl$src)
  if (!src %in% src_valid_chr) cli_abort("{.var src} must be one of {src_valid_chr}")

  if (!is.null(units)) {
    if (!tolower(units) %in% c("imperial", "metric")) {
      cli_abort("{.var units} should be 'imperial' or 'metric'")
    }
  }
  if (FALSE %in% (var %in% vars_tbl$var)) {
    cli_abort("Unknown variable{?s}: {var[!var %in% vars_tbl$var]}")
  }

  if (!is.null(session) && spinner) {
    if (!requireNamespace("shinybusy")) cli_abort("{.pkg shinybusy} is required to display a spinner")
  }

  if (!is.null(cache_dir)) {
    if (!dir.exists(cache_dir)) cli_abort("Can't find {.var cache_dir}")
  }

  if (!has_internet()) {
    cli_abort("No internet connection")
  }

  invisible(TRUE)

}

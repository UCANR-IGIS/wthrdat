#' Download weather station data
#'
#' @param src Weather data source / provider
#' @param stid Station name(s)
#' @param start_dt Start date-time
#' @param end_dt End date-time
#' @param var Standardized weather variables
#' @param per Period / interval (minutes)
#' @param key API key
#' @param tz Time Zone for the results
#'
#' @details
#' This will query station data from different APIs.
#'
#' @returns A weather data tibble (long format)
#'
#' @seealso \code{\link{wd_getdata_syn}}, \code{\link{wd_getdata_wwg}}
#'#'
#' @importFrom cli cli_abort cli_alert_warning cli_alert_info cli_alert_success cli_progress_done cli_progress_step cli_li
#' @importFrom rlang arg_match
#' @export

wd_getdata <- function(src = c("syn", "wwg")[1], stid, start_dt, end_dt, var, key, per = NULL, units = NULL, tz = Sys.timezone()) {


  if (!inherits(start_dt, "POSIXct")) cli_abort("{.var start_dt} must be a POSIXct object")
  if (!inherits(end_dt, "POSIXct")) cli_abort("{.var end_dt} must be a POSIXct object")
  if (start_dt >= end_dt) cli_abort("{.var end_dt} must be later than {.var start_dt}")
  if (!tz %in% OlsonNames()) cli_abort(c(
    "{.var {tz}} is not a recognized timezone",
    "i" = "To see valid timezones, run {.fn OlsonNames}"))

  ## Check the src argument for length and value
  src_valid_chr <- c("syn", "wwg")
  if (length(src) != 1) cli_abort("{.var src} must be one of {paste({src_valid_char}, collapse = ', ')}")
  if (!src %in% src_valid_chr) cli_abort("{.var src} must be one of {paste0('\\'',src_valid_chr, '\\'', collapse = ', ')}")

  if (!is.null(units)) {
    if (!tolower(units) %in% c("imperial", "metric")) {
      cli_abort("{.var units} should be 'imperial' or 'metric'")
    }
  }

  if (FALSE %in% (var %in% vars_tbl$var)) {
    cli_abort("Unknown variable(s): {paste(var[!var %in% vars_tbl$var], collapse = '', '')}")
  }

  if (tolower(src) == "wwg") {
    wd_getdata_wwg(stid = stid, start_dt = start_dt, end_dt = end_dt, var = var, per = per, units = units, key = key, tz = tz)

  } else if (tolower(src) == "syn") {
    wd_getdata_syn(stid = stid, start_dt = start_dt, end_dt = end_dt, var = var, per = per, units = units, key = key, tz = tz)

  }


}


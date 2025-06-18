#' Get station data from the Western Weather Group API
#'
#' @param stid Station name(s)
#' @param start_dt Start date-time
#' @param end_dt End date-time
#' @param var Standardized weather variables
#' @param per Period / interval (minutes)
#' @param key API key
#' @param tz Time Zone for the results
#'
#' @details
#' This will query station data from the Weather Weather Group API.
#' For the full documentation, see https://api.westernwx.com/docs/
#'
#' To use the WWG API, you must have an API key. If your account has API access, you can manage your API Keys from https://app.westernwx.com/apikeys. When you generate a new key, it will give you an Id, Secret, and a Key.
#'
#' @returns A weather data tibble (long format)
#'
#' @import httr2 tidyr dplyr purrr tibble
#' @importFrom lubridate ymd_hms
#' @importFrom stringr str_replace
#' @importFrom units set_units
#' @importFrom cli cli_abort cli_alert_warning cli_alert_info cli_alert_success cli_progress_done cli_progress_step cli_li
#' @export

wd_getdata_wwg <- function(stid, start_dt, end_dt, var, key, per = NULL, units = NULL, tz = Sys.timezone()) {

  ## I decided against using the units package here, because although you can have
  ## a vector of units with different units, most people won't know how to deal with that.
  ## So instead the units are saved as a separate column (as a factor).

  if (!inherits(start_dt, "POSIXct")) cli_abort("{.var start_dt} must be a POSIXct object")
  if (!inherits(end_dt, "POSIXct")) cli_abort("{.var end_dt} must be a POSIXct object")
  if (start_dt >= end_dt) cli_abort("{.var end_dt} must be later than {.var start_dt}")

  if (is.null(per)) cli_abort(c(
    "{.var per} is required for Western Weather Group",
    "i" = "Try 60. If that fails, ask your Western Weather Group support person for the time intervals supported on your account"))
  if (!tz %in% OlsonNames()) cli_abort("{tz} is not a recognized timezone.")

  if (!is.null(units)) {
    if (!tolower(units) %in% c("imperial", "metric")) {
      cli_abort("{.var units} should be 'imperial' or 'metric'")
    }
  }

  if (FALSE %in% (var %in% vars_tbl$var)) {
    cli_abort("Unknown variable(s): {paste(var[!var %in% vars_tbl$var], collapse = '', '')}")
  }

  cli_alert_info("TODO: error check the station names are valid")
  # message(" - TODO: add units = imperial|metric")

  src_chr <- "wwg"
  network_chr <- "wwg"
  wwg_baseurl <- "https://api.westernwx.com"

  ## Construct a tibble that maps the standardized weather variable names to WWG API fields
  src_var_filt_tbl <- src_var_tbl |>
    filter(var %in% .env$var, per == .env$per, src == .env$src_chr)
  if (FALSE %in% (var %in% src_var_filt_tbl$var)) {
    cli_abort("Cannot find an API field for
              {.var {paste(var[!var %in% src_var_filt_tbl$var], collapse = ', ')}}
              for this source and period.")
  }
  qry_flds <- src_var_filt_tbl$fld

  ## Create a tibble of the variables that need unit conversion
  if (is.null(units)) {
    var_units_target_tbl <- tibble(NULL)
  } else {
    var_units_target_tbl <- src_var_filt_tbl |>
      left_join(units_conv_tbl |> select(units, units_target := !!units), by = "units") |>
      filter(units != units_target) |>
      select(var, units, units_target)
  }

  start_chr <-
    start_dt |>
    format_ISO8601(usetz = TRUE) |>
    str_replace("00$", ":00")

  end_chr <-
    end_dt |>
    format_ISO8601(usetz = TRUE) |>
    str_replace("00$", ":00")

  ## Initialize objects for the result
  res_tbl <- NULL
  meta_tbl <- NULL

  ## Loop through the stations
  for (one_stid in stid) {

    ## Create the API request
    wwg_data_req <- request(wwg_baseurl) |>
      req_url_path_append("v2/stationdata") |>                     ## endpoint
      req_url_path_append(as.character(per) ) |>                   ## interval
      req_url_path_append(one_stid) |>                             ## station id
      req_url_path_append(start_chr) |>                            ## start date-time
      req_url_path_append(end_chr) |>                              ## end date-time
      req_url_query(fields = paste0(qry_flds, collapse = ",")) |>  ## fields
      req_headers(`X-Api-Key` = key,                               ## authentication
                  accept = "application/json")

    ## Perform the request
    cli_progress_step("Downloading weather data from WWG for station {.field {one_stid}}")
    wwg_data_resp <- req_perform(wwg_data_req)

    ## Check that we have a valid response
    if (resp_is_error(wwg_data_resp)) {
      cli_abort("The API request was not successful")
    } else {
      cli_progress_done()
    }

    ## Convert the body to a list
    wwg_data_lst <- resp_body_json(wwg_data_resp)

    ## Convert the list to a tibble
    obs_thisstid_tbl <- tibble(
      stid = map_chr(wwg_data_lst, "stationId"),
      date_chr = map_chr(wwg_data_lst, "date"),
      values = map(wwg_data_lst, "values")) |>
      unnest_longer(col = values, values_to = "val", keep_empty = TRUE) |>
      mutate(dt = ymd_hms(date_chr, tz = tz, quiet = TRUE),
             src = .env$src_chr,
             network = .env$network_chr,
             stid = stid,
             fld = val_id) |>
      left_join(select(src_var_filt_tbl, fld, var, units), by = "fld") |>
      select(src, network, stid, dt, var, val, units) |>
      arrange(dt)

    ## Modify the units if needed
    if (nrow(var_units_target_tbl) > 0) {
      for (var_idx in 1:nrow(var_units_target_tbl)) {
        api_units <- var_units_target_tbl[var_idx, "units", drop = TRUE]
        target_units <- var_units_target_tbl[var_idx, "units_target", drop = TRUE]
        rows_idx <- (obs_thisstid_tbl$var == var_units_target_tbl[var_idx, "var", drop = TRUE])
        cur_vals_units <- set_units(obs_thisstid_tbl[rows_idx, "val", drop = TRUE],
                                    value = api_units, mode = "standard")
        new_vals_units <- set_units(cur_vals_units, value = target_units, mode = "standard")
        obs_thisstid_tbl[rows_idx, "val"] <- as.numeric(new_vals_units)
        obs_thisstid_tbl[rows_idx, "units"] <- target_units
        # cli_alert_success("Converted the units for {.field {var_units_target_tbl[var_idx, 'var', drop = TRUE]}}")
      }
    }

    ## Append these rows to res_tbl
    res_tbl <- rbind(res_tbl, obs_thisstid_tbl)

    ## Append one row of metadata
    meta_tbl  <- rbind(meta_tbl, tibble(
      stid = one_stid,
      start_req_dt = start_dt,
      end_req_dt = end_dt,
      start_rec_dt = min(obs_thisstid_tbl$dt),
      end_rec_dt = max(obs_thisstid_tbl$dt)
    ))

    cli_alert_success("Parsed data for {.field {one_stid}}")

  }

  ## Convert character fields to factors
  res_tbl <- res_tbl |>
    mutate(across(c(src, network, stid, var, units), as.factor))

  ## Create a list for the attributes
  attrb_lst <- list(
    src = src_chr,
    network = network_chr,
    stid = stid,
    var = var,
    per = per,
    meta_tbl = meta_tbl
  )
  attributes(res_tbl) <- c(attributes(res_tbl), attrb_lst)

  ## Prepend class "wthr_df"
  class(res_tbl) <- c("wthr_tbl", class(res_tbl))

  ## Return the result
  res_tbl



  ## Return a tibble with columns src, network, stid, dt, var, val, units

}


#' Get station data from the Western Weather Group API
#'
#' @param stid Station name(s)
#' @param start_dt Start date-time
#' @param end_dt End date-time
#' @param var Standardized weather variables
#' @param per Period / interval (minutes)
#' @param units Units desired (imperial or metric)
#' @param key API key
#' @param tz Time Zone for the results
#' @param cache_dir Directory for caching
#' @param session Shiny session (for showing a spinner)
#' @param spinner Show a spinner when fetching data, logical
#' @param quiet Suppress messages
#'
#' @details
#' This will query station data from the Weather Weather Group API.
#' For the full documentation, see https://api.westernwx.com/docs/
#'
#' To use the WWG API, you must have an API key. If your account has API access, you can manage your API Keys from https://app.westernwx.com/apikeys. When you generate a new key, it will give you an Id, Secret, and a Key.
#'
#' If you pass a value for \code{cache_dir}, downloaded data will be saved in that location. The function however
#' does not clear the \code{cache_dir} upon closing, so it is recommended you use a temporary directory.

#' @returns A weather data tibble (long format)
#'
#' @import httr2 tidyr dplyr tibble
#' @importFrom lubridate ymd_hms format_ISO8601
#' @importFrom purrr map map_chr
#' @importFrom stringr str_replace
#' @importFrom units set_units
#' @importFrom rlang local_options
#' @importFrom cli cli_abort cli_alert_warning cli_alert_info cli_alert_success cli_progress_done cli_progress_step cli_li qty
#' @export

wd_getdata_wwg <- function(stid, start_dt, end_dt, var, key, per = NULL, units = NULL, tz = Sys.timezone(),
                           cache_dir = NULL, session = NULL, spinner = FALSE, quiet = FALSE) {

  ## I decided against using the units package here, because although you can have
  ## a vector of units with different units, most people won't know how to deal with that.
  ## So instead the units are saved as a separate column (as a factor).

  # cli_alert_info("TODO: error check the station names are valid")

  ## Initialize an object for the result
  src_chr <- "wwg"
  src_name <- "Western Weather Group"
  wwg_baseurl <- "https://api.westernwx.com"
  network_chr <- "wwg"                         ## The WWG API only supports one network (WWG)

  wd_getdata_checks(start_dt, end_dt, tz, src = src_chr, units, var, cache_dir, session, spinner)

  if (is.null(per)) cli_abort(c(
    "{.var per} is required for Western Weather Group",
    "i" = "Try 60. If that fails, ask your Western Weather Group support person for the time intervals supported on your account"))

  use_cache <- !is.null(cache_dir)

  ## If quiet, suppress all cli output (except of course errors)
  ## rlang::local_options will reset the handler when the function terminates
  if (quiet) {
    rlang::local_options(cli.default_handler = function(...) invisible(NULL))
  }

  ## Suppress ANSI escape codes from cli output if we're not in an interactive session
  if (!interactive()) {rlang::local_options(cli.num_colors = 1L)}

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
    units_chr <- "default"
  } else {
    var_units_target_tbl <- src_var_filt_tbl |>
      left_join(units_conv_tbl |> select(units, units_target := !!units), by = "units") |>
      filter(units != units_target) |>
      select(var, units, units_target)
    units_chr <- units
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

    if (use_cache) {
      cache_base <- paste("wd_wwg", one_stid,
                          gsub(":", "", start_chr),
                          gsub(":", "", end_chr),
                          per,
                          paste0(var, collapse = "-"),
                          units_chr,
                          sep = "_")

      cache_fn <- file.path(cache_dir, paste0(cache_base, ".rds"))
      cache_found_yn <- file.exists(cache_fn)
    } else {
      cache_fn <- NA
      cache_found_yn <- FALSE
    }

    if (use_cache && cache_found_yn) {
      obs_thisstid_tbl <- readRDS(file = cache_fn)
      cli_alert_success("Found cached data for station {.field {one_stid}}")

    } else {

      if (!is.null(session) && spinner) {
        shinybusy::show_modal_spinner(
          spin = "radar",
          text = paste0("Getting data from ", src_name),
          session = session
        )
      }

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
      cli_progress_step("Calling {src_name} API for station {.field {one_stid}}")
      wwg_data_resp <- req_perform(wwg_data_req) |> try(silent = TRUE)

      ## Close the spinner
      if (!is.null(session) && spinner) {
        shinybusy::remove_modal_spinner(session = session)
      }

      ## Trap authorization errors
      if (inherits(wwg_data_resp, "try-error")) {
        err_msg <- attr(wwg_data_resp, "condition")$message
        if (grepl("unauthorized", err_msg, ignore.case = TRUE)) {
          cli_abort("Invalid API key passed to {src_name}")
        } else {
          cli_abort("Error from {src_name}: {err_msg}")
        }
      }

      ## Check that we have a valid response
      if (resp_is_error(wwg_data_resp)) {
        cli_abort("The API call to {src_name} returned an error")
      } else {
        cli_progress_done()
      }

      if (!resp_has_body(wwg_data_resp)) {
        cli_abort(c("No data returned from {src_name} for station {.field {one_stid}}"))
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

      if (use_cache) {
        saveRDS(obs_thisstid_tbl, file = cache_fn)
      }
    }

    ## Append these rows to res_tbl
    res_tbl <- rbind(res_tbl, obs_thisstid_tbl)

    ## Append one row of metadata for this station
    meta_tbl  <- rbind(meta_tbl, tibble(
      stid = one_stid,
      start_req_dt = start_dt,
      end_req_dt = end_dt,
      start_rec_dt = min(obs_thisstid_tbl$dt),
      end_rec_dt = max(obs_thisstid_tbl$dt),
      cache_copy = cache_found_yn
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

  ## Prepend class "wthr_tbl"
  class(res_tbl) <- c("wthr_tbl", class(res_tbl))

  ## Return a tibble with columns: src, network, stid, dt, var, val, units
  res_tbl

}


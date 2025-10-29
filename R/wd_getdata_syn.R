#' Get station data from Synoptic
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
#' @param spinner Show a spinner when fetching data,logical
#' @param quiet Suppress messages
#'
#' @details
#' This will query station data from the SynopticData API.
#' For documentation, see https://docs.synopticdata.com/services/
#'
#' To use the Synoptic API, you must create a Public Token. For details log-into your Synoptic account https://customer.synopticdata.com/customer/ and go to 'Create API keys and tokens'.
#'
#' To cross-check data availability, visit: https://availability.synopticdata.com/stations
#'
#' The Synoptic time series endpoint (https://docs.synopticdata.com/services/time-series) does not support temporal resampling,
#' so the `per` argument is ignored.
#'
#' @returns A weather data tibble (long format)
#'
#' @import httr2 tidyr dplyr tibble
#' @importFrom lubridate with_tz ymd_hms
#' @importFrom units set_units
#' @importFrom cli cli_abort cli_alert_warning cli_alert_info cli_alert_success cli_progress_done cli_progress_step cli_li
#' @importFrom purrr modify_if list_rbind
#' @importFrom rlang set_names
#' @export

wd_getdata_syn <- function(stid, start_dt, end_dt, var, key, per = NULL, units = NULL, tz = Sys.timezone(),
                           cache_dir = NULL, session = NULL, spinner = FALSE, quiet = FALSE) {

  # per is ignored with Synoptic (which doesn't support temporal resampling)
  # cli_alert_info("TODO: error check the station names are valid")

  if (!inherits(start_dt, "POSIXct")) cli_abort("{.var start_dt} must be a POSIXct object")
  if (!inherits(end_dt, "POSIXct")) cli_abort("{.var end_dt} must be a POSIXct object")
  if (start_dt >= end_dt) cli_abort("{.var end_dt} must be later than {.var start_dt}")
  if (!tz %in% OlsonNames()) cli_abort("{tz} is not a recognized timezone.")
  if (!is.null(units)) {
    if (!tolower(units) %in% c("imperial", "metric")) {
      cli_abort("{.var units} should be 'imperial' or 'metric'")
    }
  }
  if (FALSE %in% (var %in% vars_tbl$var)) {
    cli_abort("Unknown variable(s): {paste(var[!var %in% vars_tbl$var], collapse = '', '')}")
  }

  use_cache <- !is.null(cache_dir)
  if (use_cache) {
    if (!dir.exists(cache_dir)) cli_abort("Can't find {.var cache_dir}")
  }

  if (!is.null(session) && spinner) {
    if (!requireNamespace("shinybusy")) cli_abort("{.pkg shinybusy} is required to display a spinner")
  }

  if (quiet) {
    rlang::local_options(cli.default_handler = NULL)
    options(cli.default_handler = NULL)
  }

  if (!is.null(per)) cli_alert_warning("{.var per} is not supported for Synoptic. Ignoring.")

  ## Initialize an object for the result
  src_chr <- "syn"
  src_name <- "Synoptic"
  syn_baseurl <- "https://api.synopticdata.com"

  ## Construct a tibble that maps the standardized weather variable names to Synoptic API fields
  src_var_filt_tbl <- filter(src_var_tbl, var %in% .env$var, src == .env$src_chr)   ## not filtering on period

  if (FALSE %in% (var %in% src_var_filt_tbl$var)) {
    cli_abort("Cannot find an API field for
              {.var {paste(var[!var %in% src_var_filt_tbl$var], collapse = ', ')}}
              for this source and period.")
  }
  qry_flds <- src_var_filt_tbl$fld

  ## Convert start_dt to a character string in UTC time (Synoptic requirement)
  start_chr <- start_dt |>
    with_tz("UTC") |>
    format("%Y%m%d%H%M")

  end_chr <- end_dt |>
    with_tz("UTC") |>
    format("%Y%m%d%H%M")

  ## Get the Synoptic networks (n=375)
  syn_networks_tbl <- wd_getnetworks_syn(key, quiet = FALSE, cache = TRUE)

  if (use_cache) {
    cache_base <- paste("wd_syn",
                        paste0(stid, collapse="-"),
                        start_chr,
                        end_chr,
                        paste0(var, collapse = "-"),
                        "metric",
                        sep = "_")

    cache_fn <- file.path(cache_dir, paste0(cache_base, ".rds"))
    cache_found_yn <- file.exists(cache_fn)
  } else {
    cache_fn <- NA
    cache_found_yn <- FALSE
  }

  if (use_cache && cache_found_yn) {
    syn_data_lst <- readRDS(file = cache_fn)
    cli_alert_success("Found cached data for {stid}")

  } else {

    if (!is.null(session) && spinner) {
      shinybusy::show_modal_spinner(
        spin = "radar",
        text = paste0("Getting data from ", src_name),
        session = session
      )
    }

    ## Create a request for all the stations at once
    syn_data_req <- request(syn_baseurl) |>
      req_url_path_append("v2/stations/timeseries") |>
      req_url_query(token = key,
                    stid = paste0(stid, collapse=","),
                    vars = paste0(qry_flds, collapse = ","),
                    varsoperator = "and",
                    units = "metric",
                    start = start_chr,
                    end = end_chr,
                    obtimezone = 'local')

    ## Perform the request
    cli_progress_step("Downloading weather data from Synoptic")
    syn_data_resp <- req_perform(syn_data_req)

    if (!is.null(session) && spinner) {
      shinybusy::remove_modal_spinner(session = session)
    }

    ## Check that we have a valid response
    if (resp_is_error(syn_data_resp)) {
      cli_abort("The API request was not successful")
    } else {
      cli_progress_done()
    }

    # ## Convert the body to a list
    syn_data_lst <- resp_body_json(syn_data_resp)
    # View(syn_data_lst)

    ## Check that the station was found
    ## Better option: don't stop but record the error in the
    ## list that gets returned (error = TRUE, err_msg = "")
    ## TODO: improve the error message showing the name of the station(s) not found
    if (syn_data_lst$SUMMARY$NUMBER_OF_OBJECTS != length(stid)) {
      cli_abort(c("A station was not found"))
    }

    ## TODO: verify we got all the vars/fields we asked for

    if (use_cache) {
      saveRDS(syn_data_lst, file = cache_fn)
    }

  }

  ## Get the network for these stations
  stid_coords_network_tbl <- tibble(station = syn_data_lst$STATION) |>
    hoist(station, "STID", "MNET_ID", "LONGITUDE", "LATITUDE", "TIMEZONE") |>
    select(stid = STID, mnet_id = MNET_ID, lon = LONGITUDE,
           lat = LATITUDE, timezone = TIMEZONE) |>
    mutate(lon = as.numeric(lon), lat = as.numeric(lat)) |>
    left_join(syn_networks_tbl |> select(id, network = shortname),
              by = join_by(mnet_id == id))

  ## Impose a rule that the stations must be from the same network
  if (length(unique(stid_coords_network_tbl$mnet_id)) > 1) cli_abort("You can only query stations from one network.")
  network_chr <- unique(stid_coords_network_tbl$network)

  ## Create a tibble of the flds queries and the corresponding key
  ## from the nested list in OBSERVATIONS (e.g., air_temp_set_1)
  keys_chr <- sapply(syn_data_lst$STATION[[1]]$SENSOR_VARIABLES, names)
  fld_key_var_tbl <- tibble(fld = names(keys_chr),
                            key = as.character(keys_chr)) |>
    left_join(src_var_filt_tbl, by = "fld")

  ## Create a tibble of the variables that need unit conversion
  if (is.null(units)) {
    var_units_target_tbl <- tibble(NULL)
    units_chr <- "default"
  } else {
    var_units_target_tbl <- fld_key_var_tbl |>
      left_join(units_conv_tbl |> select(units, units_target := !!units), by = "units") |>
      filter(units != units_target) |>
      select(var, units, units_target)
    units_chr <- units
  }

  ## TODO: Compare the actual units with the expected units ??
  #  syn_data_lst$UNITS[names(keys_chr)]   "Milimeters"
  #  fld_key_var_tbl                       "mm"

  ## Create two utility lists to lookup var and units
  key_var_lst <- set_names(as.list(fld_key_var_tbl$var), fld_key_var_tbl$key)
  key_units_lst <- set_names(as.list(fld_key_var_tbl$units), fld_key_var_tbl$key)

  ## Initialize objects for the result
  res_tbl <- NULL
  meta_tbl <- NULL

  ## Loop through the stations in syn_data_lst
  # stn_idx <- 1
  for (stn_idx in 1:length(syn_data_lst$STATION)) {

    this_stid <- syn_data_lst$STATION[[stn_idx]]$STID

    ## Get the time zone
    syn_data_tz <- syn_data_lst$STATION[[stn_idx]]$TIMEZONE

    ## Create a vector of time stamps:
    syn_data_dt <- syn_data_lst$STATION[[stn_idx]]$OBSERVATIONS$date_time |>
      unlist() |>
      ymd_hms(tz = syn_data_tz, quiet = TRUE)

    obs_thisstid_lst <- lapply(fld_key_var_tbl$key, function(key)
      tibble(
        dt = syn_data_dt,
        var = key_var_lst[[key]],
        val = unlist(modify_if(syn_data_lst$STATION[[stn_idx]]$OBSERVATIONS[[key]], is.null, ~NA)),
        units = key_units_lst[[key]]
      ))

    obs_thisstid_tbl <- obs_thisstid_lst |>
      list_rbind() |>
      mutate(src = "syn",
             stid = syn_data_lst$STATION[[stn_idx]]$STID) |>
      left_join(select(stid_coords_network_tbl, stid, network), by = "stid") |>
      relocate(src, network, stid, dt, var, val, units)

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

    ## Append one row of metadata for this station
    meta_tbl  <- rbind(meta_tbl, tibble(
      stid = this_stid,
      start_req_dt = start_dt,
      end_req_dt = end_dt,
      start_rec_dt = min(syn_data_dt),        ## actual received
      end_rec_dt = max(syn_data_dt),
      cache_copy = cache_found_yn
    ))

    cli_alert_success("Parsed data for station {.field {this_stid}}")

  }  ## for stn_idx in 1:length(syn_data_lst$STATION)

  ## Convert character fields to factors
  res_tbl <- res_tbl |>
    mutate(across(c(src, network, stid, var, units), as.factor))

  ## Append elements of the data as a whole for the attributes
  attrb_lst <- list(
    src = src_chr,
    network = network_chr,
    stid = stid,
    var = var,
    per = NA,
    meta_tbl = meta_tbl
  )
  attributes(res_tbl) <- c(attributes(res_tbl), attrb_lst)

  ## Prepend class "wthr_df"
  class(res_tbl) <- c("wthr_tbl", class(res_tbl))

  ## Return a tibble with columns: src, network, stid, dt, var, val, units
  res_tbl

}

#' Get Synoptic networks
#' @param key API key
#' @param cache Cache results
#' @param quiet Suppress messages
#'
#' @import httr2 tidyr dplyr
#' @importFrom cli cli_alert_success
#' @export

wd_getnetworks_syn <- function(key, cache = TRUE, quiet = FALSE) {

  if (cache && !is.null(cache_env$syn_networks)) {
    if (!quiet) cli_alert_success("Using cached copy of Synoptic networks")
    cache_env$syn_networks

  } else {
    syn_baseurl <- "https://api.synopticdata.com"

    syn_networks_req <- request(syn_baseurl) |>
      req_url_path_append("v2/networks") |>
      req_url_query(token = key)

    syn_networks_lst <- syn_networks_req |>
      req_perform() |>
      resp_body_json()

    ## Additional variable available: PROGRAM, LAST_OBSERVATION, REPORTING_STATIONS, PERCENT_ACTIVE, PERCENTAGE_REPORTING, ACTIVE_RESTRICTED, TOTAL_RESTRICTED
    syn_networks_tbl <- tibble(netwrk = syn_networks_lst$MNET) |>
      hoist(netwrk,
            id = "ID",
            shortname = "SHORTNAME",
            longname = "LONGNAME",
            category = "CATEGORY",
            active_stations = "ACTIVE_STATIONS",
            total_statins = "TOTAL_STATIONS",
            period_checked = "PERIOD_CHECKED") |>
      select(-netwrk)

    if (cache) {
      cache_env$syn_networks <- syn_networks_tbl
    }

    if (!quiet) cli_alert_success("Downloaded networks from Synoptic")

    syn_networks_tbl

  }

}


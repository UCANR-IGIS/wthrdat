#' Transform a tibble of weather data from long to wide
#'
#' @param x A Weather Data tibble
#' @param use_units Return columns as units objects
#'
#' @details If \code{units = TRUE}, the columns in the tibble will be of class units from the units package.
#'
#' @returns A weather data tibble (wide format)
#'
#' @importFrom units set_units
#' @import dplyr tidyr
#' @export

wd_long2wide <- function(x, use_units = TRUE) {

  if (!inherits(x, "wthr_tbl")) stop("x must be a wthr_tbl object")

  if (!use_units) stop("For right now I am only supporting use_units")

  if (use_units) {
    ## Pivot wider - values only
    xwide_units_tbl <- x |>
      pivot_wider(id_cols = src:dt,
                  names_from = var,
                  values_from = val)

    ## Construct a tibble with the units for each column
    col_units_tbl <- x |>
      select(var, units) |>
      distinct() |>
      mutate(var = as.character(var), units = as.character(units))

    ## Loop thru the columns and convert them to units objects
    for (i in 1:nrow(col_units_tbl)) {
      xwide_units_tbl[ , col_units_tbl[i,"var", drop = TRUE]] <- set_units(xwide_units_tbl[[col_units_tbl[i,"var", drop = TRUE]]],
                                                                          col_units_tbl[i,"units", drop=TRUE],
                                                                          mode = "standard")
    }

    ## Copy over attributes
    attributes(xwide_units_tbl) <- c(attributes(xwide_units_tbl),
                                     attributes(x)[c("src", "network", "stid", "var", "per", "meta_tbl") ])

    class(xwide_units_tbl) <- c("wthr_tbl", class(xwide_units_tbl))

    ## Return
    xwide_units_tbl

  } else {

    xwide_twocols_tbl <- x |>
      pivot_wider(id_cols = src:dt,
                  names_from = var,
                  values_from = c(val, units),
                  names_glue = "{var}_{.value}") |>
      rename_with(.fn = ~ gsub("_val$", "", .x), .cols = ends_with("_val"))

    var_cols_sorted <- sort(names(xwide_twocols_tbl)[6:ncol(xwide_twocols_tbl)])

    xwide_twocols_tbl |> relocate(1:5, var_cols_sorted)

    message("TODO: COPY OVER ATTRIBUTES")

  }

}


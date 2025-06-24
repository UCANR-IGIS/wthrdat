## Code to prepare `var_map` dataset goes here
## OK to source()!

library(readxl)

wthrdat_vars_fn <- here::here("data-raw/wthrdat_vars.xlsx")
if (!file.exists(wthrdat_vars_fn)) stop(paste0("Can not find ", wthrdat_vars_fn))

vars_tbl <- read_xlsx(wthrdat_vars_fn, sheet = "vars")

src_var_tbl <- read_xlsx(wthrdat_vars_fn, sheet = "src_var",
                         col_types = c("text", "text", "numeric", "text", "text", "text"))

units_conv_tbl <- read_xlsx(wthrdat_vars_fn, sheet = "units_conv")

srcs_tbl <- read_xlsx(wthrdat_vars_fn, sheet = "srcs")

## Do some checks for duplicate combos
if (anyDuplicated(src_var_tbl[, c("src", "per", "var")])) stop("Found duplicate rows for src, per, and var")
if (anyDuplicated(src_var_tbl[, c("src", "per", "var", "units")])) stop("Found duplicate rows for src, per, var, and units")

if (anyDuplicated(src_var_tbl[, c("src", "per", "fld")])) stop("Found duplicate rows for src, per, and fld")
if (anyDuplicated(src_var_tbl[, c("src", "per", "fld", "units")])) stop("Found duplicate rows for src, per, fld, and units")

if (anyDuplicated(units_conv_tbl$units)) stop("Found duplicate units in units_conv")

cat("TODO: check that units are all imperial or all metric. Stop if not.")

## Check for valid units
# all_units <- unique(c(src_var_tbl$units_api, src_var_tbl$units_imperial, src_var_tbl$units_metric))
# valid_units_chr <- c("in", "mm", "degF", "degC")
all_units_chr <- unique(src_var_tbl$units)
valid_units_chr <- unique(c(units_conv_tbl$imperial, units_conv_tbl$metric))

if (FALSE %in% (all_units_chr %in% valid_units_chr)) {
  stop(paste0("Unknown unit(s): ",
             paste(all_units_chr[!all_units_chr %in% valid_units_chr], collapse = ", ")))
}

usethis::use_data(vars_tbl, src_var_tbl, units_conv_tbl, srcs_tbl, overwrite = TRUE, internal = TRUE)


# wthr_vars <- list(
#   pr = "precipitation",
#   eto = "evapotranspiration"
# )
# usethis::use_data(wthr_vars, overwrite = TRUE)
#
# var_map_lst <- list(
#   wwg = list(
#     eto_hr = "EToHr",
#     pr = "Precip"
#   ),
#   syn = list(
#     pr = "precip_accum_one_hour",
#     eto = "evapotranspiration"
#   )
# )
# usethis::use_data(var_map_lst, overwrite = TRUE)


## This is massive overkill. It also omits the prefixes (e.g., mm & cm do not appear)
# valid_units_chr <- units::valid_udunits(quiet = FALSE) |>
#   dplyr::select(symbol, symbol_aliases, name_singular, name_singular_aliases, name_plural, name_plural_aliases) |>
#   as.matrix() |>
#   as.character() |>
#   stringi::stri_remove_empty() |>
#   stringr::str_split(pattern = fixed(",")) |>
#   unlist() |>
#   trimws() |>
#   unique() |>
#   sort()


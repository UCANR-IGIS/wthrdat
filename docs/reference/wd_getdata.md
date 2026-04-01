# Download weather station data

Download weather station data

## Usage

``` r
wd_getdata(
  src = c("syn", "wwg")[1],
  stid,
  start_dt,
  end_dt,
  var,
  key,
  per = NULL,
  units = NULL,
  tz = Sys.timezone(),
  cache_dir = NULL,
  session = NULL,
  spinner = FALSE,
  quiet = FALSE
)
```

## Arguments

- src:

  Weather data source / provider

- stid:

  Station name(s)

- start_dt:

  Start date-time

- end_dt:

  End date-time

- var:

  Standardized weather variables

- key:

  API key

- per:

  Period / interval (minutes)

- units:

  Units desired (imperial or metric)

- tz:

  Time Zone for the results

- cache_dir:

  A local directory for caching the results (optional)

- session:

  Shiny session (for showing a spinner)

- spinner:

  Show a spinner when fetching data,logical

- quiet:

  Suppress messages

## Value

A weather data tibble (long format)

## Details

This will query station data from supported networks.

If you pass a value for `cache_dir`, downloaded data will be saved in
that location. The function however does not clear the `cache_dir` upon
closing, so it is recommended you use a temporary directory.

## See also

[`wd_getdata_syn`](wd_getdata_syn.md),
[`wd_getdata_wwg`](wd_getdata_wwg.md)

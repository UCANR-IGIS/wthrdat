# Get station data from the Western Weather Group API

Get station data from the Western Weather Group API

## Usage

``` r
wd_getdata_wwg(
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

  Directory for caching

- session:

  Shiny session (for showing a spinner)

- spinner:

  Show a spinner when fetching data, logical

- quiet:

  Suppress messages

## Value

A weather data tibble (long format)

## Details

This will query station data from the Weather Weather Group API. For the
full documentation, see https://api.westernwx.com/docs/

To use the WWG API, you must have an API key. If your account has API
access, you can manage your API Keys from
https://app.westernwx.com/apikeys. When you generate a new key, it will
give you an Id, Secret, and a Key.

If you pass a value for `cache_dir`, downloaded data will be saved in
that location. The function however does not clear the `cache_dir` upon
closing, so it is recommended you use a temporary directory.

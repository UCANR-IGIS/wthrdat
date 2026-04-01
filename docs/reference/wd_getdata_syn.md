# Get station data from Synoptic

Get station data from Synoptic

## Usage

``` r
wd_getdata_syn(
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

  Show a shinybusy spinner when fetching data, logical

- quiet:

  Suppress messages

## Value

A weather data tibble (long format)

## Details

This will query station data from the SynopticData API. For
documentation, see https://docs.synopticdata.com/services/

To use the Synoptic API, you must create a Public Token. For details
log-into your Synoptic account
https://customer.synopticdata.com/customer/ and go to 'Create API keys
and tokens'.

To cross-check data availability, visit:
https://availability.synopticdata.com/stations

The Synoptic time series endpoint
(https://docs.synopticdata.com/services/time-series) does not support
temporal resampling, so the `per` argument is ignored.

If you pass a value for `cache_dir`, downloaded data will be saved in
that location. The function however does not clear the `cache_dir` upon
closing, so it is recommended you use a temporary directory.

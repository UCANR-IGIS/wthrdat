# Parameter checks

Parameter checks

## Usage

``` r
wd_getdata_checks(
  start_dt,
  end_dt,
  tz,
  src,
  units,
  var,
  cache_dir,
  session,
  spinner
)
```

## Arguments

- start_dt:

  Start date-time

- end_dt:

  End date-time

- tz:

  Time Zone for the results

- src:

  Weather data source / provider

- units:

  Units desired (imperial or metric)

- var:

  Standardized weather variables

- cache_dir:

  Directory for caching

- session:

  Shiny session (for showing a spinner)

- spinner:

  Show a spinner when fetching data,logical

## Value

TRUE if all checks are passed

## Details

This internal function checks the parameters passed to wd_getdata() and
wd_getdata_xxx()

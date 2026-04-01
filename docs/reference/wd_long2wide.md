# Transform a tibble of weather data from long to wide

Transform a tibble of weather data from long to wide

## Usage

``` r
wd_long2wide(x, use_units = TRUE)
```

## Arguments

- x:

  A Weather Data tibble

- use_units:

  Return columns as units objects

## Value

A weather data tibble (wide format)

## Details

If `units = TRUE`, the columns in the tibble will be of class units from
the units package.


<!-- README.md is generated from README.Rmd. Please edit that file -->

# wthrdat

<!-- badges: start -->

<!-- badges: end -->

The goal of `wthrdat` is to provide a standardized API to download data
from multiple weather station providers. `wthrdat` has specifically been
written to support agricultural decision support tools and research that
requires real-time weather data from multiple networks.

In its current state, `wthrdat` supports downloading time series data
for specific stations for a pair of date-times. Weather APIs can do a
lot more than that, such as help you discover new stations or get info
about the health of a station, but those functions are not supported. To
use `wthrdat`, you need to know which stations you want, and have access
to them via an API key (see also ‘Other Ways to Download Weather Data
into R’ below).

## Installation

You can install the development version of `wthrdat` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

Below we get weather data from [Synoptic](https://synopticdata.com/) for
a CIMIS station in Ventura County, CA.

Start by defining the parameters for the request:

``` r
library(wthrdat)
#> wthrdat (version 0.0.0.9000)
#> Bug reports: NA
weather_vars <- c("pr", "eto", "tair")
stn_id <- "CI152"
start_dt <- lubridate::ymd_hm("2025-04-01 00:00", tz = "America/Los_Angeles")
end_dt <- lubridate::ymd_hm("2025-04-30 23:59", tz = "America/Los_Angeles")
syn_key <- Sys.getenv("MY_SYNOPTIC_PUBLIC_TOKEN")
```

  

Next we call `wd_getdata_syn()` which returns data in a long format:

``` r
syn_key <- Sys.getenv("MY_SYNOPTIC_PUBLIC_TOKEN")
syn_key
#> [1] "91b8e95d3af4443aa981b43d25be7e06"

camarillo_tbl <- wd_getdata(src = "syn",
                            stid = stn_id,
                            start_dt = start_dt,
                            end_dt = end_dt,
                            var = weather_vars,
                            key = syn_key,
                            units = "imperial")
#> ✔ Downloaded networks from Synoptic
#> ℹ Downloading weather data from Synoptic✔ Downloading weather data from Synoptic [78ms]
#> ✔ Parsed data for station CI152
dim(camarillo_tbl)
#> [1] 2151    7
head(camarillo_tbl)
#> # A tibble: 6 × 7
#>   src   network stid  dt                  var     val units
#>   <fct> <fct>   <fct> <dttm>              <fct> <dbl> <fct>
#> 1 syn   CIMIS   CI152 2025-04-01 00:00:00 tair   53.7 degF 
#> 2 syn   CIMIS   CI152 2025-04-01 01:00:00 tair   53.8 degF 
#> 3 syn   CIMIS   CI152 2025-04-01 02:00:00 tair   53.6 degF 
#> 4 syn   CIMIS   CI152 2025-04-01 03:00:00 tair   53.3 degF 
#> 5 syn   CIMIS   CI152 2025-04-01 04:00:00 tair   51.6 degF 
#> 6 syn   CIMIS   CI152 2025-04-01 05:00:00 tair   47.3 degF
```

To work with the variables in individual columns, we need to transform
it to a wide format:

``` r
camarillo_wide_tbl <- wd_long2wide(camarillo_tbl, use_units = TRUE)
head(camarillo_wide_tbl)
#> # A tibble: 6 × 7
#>   src   network stid  dt                    tair   pr  eto
#>   <fct> <fct>   <fct> <dttm>              [degF] [in] [in]
#> 1 syn   CIMIS   CI152 2025-04-01 00:00:00   53.7    0    0
#> 2 syn   CIMIS   CI152 2025-04-01 01:00:00   53.8    0    0
#> 3 syn   CIMIS   CI152 2025-04-01 02:00:00   53.6    0    0
#> 4 syn   CIMIS   CI152 2025-04-01 03:00:00   53.3    0    0
#> 5 syn   CIMIS   CI152 2025-04-01 04:00:00   51.6    0    0
#> 6 syn   CIMIS   CI152 2025-04-01 05:00:00   47.3    0    0
```

## Some Notes on Querying Data

Functions you can use to download weather data include:

    wd_getdata_syn()    ## Synoptic
    wd_getdata_wwg()    ## Western Weather Group

In the function document, please note that *source* refers to the API,
while *network* refers to the weather station network. In many cases
these will be the same as the source (e.g., the Western Weather Group
API allows you to download data from stations in the Western Weather
Group network). However other sources, like Synoptic, do not operate
their own network of weather stations but are rather a data portal for
multiple networks.

To view a list of the standardized weather variables (that you can pass
to `wd_getdata_syn()` and `wd_getdata_wwg()`), run:

``` r
wd_vars()
#> # A tibble: 5 × 2
#>   var      desc                              
#>   <chr>    <chr>                             
#> 1 eto      reference evapotransiration       
#> 2 pr       precipitation                     
#> 3 tair     air temperature at 2m             
#> 4 tair_max max air temperature for the period
#> 5 tair_min min air temperature for the period
```

Note that not every network or station will have every variable. To see
which variable is available for a specific source, run:

``` r
## Coming soon
```

If a weather station you have access to includes a weather variable that
is not supported in `wthrdat`, you unfortunately just can’t add it
yourself. Contact the package maintainer or start an issue on GitHub and
we can try to add it.

## Units

You can ask for imperial (English) or metric units when you download
data using the `units` argument in `wd_getdata()`. The units for each
value are given in a separate column.

`wd_long2wide()` will make each variable into its own column, formatted
as a vector from the units package. This makes converting to other units
really easy (show example).

    ## Example of converting to other units

You can also convert the columns to regular numbers as follows:

    ## Example of converting units to numbers

## Temporal Resampling

Some sources (but not all) provide an option to request data at
different periods of aggregation (i.e., hourly vs daily). When
available, you can specify the period of aggregation using the `per`
argument (period in minutes). See the help page for the download
function for the source for details on which values for `per` are
supported.

You can of course also temporally resample observations in R once you
get the time series (fairly easily).

## Finding Stations

When requesting data from a source, you have to specify one or more
stations by name (using the `stid` argument). `wthrdat` does not
currently have any functions to help you find the names of stations, or
where they’re located. If it presumed that if you have access to the API
you have access to dashboards and maps from the provide that show the
stations available.

## Managing API Keys

Most / all of the weather APIs require an API key to fetch data. Whether
they are free or not, you don’t want to accidentally give-away your API
key. The recommended practice is to store them as environment variables
in your `.Renviron` file. You can edit your `.Renviron` in a text editor
or with:

    usethis::edit_r_environ()

TIP: The `.Renviron` file is usually located in your R home directory.
It is worth verifying this and making sure this file isn’t accidentally
backed up on GitHub.

Enter your API keys in `.Renviron` as follows:

    SYNOPTIC_KEY = "a2cXXXxXXXXXXXXXXXXXXXXXcXXXXde8"

After restarting R, you can bring it into your code as follow:

``` r
my_api_key <- Sys.getenv("SYNOPTIC_KEY")
```

Tip: If you save your API keys as environment variables as shown above,
and are building a Shiny app to host on ShinyApps.io, be sure to include
the ‘environment variables’ box when you publish the app.

## Other Ways to Get Weather Data in R

If you don’t need import the latest weather data into R in real-time via
an API, you can always export weather values as a CSV file and import it
with `readr`.

There are other R packages that are designed to import weather data from
specific providers via APIs. One way to find packages is to look at CRAN
packages by name and search for the word ‘weather’ or the network you’re
interested in.

See also openmeteo.

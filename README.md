
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wthrdat

<!-- badges: start -->

<!-- badges: end -->

The goal of `wthrdat` is to provide a standardized API to download data
from multiple weather station providers. `wthrdat` has specifically been
written to support agricultural decision support tools and research that
requires real-time weather data from multiple networks.

`wthrdat` supports downloading time series data for specific stations
for a pair of date-times. `wthrdat` currently has support for the
[Synoptic](https://synopticdata.com/) and [Western Weather
Group](https://www.westernweathergroup.com/) APIs. Both of these APIs
require a key. Only a subset of weather variables are currently
supported (precipitation, reference evapotranspiration, and air
temperature.

To use `wthrdat`, you need to know which stations you want, and have
access to them via an API key. For sources of ‘free’ weather data, see
‘Other Ways to Download Weather Data into R’ below.

Weather APIs can do a lot more than download station data. For example
many weather API can help you discover new stations or get info about
the health of a station. These kinds of functions are not supported.

## Installation

You can install the development version of `wthrdat` with:

    remotes::install_github("ucanr-igis/wthrdat")

View the weather providers supported:

``` r
library(wthrdat)
#> wthrdat (version 0.1.9)
#> Bug reports: https://github.com/ucanr-igis/wthrdat/issues/
wd_srcs()
#> # A tibble: 2 × 3
#>   src   name                  docs                                   
#>   <chr> <chr>                 <chr>                                  
#> 1 wwg   Western Weather Group https://api.westernwx.com/docs/        
#> 2 syn   Synoptic              https://docs.synopticdata.com/services/
```

View the subset of standardized weather variables currently supported:

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

## Example

Below we get weather data from [Synoptic](https://synopticdata.com/) for
a CIMIS station in Ventura County, CA.

Start by defining the parameters for the request:

``` r
weather_vars <- c("pr", "eto", "tair")
stn_id <- "CI152"
start_dt <- lubridate::ymd_hm("2025-04-01 00:00", tz = "America/Los_Angeles")
end_dt <- lubridate::ymd_hm("2025-04-30 23:59", tz = "America/Los_Angeles")
```

  

Next we call `wd_getdata_syn()` which returns data in a long format:

``` r
## Retrieve my API key (saved as an environment variable - see below)
syn_key <- Sys.getenv("MY_SYNOPTIC_PUBLIC_TOKEN")

camarillo_tbl <- wd_getdata(src = "syn",
                            stid = stn_id,
                            start_dt = start_dt,
                            end_dt = end_dt,
                            var = weather_vars,
                            key = syn_key,
                            units = "imperial")
#> THIS SESSION IS INTERACTIVE: FALSE
#> SESSION IS NULL: TRUE
#> SETTING COLORS TO 1
#> ✔ Downloaded networks from Synoptic
#> ℹ Calling Synoptic API✔ Calling Synoptic API [85ms]
#> ✔ Parsed data for station CI152
dim(camarillo_tbl)
#> [1] 2106    7
head(camarillo_tbl)
#> # A tibble: 6 × 7
#>   src   network stid  dt                  var     val units
#>   <fct> <fct>   <fct> <dttm>              <fct> <dbl> <fct>
#> 1 syn   CIMIS   CI152 2025-04-01 15:00:00 tair   61.5 degF 
#> 2 syn   CIMIS   CI152 2025-04-01 16:00:00 tair   60.2 degF 
#> 3 syn   CIMIS   CI152 2025-04-01 17:00:00 tair   58.3 degF 
#> 4 syn   CIMIS   CI152 2025-04-01 18:00:00 tair   57.1 degF 
#> 5 syn   CIMIS   CI152 2025-04-01 19:00:00 tair   56.2 degF 
#> 6 syn   CIMIS   CI152 2025-04-01 20:00:00 tair   55.4 degF
```

To work with the variables in individual columns, we need to transform
it to a wide format:

``` r
camarillo_wide_tbl <- wd_long2wide(camarillo_tbl, use_units = TRUE)
head(camarillo_wide_tbl)
#> # A tibble: 6 × 7
#>   src   network stid  dt                    tair   pr  eto
#>   <fct> <fct>   <fct> <dttm>              [degF] [in] [in]
#> 1 syn   CIMIS   CI152 2025-04-01 15:00:00   61.5    0 0.02
#> 2 syn   CIMIS   CI152 2025-04-01 16:00:00   60.2    0 0.01
#> 3 syn   CIMIS   CI152 2025-04-01 17:00:00   58.3    0 0   
#> 4 syn   CIMIS   CI152 2025-04-01 18:00:00   57.1    0 0   
#> 5 syn   CIMIS   CI152 2025-04-01 19:00:00   56.2    0 0   
#> 6 syn   CIMIS   CI152 2025-04-01 20:00:00   55.4    0 0
```

## Some Notes on Querying Data

Functions you can use to download weather data include:

    wd_getdata()        ## Any supported API
    wd_getdata_syn()    ## Synoptic 
    wd_getdata_wwg()    ## Western Weather Group

When calling a download function, please note that *source* refers to
the API, while *network* refers to the weather station network. In many
cases these will be the same as the source (e.g., the Western Weather
Group API allows you to download data from stations in the Western
Weather Group network). However other sources, like Synoptic, do not
operate their own network of weather stations but are rather a data
portal for multiple networks.

To view a list of the standardized weather variables (that you can pass
to `wd_getdata()`), run:

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
wd_srcs()
#> # A tibble: 2 × 3
#>   src   name                  docs                                   
#>   <chr> <chr>                 <chr>                                  
#> 1 wwg   Western Weather Group https://api.westernwx.com/docs/        
#> 2 syn   Synoptic              https://docs.synopticdata.com/services/
## sources-vars coming soon...
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

    my_api_key <- Sys.getenv("SYNOPTIC_KEY")

Tip: If you save your API keys as environment variables as shown above,
and are publishing your Shiny app on ShinyApps.io, be sure to include
the ‘environment variables’ box when you publish the app.

## Trapping Errors

There are a dozen ways that an API call can fail. The API may be
offline, a station may be offline, the API key might be expired,
expected parameter values may have changed, etc.

When an error occurs in `wd_getdata()` (and its dependent functions), it
checks for the most common causes and tries to provide a helpful error
message. If you’re working at the console or in a notebook, these error
message will hopefully help you understand and come up with a way to fix
the error.

If on the other you’re calling `wd_getdata()` from a Shiny app (one of
the main use cases for the package), you probably want to check for an
error response so you can respond to it without having the app simply
disconnect from the server. You can trap errors and present the error
message to the user in your Shiny app using the `try()` function as
illustrated below:

    data_tbl <- wd_getdata(src = "syn",
                     stid = c("CI052", "CI152"),
                     start_dt = as.POSIXct("2026-03-18 13:00:00 PDT"),
                     end_dt = as.POSIXct("2026-03-30 13:00:00 PDT"),
                     var = c("eto", "pr"),
                     key = "abcdefg",
                     units = "imperial",
                     tz = "America/Los_Angeles") |> try(silent = TRUE)

    if (inherits(data_tbl, "try-error")) {
      cat("An error occurred: ", attr(data_tbl, "condition")$message, "\n", sep = "")
    } else {
      head(data_tbl)
    }

## Other Ways to Get Weather Data in R

If you don’t need import the latest weather data into R in real-time via
an API, you can always export weather values as a CSV file and import it
with `readr`.

There are other R packages that are designed to import weather data from
specific providers via APIs. One way to find packages is to look at CRAN
packages by name and search for the word ‘weather’ or the network you’re
interested in.

See also [`openmeteo`](https://cran.r-project.org/package=openmeteo).

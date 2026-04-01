# View supported Weather Data Sources / APIs

View supported Weather Data Sources / APIs

## Usage

``` r
wd_srcs()
```

## Value

A tibble of weather data sources, including a link to their API
documentation

## Examples

``` r
wd_srcs()
#> # A tibble: 2 × 3
#>   src   name                  docs                                   
#>   <chr> <chr>                 <chr>                                  
#> 1 wwg   Western Weather Group https://api.westernwx.com/docs/        
#> 2 syn   Synoptic              https://docs.synopticdata.com/services/
```

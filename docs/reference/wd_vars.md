# View standard weather variables

View standard weather variables

## Usage

``` r
wd_vars()
```

## Value

A tibble of standard weather variables you can use in API calls. Not
every API may support every variable.

## Examples

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

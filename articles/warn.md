# Silence warnings

## The `GDPuc.warn` option

You can silence all warnings thrown by `convertGDP` by setting the
option `GDPuc.warn = FALSE`. This will affect all calls to `convertGDP`
in the active R-session.

``` r
options(GDPuc.warn = FALSE)
```

## The `replace_NAs` argument

One warning thrown by `convertGDP` comes from the addition of NAs in the
return object due to missing conversion factors. If NAs are explicitly
desired set `replace_NAs = NA`. This will silence the warning.

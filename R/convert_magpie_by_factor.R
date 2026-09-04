# Should a magpie object be converted through its conversion factors?
#
# Only worth it when the object holds more than one value per country and year, i.e. when it has a
# second spatial sub-dimension or any data dimensions. With one value per country and year there is
# nothing to collapse, and the generic code path is used, which keeps its results bit-identical.
#
# Regional data is excluded because with_regions disaggregates regions into countries by weighing them
# with their GDP share, which changes the values and the number of rows, and re-aggregates afterwards.
# return_cfs is excluded because it re-runs the conversion anyway.
use_factors_for_magpie <- function(gdp, with_regions, return_cfs) {
  if (!inherits(gdp, "magpie") || !is.null(with_regions) || return_cfs) {
    return(FALSE)
  }
  if (!rlang::is_installed("magclass")) {
    return(FALSE)
  }
  if (any(dim(gdp) == 0)) {
    return(FALSE)
  }
  nFactors <- length(unique(magclass::getItems(gdp, dim = 1.1))) * max(1L, length(magclass::getYears(gdp)))
  length(gdp) > nFactors
}


# Convert a magpie object by scaling it with its conversion factors
#
# Every elemental conversion step multiplies or divides the value column by a factor that depends only
# on the country and the year, so the conversion is linear in the data. The generic code path however
# melts the object into a long data frame which unnecessarily blows up magpie objects.
#
# So instead the conversion is run on an object holding a single 1 per country and year, which yields
# the conversion factors.
#
# A factor can be infinite (e.g. no deflator that far back), which turns a 0 in gdp into NaN even though
# the factor itself is known, not missing. So NA handling still has to happen out here after the
# multiplication, mirroring convertGDP()'s own handling of the NAs its elemental conversions produce.
convert_magpie_by_factor <- function(gdp,
                                     unit_in,
                                     unit_out,
                                     source,
                                     use_USA_cf_for_all,
                                     replace_NAs,
                                     verbose,
                                     iso3c_column,
                                     year_column) {
  iso3c <- unique(magclass::getItems(gdp, dim = 1.1))

  cf <- magclass::new.magpie(cells_and_regions = iso3c, years = magclass::getYears(gdp), fill = 1)
  magclass::getSets(cf) <- c("iso3c", "year", "data")

  # NA is passed instead of NULL so that the inner call doesn't warn about missing conversion factors
  # itself: the warning is raised once below, on the actual result, after the multiplication.
  cf <- convertGDP(gdp = cf,
                   unit_in = unit_in,
                   unit_out = unit_out,
                   source = source,
                   use_USA_cf_for_all = use_USA_cf_for_all,
                   with_regions = NULL,
                   replace_NAs = if (is.null(replace_NAs)) NA else replace_NAs,
                   verbose = verbose,
                   return_cfs = FALSE,
                   iso3c_column = iso3c_column,
                   year_column = year_column)

  # Line the factors up with the spatial dimension of gdp by country code. The
  # expanded factors stay one value wide in the data dimension.
  cf <- cf[match(magclass::getItems(gdp, dim = 1.1, full = TRUE), magclass::getItems(cf, dim = 1)), , ]
  magclass::getItems(cf, dim = 1, raw = TRUE) <- magclass::getItems(gdp, dim = 1)
  cf <- magclass::collapseDim(cf, dim = 3)

  x <- gdp * cf

  # Handle NAs the multiplication generated, mirroring convertGDP()'s own handling (convertGDP.R).
  if (!is.null(replace_NAs) && 0 %in% replace_NAs) x[is.na(x)] <- 0
  if (any(is.na(x) & !is.na(gdp))) {
    if (!is.null(replace_NAs)) {
      if ("no_conversion" %in% replace_NAs) x[is.na(x)] <- gdp[is.na(x)]
    } else {
      warn("NAs have been generated for countries lacking conversion factors!")
    }
  }

  magclass::getSets(x) <- magclass::getSets(gdp)
  x
}

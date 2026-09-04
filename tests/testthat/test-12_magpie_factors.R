skip_if_not_installed("magclass")

# A magpie object with a single value per country and year has nothing to collapse and takes the
# generic code path. Anything wider -- several data dimensions, or a second spatial sub-dimension --
# is converted by scaling it with its conversion factors instead.
test_that("the factor path is used exactly for magpie objects wider than one value per country-year", {
  flat <- magclass::new.magpie(c("USA", "FRA"), 2010:2012, "i1", fill = 1)
  items <- magclass::new.magpie(c("USA", "FRA"), 2010:2012, c("i1", "i2"), fill = 1)
  bilat <- magclass::new.magpie(c("USA.FRA", "USA.USA", "FRA.USA"), 2010:2012, "i1", fill = 1)
  noyear <- magclass::new.magpie(c("USA", "FRA"), NULL, c("i1", "i2"), fill = 1)

  expect_false(use_factors_for_magpie(flat, NULL, FALSE))
  expect_true(use_factors_for_magpie(items, NULL, FALSE))
  expect_true(use_factors_for_magpie(bilat, NULL, FALSE))
  expect_true(use_factors_for_magpie(noyear, NULL, FALSE))

  # not for data frames, regional data, or when the conversion factors are requested
  expect_false(use_factors_for_magpie(tibble::tibble(iso3c = "USA", year = 2010, value = 1), NULL, FALSE))
  expect_false(use_factors_for_magpie(items, tibble::tibble(iso3c = "USA", region = "NAM"), FALSE))
  expect_false(use_factors_for_magpie(items, NULL, TRUE))
})


test_that("converting through factors agrees with converting value by value", {
  val <- 3.5
  flat <- magclass::new.magpie(c("USA", "FRA"), 2010:2012, "i1", fill = val)
  items <- magclass::new.magpie(c("USA", "FRA"), 2010:2012, c("i1", "i2"), fill = val)
  bilat <- magclass::new.magpie(c("USA.FRA", "USA.USA", "FRA.USA"), 2010:2012, "i1", fill = val)

  ref <- convertGDP(flat, "current US$MER", "constant 2017 US$MER")
  wide <- convertGDP(items, "current US$MER", "constant 2017 US$MER")
  bi <- convertGDP(bilat, "current US$MER", "constant 2017 US$MER")

  # each item of the wide object gets the conversion of its country and year
  expect_equal(as.vector(wide[, , "i1"]), as.vector(ref))
  expect_equal(as.vector(wide[, , "i2"]), as.vector(ref))
  # a bilateral cell is converted with the factors of its first (reporting) country
  expect_equal(as.vector(bi["USA.FRA", , ]), as.vector(ref["USA", , ]))
  expect_equal(as.vector(bi["FRA.USA", , ]), as.vector(ref["FRA", , ]))

  # shape and metadata are untouched
  expect_identical(dimnames(wide), dimnames(items))
  expect_identical(magclass::getSets(wide), magclass::getSets(items))
})


test_that("NA handling through factors matches the generic path", {
  # TWN has no conversion factors in wb_wdi, and one value is NA to begin with
  x <- magclass::new.magpie(c("USA", "TWN"), 2010:2012, c("i1", "i2"), fill = 2)
  x["USA", 2011, "i1"] <- NA
  flat <- x[, , "i1"]

  for (rna in list(NA, 0, "no_conversion")) {
    wide <- suppressWarnings(convertGDP(x, "current US$MER", "constant 2017 US$MER", replace_NAs = rna))
    ref <- suppressWarnings(convertGDP(flat, "current US$MER", "constant 2017 US$MER", replace_NAs = rna))
    expect_equal(as.vector(wide[, , "i1"]), as.vector(ref))
  }

  # without replace_NAs, missing conversion factors are still reported
  expect_warning(convertGDP(x, "current US$MER", "constant 2017 US$MER"),
                 "NAs have been generated for countries lacking conversion factors!")
})


test_that("a 0 value against an infinite conversion factor is handled like the generic path", {
  # BLR has no CPI data in 1990, so its constant-to-current factor is infinite there, turning a 0 value
  # into NaN even though the factor itself is not missing -- distinct from the "no factor at all" case
  # covered above.
  zero <- magclass::new.magpie("BLR", c(1990, 2020), c("i1", "i2"), fill = 0)

  for (rna in list(NULL, NA, 0, "no_conversion", c("linear", "no_conversion"))) {
    label <- if (is.null(rna)) "NULL" else paste(rna, collapse = "+")
    fast <- suppressWarnings(convertGDP(zero, "constant 2017 US$MER", "current US$MER", replace_NAs = rna))
    slow <- suppressWarnings(convertGDP(as_long(zero), "constant 2017 US$MER", "current US$MER", replace_NAs = rna))
    expect_equal(as.vector(fast), slow$value, label = label)
  }

  # replace_NAs = "no_conversion" restores the original (0) value, rather than leaving NaN
  restored <- suppressWarnings(convertGDP(zero, "constant 2017 US$MER", "current US$MER",
                                          replace_NAs = "no_conversion"))
  expect_equal(as.vector(restored), rep(0, length(zero)))

  # without replace_NAs, the fast path warns exactly like the generic one
  expect_warning(convertGDP(zero, "constant 2017 US$MER", "current US$MER"),
                 "NAs have been generated for countries lacking conversion factors!")
})


test_that("no warning is raised when the only factor-less country has no non-NA data", {
  # TWN has no conversion factors, but every one of its values is already NA, so neither path should
  # warn about NAs being generated for it.
  x <- magclass::new.magpie(c("USA", "TWN"), 2010:2012, c("i1", "i2"), fill = 2)
  x["TWN", , ] <- NA

  expect_no_warning(convertGDP(x, "current US$MER", "constant 2017 US$MER"))
})


test_that("one and the same object converted both ways gives the same values", {
  gdp <- magclass::new.magpie(c("USA.FRA", "FRA.USA", "USA.USA"), 2010:2013, c("i1", "i2"), fill = 0)
  gdp[, , ] <- seq_len(length(gdp)) * 1.5

  expect_true(use_factors_for_magpie(gdp, NULL, FALSE))
  expect_matches_long_form(gdp, unit_in = "current US$MER", unit_out = "constant 2017 US$MER")
})


# The factors are lined up with the data by country code and the multiplication then aligns on
# dimnames, so an object whose countries, years and items are in no particular order, and which has
# more than one sub-dimension on either side, has to come back in exactly the shape it went in.
test_that("scrambled and multi-sub-dimensional objects keep their layout", {
  gdp <- magclass::new.magpie(c("ZWE.AFG", "AFG.DEU", "DEU.ZWE", "DEU.AFG"),
                              c(2015, 2001, 2008),
                              c("i2.eA", "i1.eB"),
                              fill = 0)
  gdp[, , ] <- seq_len(length(gdp)) * 0.75
  magclass::getSets(gdp) <- c("ISO", "Partner", "Year", "Item", "Element")

  out <- expect_matches_long_form(gdp,
                                  unit_in = "current US$MER",
                                  unit_out = "constant 2017 US$MER",
                                  replace_NAs = "no_conversion")

  expect_identical(dimnames(out), dimnames(gdp))
  expect_identical(magclass::getSets(out), magclass::getSets(gdp))
})


test_that("the factor path agrees with the generic path across unit combinations", {
  units <- c("current LCU", "current US$MER", "current Int$PPP",
             "constant 2010 LCU", "constant 2010 US$MER", "constant 2010 Int$PPP",
             "constant 2010 EUR", "constant 2010 JPN_CU")

  gdp <- magclass::new.magpie(c("USA.FRA", "FRA.USA", "USA.USA"), 2010:2012, c("i1", "i2"), fill = 0)
  gdp[, , ] <- seq_len(length(gdp)) * 1.5

  tried <- 0L
  for (unitIn in units) {
    for (unitOut in units) {
      if (identical(unitIn, unitOut)) next
      label <- paste(unitIn, "->", unitOut)
      tried <- tried + 1L

      slow <- tryCatch(suppressWarnings(convertGDP(as_long(gdp), unitIn, unitOut)),
                       error = function(e) conditionMessage(e))
      fast <- tryCatch(suppressWarnings(convertGDP(gdp, unitIn, unitOut)),
                       error = function(e) conditionMessage(e))

      if (is.character(slow)) {
        # a combination the package rejects has to be rejected identically on both paths
        expect_identical(fast, slow, label = label)
      } else {
        expect_false(is.character(fast), label = label)
        if (!is.character(fast)) {
          joinBy <- setdiff(names(slow), "value")
          cmp <- dplyr::inner_join(dplyr::rename(as_long(fast), "fastValue" = "value"), slow, by = joinBy)
          expect_equal(nrow(cmp), length(gdp), label = label)
          expect_equal(cmp$fastValue, cmp$value, label = label)
        }
      }
    }
  }
  expect_gt(tried, 40)
})


test_that("the factor path passes the remaining arguments through unchanged", {
  gdp <- magclass::new.magpie(c("USA.FRA", "FRA.USA", "USA.USA"), 2010:2012, c("i1", "i2"), fill = 0)
  gdp[, , ] <- seq_len(length(gdp)) * 1.5

  # a source passed in as a data frame rather than by name
  mySource <- dplyr::filter(wb_wdi, .data$iso3c %in% c("USA", "FRA"))
  expect_matches_long_form(gdp, unit_in = "current US$MER", unit_out = "constant 2017 US$MER",
                           source = mySource, label = "custom source")

  expect_matches_long_form(gdp, unit_in = "current US$MER", unit_out = "constant 2017 US$MER",
                           use_USA_cf_for_all = TRUE, label = "use_USA_cf_for_all")

  # constant to constant needs no year column, but the object still carries years
  expect_matches_long_form(gdp, unit_in = "constant 2010 US$MER", unit_out = "constant 2017 US$MER",
                           label = "constant to constant with years")
})

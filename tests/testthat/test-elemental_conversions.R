#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = current_LCU

test_that("current_LCU_2_constant_LCU_base_x wb_wdi", {
  country_base_years <- wb_wdi %>%
    dplyr::filter(`GDP deflator (base year varies by country)` == 100) %>%
    dplyr::select("iso3c", "year")

  my_years <- country_base_years %>%
    dplyr::group_by(year) %>%
    dplyr::count() %>%
    dplyr::arrange(-n) %>%
    dplyr::pull(year)

  for (my_base_year in my_years) {
    my_countries <- country_base_years %>%
      dplyr::filter(year == my_base_year) %>%
      dplyr::pull(iso3c)

    gdp_in <- wb_wdi %>%
      dplyr::filter(iso3c %in% my_countries,
                    !is.na(`GDP (constant LCU)`)) %>%
      dplyr::select("iso3c", "year", "value" = `GDP (current LCU)`)

    gdp_conv <- current_LCU_2_constant_LCU_base_y(gdp_in, my_base_year, wb_wdi) %>%
      dplyr::filter(!is.na(value))

    gdp_out <- wb_wdi %>%
      dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
      dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

    expect_equal(gdp_conv, gdp_out)
  }
})

# TO LOOK AT: the deflator seems to be a mix between a linked deflator and a non-linked one
# test_that("current_LCU_2_constant_LCU_base_x imf_weo", {
#   country_base_years <- imf_weo %>%
#     dplyr::filter(`GDP deflator` == 1) %>%
#     dplyr::select("iso3c", "year")
#
#   my_years <- country_base_years %>%
#     dplyr::group_by(year) %>%
#     dplyr::count() %>%
#     dplyr::arrange(-n) %>%
#     dplyr::pull(year)
#
#   for (my_base_year in my_years) {
#     my_countries <- country_base_years %>%
#       dplyr::filter(year == my_base_year) %>%
#       dplyr::pull(iso3c)
#
#     gdp_in <- imf_weo %>%
#       dplyr::filter(iso3c %in% my_countries,
#                     !is.na(`GDP (constant LCU)`)) %>%
#       dplyr::select("iso3c", "year", "value" = `GDP (current LCU)`)
#
#     gdp_conv <- current_LCU_2_constant_LCU_base_y(gdp_in, my_base_year, imf_weo) %>%
#       dplyr::filter(!is.na(value))
#
#     gdp_out <- imf_weo %>%
#       dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
#       dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)
#
#     expect_equal(gdp_conv, gdp_out, tolerance = 0.01)
#   }
# })

test_that("current_LCU_2_current_IntPPP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (current international $)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP: linked series (current LCU)`)

  gdp_conv <- current_LCU_2_current_IntPPP(gdp_in, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (current international $)`)

  expect_equal(gdp_conv, gdp_out)
})

test_that("current_LCU_2_current_USMER", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP (current US$)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current LCU)`)

  gdp_conv <- current_LCU_2_current_USMER(gdp_in, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current US$)`)

  expect_equal(gdp_conv, gdp_out)
})

#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = current_IntPPP

test_that("current_IntPPP_2_current_LCU", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP: linked series (current LCU)`),
                  `GDP (current LCU)` == `GDP: linked series (current LCU)`) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (current international $)`)

  gdp_conv <- current_IntPPP_2_current_LCU(gdp_in, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP: linked series (current LCU)`)

  expect_equal(gdp_conv, gdp_out)

  # Test IMF
  # gdp_in <- imf_weo %>%
  #   dplyr::filter(!is.na(`GDP (current LCU)`)) %>%
  #   dplyr::select("iso3c", "year", "value" = `GDP, PPP (current international $)`)
  #
  # gdp_conv <- current_IntPPP_2_current_LCU(gdp_in, imf_weo) %>%
  #   dplyr::filter(!is.na(value))
  #
  # gdp_out <- imf_weo %>%
  #   dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
  #   dplyr::select("iso3c", "year", "value" = `GDP (current LCU)`)
  #
  # expect_equal(gdp_conv, gdp_out, tolerance = 0.0001)
})


#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = current_USMER

test_that("current_USMER_2_current_LCU", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP (current LCU)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current US$)`)

  gdp_conv <- current_USMER_2_current_LCU(gdp_in, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current LCU)`)

  expect_equal(gdp_conv, gdp_out)

  # Test IMF
  # gdp_in <- imf_weo %>%
  #   dplyr::filter(!is.na(`GDP (current LCU)`)) %>%
  #   dplyr::select("iso3c", "year", "value" = `GDP (current US$)`)
  #
  # gdp_conv <- current_USMER_2_current_LCU(gdp_in, imf_weo) %>%
  #   dplyr::filter(!is.na(value))
  #
  # gdp_out <- imf_weo %>%
  #   dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
  #   dplyr::select("iso3c", "year", "value" = `GDP (current LCU)`)
  #
  # expect_equal(gdp_conv, gdp_out)
})

#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = constant_LCU

test_that("constant_LCU_2_constant_IntPPP", {
  country_base_years <- wb_wdi %>%
    dplyr::filter(`GDP deflator (base year varies by country)` == 100) %>%
    dplyr::select("iso3c", "year")

  my_countries <- country_base_years %>%
    dplyr::filter(year == 2017) %>%
    dplyr::pull(iso3c)

  gdp_in <- wb_wdi %>%
    dplyr::filter(iso3c %in% my_countries,
                  !is.na(`GDP, PPP (constant 2017 international $)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

  gdp_conv <- constant_LCU_2_constant_IntPPP(gdp_in, 2017, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (constant 2017 international $)`)

  expect_equal(gdp_conv, gdp_out)
})

test_that("constant_LCU_2_constant_USMER", {
  country_base_years <- wb_wdi %>%
    dplyr::filter(`GDP deflator (base year varies by country)` == 100) %>%
    dplyr::select("iso3c", "year")

  my_countries <- country_base_years %>%
    dplyr::filter(year == 2010) %>%
    dplyr::pull(iso3c)

  gdp_in <- wb_wdi %>%
    dplyr::filter(iso3c %in% my_countries,
                  !is.na(`GDP (constant 2010 US$)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

  gdp_conv <- constant_LCU_2_constant_USMER(gdp_in, 2010, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (constant 2010 US$)`)

  expect_equal(gdp_conv, gdp_out)
})



#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = constant_IntPPP

test_that("constant_IntPPP_2_constant_LCU", {
  country_base_years <- wb_wdi %>%
    dplyr::filter(`GDP deflator (base year varies by country)` == 100) %>%
    dplyr::select("iso3c", "year")

  my_countries <- country_base_years %>%
    dplyr::filter(year == 2017) %>%
    dplyr::pull(iso3c)

  gdp_in <- wb_wdi %>%
    dplyr::filter(iso3c %in% my_countries,
                  !is.na(`GDP (constant LCU)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (constant 2017 international $)`)

  gdp_conv <- constant_IntPPP_2_constant_LCU(gdp_in, 2017, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

  expect_equal(gdp_conv, gdp_out)
})

#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = constant_USMER

test_that("constant_USMER_2_constant_LCU", {
  country_base_years <- wb_wdi %>%
    dplyr::filter(`GDP deflator (base year varies by country)` == 100) %>%
    dplyr::select("iso3c", "year")

  my_countries <- country_base_years %>%
    dplyr::filter(year == 2010) %>%
    dplyr::pull(iso3c)

  gdp_in <- wb_wdi %>%
    dplyr::filter(iso3c %in% my_countries,
                  !is.na(`GDP (constant LCU)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (constant 2010 US$)`)

  gdp_conv <- constant_USMER_2_constant_LCU(gdp_in, 2010, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

  expect_equal(gdp_conv, gdp_out)
})

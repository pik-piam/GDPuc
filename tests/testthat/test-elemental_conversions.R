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

    for (country in unique(gdp_conv$iso3c)) {
      expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                   gdp_out %>% dplyr::filter(iso3c == country),
                   label = country)
    }
  }
})

# test_that("current_LCU_2_constant_LCU_base_y linked series", {
#   country_base_years <- wb_wdi_linked %>%
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
#     gdp_in <- wb_wdi %>%
#       dplyr::filter(iso3c %in% my_countries,
#                     !is.na(`GDP (constant LCU)`)) %>%
#       dplyr::select("iso3c", "year", "value" = `GDP: linked series (current LCU)`)
#
#     gdp_conv <- current_LCU_2_constant_LCU_base_y(gdp_in, my_base_year, wb_wdi_linked) %>%
#       dplyr::filter(!is.na(value))
#
#     gdp_out <- wb_wdi %>%
#       dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
#       dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)
#
#     for (country in unique(gdp_conv$iso3c)) {
#       expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
#                    gdp_out %>% dplyr::filter(iso3c == country),
#                    label = country)
#     }
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

  for (country in unique(gdp_conv$iso3c)) {
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
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

  for (country in unique(gdp_conv$iso3c)) {
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
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

  for (country in unique(gdp_conv$iso3c)) {
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
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

  for (country in unique(gdp_conv$iso3c)) {
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
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
    dplyr::filter(year == year_IntPPP) %>%
    dplyr::pull(iso3c)

  gdp_in <- wb_wdi %>%
    dplyr::filter(iso3c %in% my_countries,
                  dplyr::if_all(tidyselect::matches(regex_var_IntPPP), ~!is.na(.x))) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

  gdp_conv <- constant_LCU_2_constant_IntPPP(gdp_in, year_IntPPP, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_IntPPP))

  for (country in unique(gdp_conv$iso3c)) {
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})

test_that("constant_LCU_2_constant_USMER", {
  country_base_years <- wb_wdi %>%
    dplyr::filter(`GDP deflator (base year varies by country)` == 100) %>%
    dplyr::select("iso3c", "year")

  my_countries <- country_base_years %>%
    dplyr::filter(year == year_USMER) %>%
    dplyr::pull(iso3c)

  gdp_in <- wb_wdi %>%
    dplyr::filter(iso3c %in% my_countries,
                  dplyr::if_all(tidyselect::matches(regex_var_USMER), ~!is.na(.x))) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

  gdp_conv <- constant_LCU_2_constant_USMER(gdp_in, year_USMER, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_USMER))

  for (country in unique(gdp_conv$iso3c)) {
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
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
    dplyr::filter(year == year_IntPPP) %>%
    dplyr::pull(iso3c)

  gdp_in <- wb_wdi %>%
    dplyr::filter(iso3c %in% my_countries,
                  !is.na(`GDP (constant LCU)`)) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_IntPPP))

  gdp_conv <- constant_IntPPP_2_constant_LCU(gdp_in, year_IntPPP, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

  for (country in unique(gdp_conv$iso3c)) {
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
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
    dplyr::filter(year == year_USMER) %>%
    dplyr::pull(iso3c)

  gdp_in <- wb_wdi %>%
    dplyr::filter(iso3c %in% my_countries,
                  !is.na(`GDP (constant LCU)`)) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_USMER))

  gdp_conv <- constant_USMER_2_constant_LCU(gdp_in, year_USMER, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

  for (country in unique(gdp_conv$iso3c)) {
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})


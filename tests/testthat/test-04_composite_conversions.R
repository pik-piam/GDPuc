#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = current LCU

test_that("current_LCU_2_current_xCU", {
  # This test only checks that a conversion to a country's own currency returns the same value
  gdp_in <- wb_wdi %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current LCU)`) %>%
    dplyr::filter(!is.na(value))

  for (country in unique(gdp_in$iso3c)) {
    gdp_conv <- current_LCU_2_current_xCU(gdp_in, iso3c_y = country, wb_wdi) %>%
      dplyr::filter(!is.na(value))

    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_in %>% dplyr::semi_join(dplyr::filter(gdp_conv, iso3c == country),
                                             by = dplyr::join_by("iso3c", "year")),
                 label = country)
  }

})

test_that("current_LCU_2_constant_IntPPP_base_y", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(!!rlang::sym(var_IntPPP))) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current LCU)`)

  gdp_conv <- current_LCU_2_constant_IntPPP_base_y(gdp_in, year_IntPPP, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_IntPPP))

  for (country in unique(gdp_conv$iso3c)) {
    if (country %in% bad_countries) next
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})

test_that("current_LCU_2_constant_xCU_base_y", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(!!rlang::sym(var_USMER))) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current LCU)`)

  gdp_conv <- current_LCU_2_constant_xCU_base_y(gdp_in, "USA", year_USMER, wb_wdi)%>%
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
# Unit_in = current Int$PPP

test_that("current_IntPPP_2_current_USMER", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP (current US$)`)) %>%
    # Convert the PPP current series from one based on the linked LCU series
    # to one based on the standard LCU series
    dplyr::mutate(value = `GDP, PPP (current international $)` *
                    `GDP deflator (base year varies by country)` /
                    `GDP deflator: linked series (base year varies by country)`) %>%
    dplyr::select("iso3c", "year", "value")

  gdp_conv <- current_IntPPP_2_current_USMER(gdp_in, wb_wdi)%>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current US$)`)

  for (country in unique(gdp_conv$iso3c)) {
    if (country %in% bad_countries) next
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})

test_that("current_IntPPP_2_constant_LCU_base_y", {
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
      # Convert the PPP current series from one based on the linked LCU series
      # to one based on the standard LCU series
      dplyr::mutate(value = `GDP, PPP (current international $)` *
                      `GDP deflator (base year varies by country)` /
                      `GDP deflator: linked series (base year varies by country)`) %>%
      dplyr::select("iso3c", "year", "value")

    if (nrow(gdp_in) == 0) next

    gdp_conv <- current_IntPPP_2_constant_LCU_base_y(gdp_in, my_base_year, wb_wdi) %>%
      dplyr::filter(!is.na(value))

    gdp_out <- wb_wdi %>%
      dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
      dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

    for (country in unique(gdp_conv$iso3c)) {
      if (country %in% bad_countries) next
      expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                   gdp_out %>% dplyr::filter(iso3c == country),
                   label = country)
    }
  }
})


test_that("current_IntPPP_2_constant_IntPPP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(!!rlang::sym(var_IntPPP))) %>%
    # Convert the PPP current series from one based on the linked LCU series
    # to one based on the standard LCU series
    dplyr::mutate(value = `GDP, PPP (current international $)` *
                    `GDP deflator (base year varies by country)` /
                    `GDP deflator: linked series (base year varies by country)`) %>%
    dplyr::select("iso3c", "year", "value")

  gdp_conv <- current_IntPPP_2_constant_IntPPP_base_y(gdp_in, year_IntPPP, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_IntPPP))

  for (country in unique(gdp_conv$iso3c)) {
    if (country %in% bad_countries) next
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})


test_that("current_IntPPP_2_constant_xCU_base_y", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(!!rlang::sym(var_USMER))) %>%
    # Convert the PPP current series from one based on the linked LCU series
    # to one based on the standard LCU series
    dplyr::mutate(value = `GDP, PPP (current international $)` *
                    `GDP deflator (base year varies by country)` /
                    `GDP deflator: linked series (base year varies by country)`) %>%
    dplyr::select("iso3c", "year", "value")

  gdp_conv <- current_IntPPP_2_constant_xCU_base_y(gdp_in, "USA", year_USMER, wb_wdi)%>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_USMER))

  for (country in unique(gdp_conv$iso3c)) {
    if (country %in% bad_countries) next
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})


#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = current xCU

test_that("current_xCU_2_current_IntPPP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (current international $)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current US$)`)

  gdp_conv <- current_xCU_2_current_IntPPP(gdp_in, "USA", wb_wdi)%>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year"))  %>%
    # Convert the PPP current series from one bases on the linked LCU series
    # to one based on the standard LCU series
    dplyr::mutate(value = `GDP, PPP (current international $)` *
                    `GDP deflator (base year varies by country)` /
                    `GDP deflator: linked series (base year varies by country)`) %>%
    dplyr::select("iso3c", "year", "value")

  for (country in unique(gdp_conv$iso3c)) {
    if (country %in% bad_countries) next
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})

test_that("current_xCU_2_constant_xCU_base_y", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(!!rlang::sym(var_USMER))) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current US$)`)

  gdp_conv <- current_xCU_2_constant_xCU_base_y(gdp_in, "USA", "USA", year_USMER, wb_wdi) %>%
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

test_that("current_xCU_2_constant_IntPPP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(!!rlang::sym(var_IntPPP))) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current US$)`)

  gdp_conv <- current_xCU_2_constant_IntPPP_base_y(gdp_in, "USA", year_IntPPP, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_IntPPP))

  for (country in unique(gdp_conv$iso3c)) {
    if (country %in% bad_countries) next
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})

#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = constant_LCU

test_that("constant_LCU_base_x_2_current_IntPPP", {
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
                    !is.na(`GDP, PPP (current international $)`)) %>%
      dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

    if (nrow(gdp_in) == 0) next

    gdp_conv <- constant_LCU_base_x_2_current_IntPPP(gdp_in, my_base_year, wb_wdi) %>%
      dplyr::filter(!is.na(value))

    gdp_out <- wb_wdi %>%
      dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
      # Convert the PPP current series from one bases on the linked LCU series
      # to one based on the standard LCU series
      dplyr::mutate(value = `GDP, PPP (current international $)` *
                      `GDP deflator (base year varies by country)` /
                      `GDP deflator: linked series (base year varies by country)`) %>%
      dplyr::select("iso3c", "year", "value")

    for (country in unique(gdp_conv$iso3c)) {
      if (country %in% bad_countries) next
      expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                   gdp_out %>% dplyr::filter(iso3c == country),
                   label = country)
    }
  }
})

test_that("constant_LCU_base_x_2_current_USMER", {
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
                    !is.na(`GDP (current US$)`)) %>%
      dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

    if (nrow(gdp_in) == 0) next

    gdp_conv <- constant_LCU_base_x_2_current_USMER(gdp_in, my_base_year, wb_wdi) %>%
      dplyr::filter(!is.na(value))

    gdp_out <- wb_wdi %>%
      dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
      dplyr::select("iso3c", "year", "value" = `GDP (current US$)`)

    for (country in unique(gdp_conv$iso3c)) {
      expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                   gdp_out %>% dplyr::filter(iso3c == country),
                   label = country)
    }
  }
})

test_that("constant_LCU_base_x_2_constant_IntPPP_base_y", {
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
                    !is.na(!!rlang::sym(var_IntPPP))) %>%
      dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

    gdp_conv <- constant_LCU_base_x_2_constant_IntPPP_base_y(gdp_in, my_base_year, year_IntPPP, wb_wdi) %>%
      dplyr::filter(!is.na(value))

    gdp_out <- wb_wdi %>%
      dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
      dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_IntPPP))

    for (country in unique(gdp_conv$iso3c)) {
      if (country %in% bad_countries) next
      expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                   gdp_out %>% dplyr::filter(iso3c == country),
                   label = country)
    }
  }
})

test_that("constant_LCU_base_x_2_constant_xCU_base_y", {
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
                    !is.na(!!rlang::sym(var_USMER))) %>%
      dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

    gdp_conv <- constant_LCU_base_x_2_constant_xCU_base_y(gdp_in, my_base_year, "USA", year_USMER, wb_wdi) %>%
      dplyr::filter(!is.na(value))

    gdp_out <- wb_wdi %>%
      dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
      dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_USMER))

    for (country in unique(gdp_conv$iso3c)) {
      expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                   gdp_out %>% dplyr::filter(iso3c == country),
                   label = country)
    }
  }
})


#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = constant_IntPPP

test_that("constant_IntPPP_base_x_2_current_LCU", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP (current LCU)`),
                  `GDP: linked series (current LCU)` == `GDP (current LCU)`, iso3c == "USA") %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_IntPPP))

  gdp_conv <- constant_IntPPP_base_x_2_current_LCU(gdp_in, year_IntPPP, wb_wdi)%>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP: linked series (current LCU)`)

  for (country in unique(gdp_conv$iso3c)) {
    if (country %in% bad_countries) next
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})

test_that("constant_IntPPP_base_x_2_current_xCU", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP (current US$)`)) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_IntPPP))

  gdp_conv <- constant_IntPPP_base_x_2_current_xCU(gdp_in, year_IntPPP, "USA", wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current US$)`)

  for (country in unique(gdp_conv$iso3c)) {
    if (country %in% bad_countries) next
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})

test_that("constant_IntPPP_base_x_2_current_IntPPP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (current international $)`)) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_IntPPP))

  gdp_conv <- constant_IntPPP_base_x_2_current_IntPPP(gdp_in, year_IntPPP, wb_wdi_linked) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (current international $)`)

  for (country in unique(gdp_conv$iso3c)) {
    if (country %in% bad_countries) next
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})

test_that("constant_IntPPP_base_x_2_constant_LCU_base_y", {
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
      dplyr::filter(iso3c %in% my_countries) %>%
      dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_IntPPP))

    gdp_conv <- constant_IntPPP_base_x_2_constant_LCU_base_y(gdp_in, year_IntPPP, my_base_year, wb_wdi) %>%
      dplyr::filter(!is.na(value))

    gdp_out <- wb_wdi %>%
      dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
      dplyr::select("iso3c", "year", "value" = `GDP (constant LCU)`)

    for (country in unique(gdp_conv$iso3c)) {
      if (country %in% bad_countries) next
      expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                   gdp_out %>% dplyr::filter(iso3c == country),
                   label = country)
    }
  }
})

test_that("constant_IntPPP_base_x_2_constant_xCU_base_y", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(!!rlang::sym(var_USMER))) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_IntPPP))

  gdp_conv <- constant_IntPPP_base_x_2_constant_xCU_base_y(gdp_in, year_IntPPP, "USA", year_USMER, wb_wdi)%>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_USMER))

  for (country in unique(gdp_conv$iso3c)) {
    if (country %in% bad_countries) next
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})

test_that("constant_IntPPP_base_x_2_constant_xCU_base_y", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(!!rlang::sym(var_USMER))) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_IntPPP))

  for (country in unique(gdp_in$iso3c)) {
    gdp_in <- dplyr::filter(gdp_in, iso3c == country)

    gdp_conv <- constant_IntPPP_base_x_2_constant_xCU_base_y(gdp_in, year_IntPPP, country, year_USMER, wb_wdi)%>%
      dplyr::filter(!is.na(value))

    gdp_out <-constant_IntPPP_base_x_2_constant_LCU_base_y(gdp_in, year_IntPPP, year_USMER, wb_wdi)%>%
      dplyr::filter(!is.na(value))

    if (country %in% bad_countries) next

    expect_equal(gdp_conv, gdp_out, label = country)
  }
})


#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = constant_USMER

test_that("constant_USMER_base_x_2_current_LCU", {
  gdp_in <- wb_wdi %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_USMER))

  gdp_conv <- constant_USMER_base_x_2_current_LCU(gdp_in, year_USMER, wb_wdi)%>%
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

test_that("constant_xCU_base_x_2_current_IntPPP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (current international $)`)) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_USMER))

  gdp_conv <- constant_xCU_base_x_2_current_IntPPP(gdp_in, "USA", year_USMER, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year"))%>%
    # Convert the PPP current series from one bases on the linked LCU series
    # to one based on the standard LCU series
    dplyr::mutate(value = `GDP, PPP (current international $)` *
                    `GDP deflator (base year varies by country)` /
                    `GDP deflator: linked series (base year varies by country)`) %>%
    dplyr::select("iso3c", "year", "value")

  for (country in unique(gdp_conv$iso3c)) {
    if (country %in% bad_countries) next
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})

test_that("constant_xCU_base_x_2_current_USMER", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP (current US$)`)) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_USMER))

  gdp_conv <- constant_xCU_base_x_2_current_USMER(gdp_in, "USA", year_USMER, wb_wdi) %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = `GDP (current US$)`) %>%
    dplyr::select("iso3c", "year", "value")

  for (country in unique(gdp_conv$iso3c)) {
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})

test_that("constant_xCU_base_x_2_constant_IntPPP_base_y", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(!!rlang::sym(var_IntPPP))) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_USMER))

  gdp_conv <- constant_xCU_base_x_2_constant_IntPPP_base_y(gdp_in, "USA", year_USMER, year_IntPPP, wb_wdi)%>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_IntPPP))

  for (country in unique(gdp_conv$iso3c)) {
    if (country %in% bad_countries) next
    expect_equal(gdp_conv %>% dplyr::filter(iso3c == country),
                 gdp_out %>% dplyr::filter(iso3c == country),
                 label = country)
  }
})

test_that("constant_USMER_base_x_2_constant_LCU_base_y", {
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
      dplyr::filter(iso3c %in% my_countries) %>%
      dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_USMER))

    gdp_conv <- constant_USMER_base_x_2_constant_LCU_base_y(gdp_in, year_USMER, my_base_year, wb_wdi) %>%
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


test_that("constant_xCU_base_x_2_constant_xCU_base_y", {
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

    for (country in my_countries) {
    gdp_in <- wb_wdi %>%
      dplyr::filter(iso3c == country) %>%
      dplyr::select("iso3c", "year", "value" = tidyselect::matches(regex_var_USMER))

    gdp_conv <- constant_xCU_base_x_2_constant_xCU_base_y(gdp_in, "USA", year_USMER, country, my_base_year, wb_wdi) %>%
      dplyr::filter(!is.na(value))

    gdp_out <- constant_USMER_base_x_2_constant_LCU_base_y(gdp_in, year_USMER, my_base_year, wb_wdi) %>%
      dplyr::filter(!is.na(value))

      expect_equal(gdp_conv, gdp_out, label = country)
    }
  }
})

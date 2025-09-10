#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = current_LCU

# Convert from current LCU to current Int$PPP
current_LCU_2_current_IntPPP <- function(gdp, source) {
  PPP <-  source %>%
    dplyr::select("iso3c", "year", "PPP conversion factor, GDP (LCU per international $)")

  cli_elemental(from = "current LCU",
                to = "current Int$PPP",
                with = "PPP conversion factor",
                unit = "(LCU per international $)",
                val = dplyr::filter(PPP,
                                    .data$iso3c %in% unique(gdp$iso3c),
                                    .data$year %in% unique(gdp$year)))

  gdp %>%
    dplyr::left_join(PPP, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = .data$value / .data$`PPP conversion factor, GDP (LCU per international $)`,
                  .keep = "unused")
}

# Convert from current LCU to current xLCU
current_LCU_2_current_USMER <- function(gdp, source) {
  MER <-  source %>%
    dplyr::select("iso3c", "year", "MER (LCU per US$)")

  cli_elemental(from = "current LCU",
                to = "current US$MER",
                with = "MER",
                unit = "(LCU per US$)",
                val = dplyr::filter(MER,
                                    .data$iso3c %in% unique(gdp$iso3c),
                                    .data$year %in% unique(gdp$year)))

  gdp %>%
    dplyr::left_join(MER, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = .data$value / .data$`MER (LCU per US$)`,
                  .keep = "unused")
}

# Convert from current LCU to constant LCU base y
current_LCU_2_constant_LCU_base_y <- function(gdp, base_y, source) {

  deflator <-  "GDP deflator"

  gdp_deflator <- source %>%
    dplyr::select("iso3c", "year", rlang::sym(deflator))

  def_base_unkwown_at_y <- gdp_deflator %>%
    dplyr::filter(.data$year == base_y) %>%
    dplyr::select("iso3c", "GDP deflator in y" = rlang::sym(deflator))

  def_base_y <- gdp_deflator  %>%
    dplyr::left_join(def_base_unkwown_at_y, by = "iso3c") %>%
    dplyr::mutate(!!rlang::sym(deflator) := !!rlang::sym(deflator) / .data$`GDP deflator in y`,
                  .keep = "unused")

  cli_elemental(from = "current LCU",
                to = glue::glue("constant {base_y} LCU"),
                with = glue::glue("Base {base_y} {deflator}"),
                unit = glue::glue("(current LCU per contant {base_y} LCU)"),
                val =  dplyr::filter(def_base_y,
                                     .data$iso3c %in% unique(gdp$iso3c),
                                     .data$year %in% unique(gdp$year)))

  gdp %>%
    dplyr::left_join(def_base_y, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = .data$value / !!rlang::sym(deflator), .keep = "unused")
}

#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = current_IntPPP

# Convert from current Int$PPP to current LCU
current_IntPPP_2_current_LCU <- function(gdp, source) {
  PPP <-  source %>%
    dplyr::select("iso3c", "year", "PPP conversion factor, GDP (LCU per international $)")

  cli_elemental(from = "current Int$PPP",
                to = "current LCU",
                with = "PPP conversion factor",
                unit = "(LCU per international $)",
                val = dplyr::filter(PPP,
                                    .data$iso3c %in% unique(gdp$iso3c),
                                    .data$year %in% unique(gdp$year)))

  gdp %>%
    dplyr::left_join(PPP, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = .data$value * .data$`PPP conversion factor, GDP (LCU per international $)`,
                  .keep = "unused")
}

#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = current_USMER, current_xCU

# Convert from current US$MER to current LCU
current_USMER_2_current_LCU <- function(gdp, source) {
  MER <-  source %>%
    dplyr::select("iso3c", "year", "MER (LCU per US$)")

  cli_elemental(from = "current US$MER",
                to = "current LCU",
                with = "MER",
                unit = "(LCU per US$)",
                val = dplyr::filter(MER,
                                    .data$iso3c %in% unique(gdp$iso3c),
                                    .data$year %in% unique(gdp$year)))

  gdp %>%
    dplyr::left_join(MER, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = .data$value * .data$`MER (LCU per US$)`,
                  .keep = "unused")
}

current_xCU_2_current_USMER <- function(gdp, iso3c_x, source) {
  if (iso3c_x == "USA") {
    return(gdp)
  }

  MER <- source %>%
    dplyr::filter(.data$iso3c == iso3c_x) %>%
    dplyr::select("iso3c", "year", "MER (LCU per US$)")

  cli_elemental(from = glue::glue("current {iso3c_x}_CU"),
                to = "current US$MER",
                with = "MER",
                unit = glue::glue("{iso3c_x}_CU per US$"),
                val = dplyr::filter(MER, .data$year %in% unique(gdp$year)))

  gdp %>%
    dplyr::left_join(MER %>% dplyr::select(-"iso3c"), by = "year") %>%
    dplyr::mutate(value = .data$value / .data$`MER (LCU per US$)`, .keep = "unused")
}

current_USMER_2_current_xCU <- function(gdp, iso3c_y, source) {
  if (iso3c_y == "USA") {
    return(gdp)
  }

  MER <- source %>%
    dplyr::filter(.data$iso3c == iso3c_y) %>%
    dplyr::select("iso3c", "year", "MER (LCU per US$)")

  cli_elemental(from = "current US$MER",
                to = glue::glue("current {iso3c_y}_CU"),
                with = "MER",
                unit = glue::glue("{iso3c_y}_CU per US$"),
                val = dplyr::filter(MER, .data$year %in% unique(gdp$year)))

  gdp %>%
    dplyr::left_join(MER %>% dplyr::select(-"iso3c"), by = "year") %>%
    dplyr::mutate(value = .data$value * .data$`MER (LCU per US$)`, .keep = "unused")
}


#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = constant_LCU

# Convert from constant LCU base x to current LCU
constant_LCU_base_x_2_current_LCU <- function(gdp, base_x, source) {

  deflator <-  "GDP deflator"

  gdp_deflator <- source %>%
    dplyr::select("iso3c", "year", rlang::sym(deflator))

  def_base_unkwown_at_x <- gdp_deflator %>%
    dplyr::filter(.data$year == base_x) %>%
    dplyr::select("iso3c", "GDP deflator in x" = rlang::sym(deflator))

  def_base_x <- gdp_deflator  %>%
    dplyr::left_join(def_base_unkwown_at_x, by = "iso3c") %>%
    dplyr::mutate(!!rlang::sym(deflator) := !!rlang::sym(deflator) / .data$`GDP deflator in x`,
                  .keep = "unused")

  cli_elemental(from = glue::glue("constant {base_x} LCU"),
                to = glue::glue("current LCU"),
                with = glue::glue("Base {base_x} {deflator}"),
                unit = glue::glue("(current LCU per constant {base_x} LCU)"),
                val = dplyr::filter(def_base_x,
                                    .data$iso3c %in% unique(gdp$iso3c),
                                    .data$year %in% unique(gdp$year)))

  gdp %>%
    dplyr::left_join(def_base_x, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = .data$value * !!rlang::sym(deflator), .keep = "unused")
}

# Convert constant LCU series from one base year to another
constant_LCU_base_x_2_constant_LCU_base_y <- function(gdp, base_x, base_y, source) {
  if (base_x == base_y) {
    return(gdp)
  }

  deflator <-  "GDP deflator"

  gdps_LCU <- source %>%
    dplyr::select("iso3c", "year", rlang::sym(deflator))

  def_base_unknown_at_x <- gdps_LCU %>%
    dplyr::filter(.data$year == base_x) %>%
    dplyr::select("iso3c", "def" = rlang::sym(deflator))

  def_base_x <- gdps_LCU  %>%
    dplyr::left_join(def_base_unknown_at_x, by = "iso3c") %>%
    dplyr::mutate(!!rlang::sym(deflator) := !!rlang::sym(deflator) / .data$def,
                  .keep = "unused")

  def_base_x_at_y <- def_base_x %>%
    dplyr::filter(.data$year == base_y) %>%
    dplyr::select("iso3c", "def" = rlang::sym(deflator))

  cli_elemental(from = glue::glue("constant {base_x} LCU"),
                to = glue::glue("constant {base_y} LCU"),
                with = glue::glue("{base_y} value of base {base_x} {deflator}"),
                unit = glue::glue("(constant {base_y} LCU per constant {base_x} LCU)"),
                val = dplyr::filter(def_base_x_at_y, .data$iso3c %in% unique(gdp$iso3c)))

  gdp %>%
    dplyr::left_join(def_base_x_at_y, by = "iso3c") %>%
    dplyr::mutate(value = .data$value * .data$def, .keep = "unused")
}

# Convert from constant LCU to constant Int$PPP
constant_LCU_2_constant_IntPPP <- function(gdp, base, source) {
  PPP_base <-  source %>%
    dplyr::filter(.data$year == base) %>%
    dplyr::select("iso3c", "PPP conversion factor, GDP (LCU per international $)")

  cli_elemental(from = glue::glue("constant {base} LCU"),
                to = glue::glue("constant {base} Int$PPP"),
                with = glue::glue("{base} PPP conversion factor"),
                unit = "(LCU per international $)",
                val = dplyr::filter(PPP_base, .data$iso3c %in% unique(gdp$iso3c)))

  gdp %>%
    dplyr::left_join(PPP_base, by = "iso3c") %>%
    dplyr::mutate(value = .data$value / .data$`PPP conversion factor, GDP (LCU per international $)`,
                  .keep = "unused")
}

# Convert from constant LCU to constant US$MER
constant_LCU_2_constant_USMER <- function(gdp, base, source) {
  MER_base <-  source %>%
    dplyr::filter(.data$year == base) %>%
    dplyr::select("iso3c", "MER (LCU per US$)")

  cli_elemental(from = glue::glue("constant {base} LCU"),
                to = glue::glue("constant {base} US$MER"),
                with = glue::glue("{base} MER"),
                unit = "(LCU per US$)",
                val = dplyr::filter(MER_base, .data$iso3c %in% unique(gdp$iso3c)))

  gdp %>%
    dplyr::left_join(MER_base, by = "iso3c") %>%
    dplyr::mutate(value = .data$value / .data$`MER (LCU per US$)`,
                  .keep = "unused")
}

#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = constant_IntPPP

# Convert from constant Int$PPP to constant LCU
constant_IntPPP_2_constant_LCU <- function(gdp, base, source) {
  PPP_base <- source %>%
    dplyr::filter(.data$year == base) %>%
    dplyr::select("iso3c", "PPP conversion factor, GDP (LCU per international $)")

  cli_elemental(from = glue::glue("constant {base} Int$PPP"),
                to = glue::glue("constant {base} LCU"),
                with = glue::glue("{base} PPP conversion factor"),
                unit = "(LCU per international $)",
                val = dplyr::filter(PPP_base, .data$iso3c %in% unique(gdp$iso3c)))

  gdp %>%
    dplyr::left_join(PPP_base, by = "iso3c") %>%
    dplyr::mutate(value = .data$value * .data$`PPP conversion factor, GDP (LCU per international $)`,
                  .keep = "unused")
}

#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = constant_USMER

# Convert from constant US$MER to constant LCU
constant_USMER_2_constant_LCU <- function(gdp, base, source) {
  MER_base <-  source %>%
    dplyr::filter(.data$year == base) %>%
    dplyr::select("iso3c", "MER (LCU per US$)")

  cli_elemental(from = glue::glue("constant {base} US$MER"),
                to = glue::glue("constant {base} LCU"),
                with = glue::glue("{base} MER"),
                unit = "(LCU per US$)",
                val = dplyr::filter(MER_base, .data$iso3c %in% unique(gdp$iso3c)))

  gdp %>%
    dplyr::left_join(MER_base, by = "iso3c") %>%
    dplyr::mutate(value = .data$value * .data$`MER (LCU per US$)`, .keep = "unused")
}

# Convert from constant US$MER to constant xCU
constant_USMER_2_constant_xCU <- function(gdp, iso3c_unit, base, source) {
  if (iso3c_unit == "USA") {
    return(gdp)
  }

  xCU_per_DOLLAR <- source %>%
    dplyr::filter(.data$year == base, .data$iso3c == iso3c_unit) %>%
    dplyr::pull(.data$`MER (LCU per US$)`)

  MER_base <- source %>%
    dplyr::filter(.data$year == base) %>%
    dplyr::mutate("MER (LCU per US$)" = xCU_per_DOLLAR) %>%
    dplyr::select("iso3c", "MER (LCU per US$)")

  cli_elemental(from = glue::glue("constant {base} US$MER"),
                to = glue::glue("constant {base} {iso3c_unit}_CU"),
                with = glue::glue("{base} MER"),
                unit = glue::glue("({iso3c_unit}_CU per US$MER)"),
                val = dplyr::filter(MER_base, .data$iso3c %in% unique(gdp$iso3c)))

  gdp %>%
    dplyr::left_join(MER_base, by = "iso3c") %>%
    dplyr::mutate(value = .data$value * .data$`MER (LCU per US$)`, .keep = "unused")
}


#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# Unit_in = constant xCU

constant_xCU_2_constant_USMER <- function(gdp, iso3c_unit, base, source) {
  if (iso3c_unit == "USA") {
    return(gdp)
  }

  xCU_per_DOLLAR <- source %>%
    dplyr::filter(.data$year == base, .data$iso3c == iso3c_unit) %>%
    dplyr::pull(.data$`MER (LCU per US$)`)

  MER_base <- source %>%
    dplyr::filter(.data$year == base) %>%
    dplyr::mutate("MER (LCU per US$)" = xCU_per_DOLLAR) %>%
    dplyr::select("iso3c", "MER (LCU per US$)")

  cli_elemental(from = glue::glue("constant {base} {iso3c_unit}_CU"),
                to = glue::glue("constant {base} US$MER"),
                with = glue::glue("{base} MER"),
                unit = glue::glue("({iso3c_unit}_CU per US$MER)"),
                val = dplyr::filter(MER_base, .data$iso3c %in% unique(gdp$iso3c)))

  gdp %>%
    dplyr::left_join(MER_base, by = "iso3c") %>%
    dplyr::mutate(value = .data$value / .data$`MER (LCU per US$)`, .keep = "unused")
}

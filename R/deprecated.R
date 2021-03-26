# DEPRECATED
# @inheritParams constant_LCU_base_x_2_constant_LCU_base_y
# constant_LCU_2_constant_LCU_base_y <- function(gdp, base_y, source) {
#   gdp_current_LCU <- eval(rlang::sym(source)) %>%
#     dplyr::select(iso3c, year, `GDP (current LCU)`)
#
#   def_at_y <- gdp %>%
#     dplyr::left_join(gdp_current_LCU, by = c("iso3c", "year")) %>%
#     dplyr::mutate(def = `GDP (current LCU)` / value) %>%
#     dplyr::filter(year == base_y) %>%
#     dplyr::select(iso3c, def)
#
#   gdp %>%
#     dplyr::left_join(def_at_y, by = "iso3c") %>%
#     dplyr::mutate(value = value * def, .keep = "unused")
# }

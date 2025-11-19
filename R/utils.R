# Eager pipe. Used to make sure the messages of the elemental conversion are printed in the right order.
`%>e%` <- magrittr::pipe_eager_lexical

smart_select_iso3c <- function(gdp, def = "iso3c") {
  if (def %in% colnames(gdp)) {
    return(def)
  }
  gdp %>%
    dplyr::select(tidyselect::vars_select_helpers$where(
      ~ (is.character(.x) || is.factor(.x)) &&
        all(nchar(as.character(.x)) == 3) &&
        all(.x == toupper(.x))
    )) %>%
    colnames()
}

smart_select_year <- function(gdp, def = "year") {
  if (def %in% colnames(gdp)) {
    return(def)
  }
  gdp %>%
    dplyr::select(tidyselect::vars_select_helpers$where(
      ~ is.numeric(.x) &&
        all(!is.na(.x)) &&
        all(nchar(as.character(.x)) == 4)
    )) %>%
    colnames()
}

#' Print information on sources
#'
#' @description
#'
#' Print detailed information on conversion factor sources to the screen. Information includes the name, origin, date,
#'  html-link and an associated note. Calling the function without any argument will print information on all
#'  available sources.
#'
#' @param source Empty, or the name of one of the internal sources:
#'   \enumerate{
#'     \item "wb_wdi"
#'     \item "wb_wdi_linked"
#'     \item "wb_wdi_cpi"
#'   }
#' @return No return value, called for side effects.
#' @examples
#' print_source_info()
#'
#' @export
print_source_info <- function(source) {
  if (missing(source)) cli::cli_alert_info("Sources available:")
  if (missing(source) || source == "wb_wdi") {
    cli_source_info(name = "wb_wdi",
                    origin = "The World Bank's World Development Indicator Database",
                    date = "Downloaded on the 30th of April 2024",
                    html = "https://databank.worldbank.org/source/world-development-indicators",
                    note = "Uses the GDP deflator.")
  }
  if (missing(source) || source == "wb_wdi_linked") {
    cli_source_info(name = "wb_wdi_linked",
                    origin = "The World Bank's World Development Indicator Database",
                    date = "Downloaded on the 30th of April 2024",
                    html = "https://databank.worldbank.org/source/world-development-indicators",
                    note = "Uses the linked GDP deflator.")
  }
  if (missing(source) || source == "wb_wdi_cpi") {
    cli_source_info(name = "wb_wdi_cpi",
                    origin = "The World Bank's World Development Indicator Database",
                    date = "Downloaded on the 30th of April 2024",
                    html = "https://databank.worldbank.org/source/world-development-indicators",
                    note = "Uses the CPI as deflator.")
  }
}

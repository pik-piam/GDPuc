#' Print information on sources
#'
#' @description
#'  `r lifecycle::badge("stable")`
#'
#' Print detailed information on sources to the screen. Information includes the
#' name, origin, date, html-link and an associated note. Calling the function
#' without any argument will print information on all available sources.
#'
#' @param source The name of one of the internal sources:
#'   \enumerate{
#'     \item "wb_wdi"
#'     \item "wb_wdi_linked"
#'   }
#' @export
print_source_info <- function(source) {
  if (missing(source)) cli::cli_alert_info("Sources available:")
  if (missing(source) || source == "wb_wdi") {
    cli_source_info(name = "wb_wdi",
                    origin = "The World Bank's World Development Indicator Database",
                    date = "Downloaded on the 22 of Oktober 2021",
                    html = "https://databank.worldbank.org/source/world-development-indicators",
                    note = "Uses the standard deflator.")
  }
  if (missing(source) || source == "wb_wdi_linked") {
    cli_source_info(name = "wb_wdi_linked",
                    origin = "The World Bank's World Development Indicator Database",
                    date = "Downloaded on the 22 of Oktober 2021",
                    html = "https://databank.worldbank.org/source/world-development-indicators",
                    note = "Uses the linked deflator.")
  }
}

#' Check that an API key exists for \pkg{tidycensus} calls.
#' @noRd
check_census_api_key <- function () {

    key <- Sys.getenv ("CENSUS_API_KEY")
    if (!nzchar (key)) {
        stop (
            "This package requires an API key from ",
            "census.gov to be stored as ",
            "'CENSUS_API_KEY', generally in an ",
            ".Renviron file.",
            call. = FALSE
        )
    }
}

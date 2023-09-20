#' Get raw data from US census
#'
#' This returns the five primary variables required for the hardship index:
#' \itemize{
#'   \item \code{occupancy}: Proportion of rooms with > 1 occupant
#'   \item \code{poverty}: Proportion of households below poverty
#'   \item \code{unemployment}: Proportion of unemployed adults (between 16 and
#'   64)
#'   \item \code{no_hs}: Proportion of population > 25 without a highschool
#'   diploma
#'   \item \code{prop_deps}: Proportion of population who may be considered
#'   dependent; that is either under 18 or over 65
#'   \item \code{income}: Per-capita income
#' }
#'
#' @param state A two-letter abbreviation for a US state (case-insensitive).
#' @param year Year for which data are to be obtained, must be between 2009 and
#' year prior to present.
#' @param survey Either "acs5" (the default) for five-year aggregated values,
#' or "acs1". Anuual availabilities differ for the different surveys. Data from
#' both surveys are available from 2010, for "acs5" generally to two years prior
#' to current year, and for "acs1" generally to one year prior.
#' @return A single, \pkg{sf}-formatted `data.frame` with geometries, and
#' columns for the five main variables.
#' @export

hs_get_census_data <- function (state = "AZ", year = 2022, survey = "acs5") {

    # -------- Start Assertions
    check_census_api_key ()
    state <- check_state_code (state)
    checkmate::assert_numeric (year, len = 1L)
    if (!is.integer (year)) year <- as.integer (year)
    checkmate::assert_character (survey, len = 1L, n.chars = 4L)
    if (!survey %in% c ("acs1", "acs5")) {
        stop ("'survey' must be one of 'acs1' or 'acs5'", call. = FALSE)
    }
    if (survey == "acs1") {
        stop ("'acs1' data are not currently supported", call. = FALSE)
    }

    stopifnot (year >= 2009)
    this_year <- as.integer (substring (Sys.time (), 1, 4))
    stopifnot (year <= this_year)
    # -------- End Assertions

    occupancy <- hs_get_occupancy (state, year, survey)
    poverty <- hs_get_poverty_rate (state, year, survey)
    unemployment <- hs_get_unemployment (state, year, survey)
    no_hs <- hs_get_no_hs (state, year, survey)
    prop_deps <- hs_prop_deps (state, year, survey)
    income <- hs_per_capita_income (state, year, survey)

    res <- dplyr::left_join (occupancy, poverty, by = c ("GEOID", "NAME")) |>
        dplyr::left_join (unemployment, by = c ("GEOID", "NAME")) |>
        dplyr::left_join (no_hs, by = c ("GEOID", "NAME")) |>
        dplyr::left_join (prop_deps, by = c ("GEOID", "NAME")) |>
        dplyr::left_join (income, by = c ("GEOID", "NAME"))

    return (res)
}

#' Proportion of rooms with > 1 occupant
#'
#' @noRd
hs_get_occupancy <- function (state, year, survey) {

    measure <- NULL # suppress no visible binding note
    code <- "B25014"
    codes <- paste0 (code, "_", sprintf ("%03i", c (2, 5:8, 11:13)))
    code_totals <- unique (grep ("00(2|8)$", codes, value = TRUE))

    ret <- hs_get_one_census_data (
        state = state,
        year = year,
        codes = codes,
        code_totals = code_totals,
        survey = survey
    ) |>
        dplyr::rename (occupancy = measure)

    cli::cli_alert_success (cli::col_green (" [1/6] Obtained occpuancy data"))

    return (ret)
}

#'  Households below poverty
#'
#' @noRd
hs_get_poverty_rate <- function (state, year, survey) {

    measure <- NULL # suppress no visible binding note
    code <- "C17002"
    codes <- paste0 (code, "_", sprintf ("%03i", 1:3))
    code_totals <- grep ("001$", codes, value = TRUE)

    ret <- hs_get_one_census_data (
        state = state,
        year = year,
        codes = codes,
        code_totals = code_totals,
        survey = survey
    ) |>
        dplyr::rename (poverty = measure)

    cli::cli_alert_success (cli::col_green (" [2/6] Obtained poverty data"))

    return (ret)
}

#' Unemployment for those >= 16
#'
#' @noRd
hs_get_unemployment <- function (state, year, survey) {

    measure <- NULL # suppress no visible binding note
    code <- "B23027"
    codes <- paste0 (
        code, "_",
        sprintf ("%03i", c (2, 6:7, 11:12, 16:17, 21:22, 26))
    )
    code_totals <- paste0 (code, "_", sprintf ("%03i", c (2, 7, 12, 17, 22)))

    ret <- hs_get_one_census_data (
        state = state,
        year = year,
        codes = codes,
        code_totals = code_totals,
        survey = survey
    ) |>
        dplyr::rename (unemployment = measure)

    cli::cli_alert_success (cli::col_green (" [3/6] Obtained employment data"))

    return (ret)
}

#' Proportion without highschool diploma
#'
#' @noRd
hs_get_no_hs <- function (state, year, survey) {

    measure <- NULL # suppress no visible binding note
    code <- "B15003"
    codes <- paste0 (code, "_", sprintf ("%03i", 1:16))
    code_totals <- grep ("001$", codes, value = TRUE)

    ret <- hs_get_one_census_data (
        state = state,
        year = year,
        codes = codes,
        code_totals = code_totals,
        survey = survey
    ) |>
        dplyr::rename (no_hs = measure)

    cli::cli_alert_success (cli::col_green (" [4/6] Obtained education data"))

    return (ret)
}

#' Proportion of population < 16 & > 64
#'
#' Actually includes 15-17 bracket
#' @noRd
hs_prop_deps <- function (state, year, survey) {

    measure <- NULL # suppress no visible binding note
    code <- "B01001"
    codes <- paste0 (
        code, "_",
        sprintf ("%03i", c (1, 3:6, 20:25, 27:30, 44:49))
    )
    code_totals <- grep ("001$", codes, value = TRUE)

    ret <- hs_get_one_census_data (
        state = state,
        year = year,
        codes = codes,
        code_totals = code_totals,
        survey = survey
    ) |>
        dplyr::rename (deps = measure)

    cli::cli_alert_success (cli::col_green (" [5/6] Obtained dependents data"))

    return (ret)
}

#' Per-capita income
#'
#' @noRd
hs_per_capita_income <- function (state, year, survey) {

    # suppress no visible binding notes:
    estimate <- GEOID <- NAME <- income <- geometry <- NULL
    code <- "B19301_001"
    ret <- hs_get_one_census_data_simple (
        state = state,
        year = year,
        code,
        geometry = TRUE,
        survey = survey
    ) |>
        dplyr::rename (income = estimate) |>
        dplyr::select (GEOID, NAME, income, geometry)

    cli::cli_alert_success (cli::col_green (" [6/6] Obtained income data"))

    return (ret)
}

hs_get_one_census_data <- function (state = "AZ", year = 2022,
                                    codes, code_totals, survey) {

    suppressMessages ({
        x <- tidycensus::get_acs (
            geography = "block group",
            variables = codes,
            state = state,
            geometry = FALSE,
            year = year,
            survey = survey
        )
    })

    # Suppress no visible binding notes:
    variable <- GEOID <- NAME <- total <-
        estimate <- variable <- measure <- measure_sum <- NULL

    x_tots <- dplyr::filter (x, variable %in% code_totals) |>
        dplyr::group_by (GEOID) |>
        dplyr::summarise (NAME = unique (NAME), total = sum (estimate))
    x_pars <- dplyr::filter (x, !variable %in% code_totals) |>
        dplyr::group_by (GEOID) |>
        dplyr::summarise (NAME = unique (NAME), measure_sum = sum (estimate))
    x_tots <- dplyr::left_join (x_tots, x_pars, by = c ("GEOID", "NAME")) |>
        dplyr::mutate (measure = measure_sum / total) |>
        dplyr::select (GEOID, NAME, measure)

    return (x_tots)
}

hs_get_one_census_data_simple <- function (state = "AZ", year = 2022,
                                           code, geometry = FALSE, survey) {

    suppressMessages ({
        tidycensus::get_acs (
            geography = "block group",
            variables = code,
            state = state,
            geometry = geometry,
            year = year,
            survey = survey
        )
    })
}

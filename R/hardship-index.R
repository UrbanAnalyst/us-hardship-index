#' Use raw census data to calculate compound hardship index
#'
#' @inheritParams hs_get_census_data
#' @export
hs_hardship_index <- function (state = "AZ", year = 2022, survey = "acs5") {

    dat <- m_hs_get_census_data (state = state, year = year, survey = survey)

    return (dat)
}

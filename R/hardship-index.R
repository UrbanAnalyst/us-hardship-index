#' Use raw census data to calculate compound hardship index
#'
#' The index is calculated following the methodology of "An Update on Urban
#' Hardship," by Lisa M. Monteil, Richard P. Nathan, and David J. Wright (2004),
#' The Nelson A. Rockefeller Institute of Goverment. It is formed as a multiple
#' of the following six measures, each standardised to unit (or percentage)
#' scales:
#'
#' \itemize{
#'   \item \code{occupancy}: Proportion of rooms with > 1 occupant per room;
#'   \item \code{poverty}: Proportion of households below poverty line;
#'   \item \code{unemployment}: Proportion of unemployed adults;
#'   \item \code{no_hs}: Proportion of population without highschool diploma
#'   \item \code{deps}:  Proportion of population who may be considered
#'   dependent; that is either under 18 or over 65
#'   \item \code{income}: Per-capita income
#' }
#'
#' All variables are quantified such that lower values are better, except for
#' income.
#'
#' @inheritParams hs_get_census_data
#' @export
hs_hardship_index <- function (state = "AZ", year = 2022, survey = "acs5") {

    dat <- m_hs_get_census_data (state = state, year = year, survey = survey)

    dat$income <- max (dat$income, na.rm = TRUE) - dat$income
    vars <- c ("occupancy", "poverty", "unemployment", "no_hs", "deps", "income")
    for (v in vars) {
        vmin <- min (dat [[v]], na.rm = TRUE)
        vmax <- max (dat [[v]], na.rm = TRUE)
        dat [[v]] <- (dat [[v]] - vmin) / (vmax - vmin)
    }

    dat_mat <- as.matrix (dat [, vars])
    dat$hardship <- apply (dat_mat, 1, prod)
    dat$hardship <- dat$hardship / max (dat$hardship, na.rm = TRUE)

    return (dat)
}

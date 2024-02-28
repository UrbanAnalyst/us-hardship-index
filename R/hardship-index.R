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

    # log-transform all data:
    nms <- c ("GEOID", "NAME", "geometry")
    vars <- names (dat) [which (!names (dat) %in% nms)]

    for (v in vars) {
        dat [[v]] <- log10 (dat [[v]])
        dat [[v]] [which (!is.finite (dat [[v]]))] <- NA
        vmin <- min (dat [[v]], na.rm = TRUE)
        vmax <- max (dat [[v]], na.rm = TRUE)
        dat [[v]] <- (dat [[v]] - vmin) / (vmax - vmin)
    }

    # Then impute any missing values:
    suppressWarnings (
        xy_cent <- sf::st_as_sf (dat) |>
            sf::st_transform (crs = 4326) |>
            sf::st_centroid () |>
            sf::st_coordinates ()
    )

    index <- which (!is.na (xy_cent [, 1]) & !is.na (xy_cent [, 2]))
    xy_cent <- xy_cent [index, ]
    dat <- dat [index, ]

    # Cast to polygons:
    dat <- sf::st_as_sf (dat)
    dat <- sf::st_cast (dat, "POLYGON")
    index <- which (duplicated (dat$GEOID))
    if (length (index) > 0L) {
        dat <- dat [-index, ]
    }

    # replace any NA values with weighted neighbour values:
    cli::cli_alert_info (cli::col_yellow ("Imputing missing values ..."))
    pb <- utils::txtProgressBar ()
    count <- 1
    for (v in vars) {
        utils::setTxtProgressBar (pb, count / length (vars))
        count <- count + 1
        index_NA <- which (is.na (dat [[v]]))
        index <- which (!is.na (dat [[v]]))
        dmin <- geodist::geodist (xy_cent [index_NA, ], xy_cent [index, ], measure = "haversine")
        d_wts <- exp (-dmin / 1000)
        d_wt_sums <- array (rowSums (d_wts), dim = t (dim (dmin)))
        d_wts <- d_wts / d_wt_sums
        dat_wt <- array (dat [[v]] [index], dim = t (dim (dmin)))
        dat [[v]] [index_NA] <- rowSums (d_wts * dat_wt, na.rm = TRUE)
    }
    close (pb)
    cli::cli_alert_success (cli::col_yellow ("Imputed missing values"))

    dat_mat <- as.matrix (dat [, vars])
    dat$hardship <- apply (dat_mat, 1, prod)
    dat$hardship <- dat$hardship / max (dat$hardship, na.rm = TRUE)

    return (dat)
}

# nocov start
# Set tigris_use_cache = TRUE to cache geodata for tidycensus calls with
# `geometry = TRUE`.
.onLoad <- function (libname, pkgname) { # nolint

    uc <- options ("tigris_use_cache")
    f <- tempfile (pattern = "tigris_cache")
    saveRDS (uc, f)

    options (tigris_use_cache = TRUE)
}

.onUnload <- function (libname, pkgname) { # nolint

    f <- list.files (tempdir (), pattern = "tigris_cache", full.names = TRUE)
    uc <- readRDS (f)
    options (tigris_use_cache = uc$tigris_use_cache)
    tryCatch (
        file.remove (f),
        error = function (e) NULL
    )
}
# nocov end

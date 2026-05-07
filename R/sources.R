# Source-specific get and list functions.
# Each vs_get_<source>() is a named wrapper over vs_get() that tags the result
# with the source label, enabling the print method and vs_plot() caption.

.vs_get_source <- function(name, source, start = NULL, end = NULL,
                            limit = NULL, base_url = VSW_BASE_URL) {
  df <- vs_get(name, start = start, end = end, limit = limit, base_url = base_url)
  attr(df, "vs_source") <- source
  df
}

.vs_list_source <- function(source) {
  df <- vs_list()
  df[!is.na(df$source) & df$source == source, ]
}


# ---------------------------------------------------------------------------
# Stats NZ
# ---------------------------------------------------------------------------

#' Fetch a Stats NZ series
#'
#' A named wrapper over [vs_get()] that tags the result with the
#' "Stats NZ" source label.
#'
#' @param name Dataset identifier, e.g. `"nz_cpi"`.
#' @param start ISO date lower bound, e.g. `"2020-01-01"`. Optional.
#' @param end   ISO date upper bound, e.g. `"2024-12-31"`. Optional.
#' @param limit Max rows to return. Default `NULL` requests the full dataset
#'   (server enforces a 50,000-row cap on Free/Starter plans; Pro is unlimited).
#'   Pass an integer to request fewer rows.
#' @return A `vs_series` data frame.
#' @export
#' @examples
#' \dontrun{
#' vs_key("vs_your_key")
#' df <- vs_get_statsnz("nz_cpi", start = "2015-01-01")
#' vs_plot(df)
#' }
vs_get_statsnz <- function(name, start = NULL, end = NULL, limit = NULL) {
  .vs_get_source(name, "Stats NZ", start = start, end = end, limit = limit)
}

#' List all Stats NZ series
#' @return A data frame (tibble if available) of series metadata.
#' @export
vs_list_statsnz <- function() .vs_list_source("Stats NZ")


# ---------------------------------------------------------------------------
# OECD
# ---------------------------------------------------------------------------

#' Fetch an OECD series
#' @inheritParams vs_get_statsnz
#' @export
vs_get_oecd <- function(name, start = NULL, end = NULL, limit = NULL) {
  .vs_get_source(name, "OECD", start = start, end = end, limit = limit)
}

#' List all OECD series
#' @export
vs_list_oecd <- function() .vs_list_source("OECD")


# ---------------------------------------------------------------------------
# RBNZ
# ---------------------------------------------------------------------------

#' Fetch an RBNZ series
#' @inheritParams vs_get_statsnz
#' @export
vs_get_rbnz <- function(name, start = NULL, end = NULL, limit = NULL) {
  .vs_get_source(name, "RBNZ", start = start, end = end, limit = limit)
}

#' List all RBNZ series
#' @export
vs_list_rbnz <- function() .vs_list_source("RBNZ")


# ---------------------------------------------------------------------------
# NZ Treasury
# ---------------------------------------------------------------------------

#' Fetch a NZ Treasury series
#' @inheritParams vs_get_statsnz
#' @export
vs_get_treasury <- function(name, start = NULL, end = NULL, limit = NULL) {
  .vs_get_source(name, "NZ Treasury", start = start, end = end, limit = limit)
}

#' List all NZ Treasury series
#' @export
vs_list_treasury <- function() .vs_list_source("NZ Treasury")


# ---------------------------------------------------------------------------
# LINZ
# ---------------------------------------------------------------------------

#' Fetch a LINZ series
#' @inheritParams vs_get_statsnz
#' @export
vs_get_linz <- function(name, start = NULL, end = NULL, limit = NULL) {
  .vs_get_source(name, "LINZ", start = start, end = end, limit = limit)
}

#' List all LINZ series
#' @export
vs_list_linz <- function() .vs_list_source("LINZ")


# ---------------------------------------------------------------------------
# Stats NZ Geospatial
# ---------------------------------------------------------------------------

#' Fetch a Stats NZ Geospatial dataset
#' @inheritParams vs_get_statsnz
#' @export
vs_get_statsnz_geo <- function(name, start = NULL, end = NULL, limit = NULL) {
  .vs_get_source(name, "Stats NZ Geospatial", start = start, end = end, limit = limit)
}

#' List all Stats NZ Geospatial datasets
#' @export
vs_list_statsnz_geo <- function() .vs_list_source("Stats NZ Geospatial")


# ---------------------------------------------------------------------------
# MBIE
# ---------------------------------------------------------------------------

#' Fetch an MBIE dataset
#' @inheritParams vs_get_statsnz
#' @export
vs_get_mbie <- function(name, start = NULL, end = NULL, limit = NULL) {
  .vs_get_source(name, "MBIE", start = start, end = end, limit = limit)
}

#' List all MBIE datasets
#' @export
vs_list_mbie <- function() .vs_list_source("MBIE")


# ---------------------------------------------------------------------------
# Waka Kotahi (NZTA)
# ---------------------------------------------------------------------------

#' Fetch a Waka Kotahi (NZTA) dataset
#' @inheritParams vs_get_statsnz
#' @export
vs_get_nzta <- function(name, start = NULL, end = NULL, limit = NULL) {
  .vs_get_source(name, "Waka Kotahi", start = start, end = end, limit = limit)
}

#' List all Waka Kotahi datasets
#' @export
vs_list_nzta <- function() .vs_list_source("Waka Kotahi")


# ---------------------------------------------------------------------------
# MSD
# ---------------------------------------------------------------------------

#' Fetch an MSD dataset
#' @inheritParams vs_get_statsnz
#' @export
vs_get_msd <- function(name, start = NULL, end = NULL, limit = NULL) {
  .vs_get_source(name, "MSD", start = start, end = end, limit = limit)
}

#' List all MSD datasets
#' @export
vs_list_msd <- function() .vs_list_source("MSD")


# ---------------------------------------------------------------------------
# NZ Police / MoJ
# ---------------------------------------------------------------------------

#' Fetch an NZ Police / MoJ dataset
#' @inheritParams vs_get_statsnz
#' @export
vs_get_police <- function(name, start = NULL, end = NULL, limit = NULL) {
  .vs_get_source(name, "NZ Police / MoJ", start = start, end = end, limit = limit)
}

#' List all NZ Police / MoJ datasets
#' @export
vs_list_police <- function() .vs_list_source("NZ Police / MoJ")

# Source-specific get and list functions.
# Each vs_get_<source>() is a named wrapper over vs_get() that tags the result
# with the source label, enabling the print method and vs_plot() caption.

.vs_get_source <- function(name, source, start = NULL, end = NULL,
                            base_url = VSW_BASE_URL) {
  df <- vs_get(name, start = start, end = end, base_url = base_url)
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
#' @param name Series identifier, e.g. `"nz_cpi"`.
#' @param start ISO date lower bound, e.g. `"2020-01-01"`. Optional.
#' @param end   ISO date upper bound, e.g. `"2024-12-31"`. Optional.
#' @return A `vs_series` data frame.
#' @export
#' @examples
#' \dontrun{
#' vs_key("vs_your_key")
#' df <- vs_get_statsnz("nz_cpi", start = "2015-01-01")
#' vs_plot(df)
#' }
vs_get_statsnz <- function(name, start = NULL, end = NULL) {
  .vs_get_source(name, "Stats NZ", start = start, end = end)
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
vs_get_oecd <- function(name, start = NULL, end = NULL) {
  .vs_get_source(name, "OECD", start = start, end = end)
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
vs_get_rbnz <- function(name, start = NULL, end = NULL) {
  .vs_get_source(name, "RBNZ", start = start, end = end)
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
vs_get_treasury <- function(name, start = NULL, end = NULL) {
  .vs_get_source(name, "NZ Treasury", start = start, end = end)
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
vs_get_linz <- function(name, start = NULL, end = NULL) {
  .vs_get_source(name, "LINZ", start = start, end = end)
}

#' List all LINZ series
#' @export
vs_list_linz <- function() .vs_list_source("LINZ")

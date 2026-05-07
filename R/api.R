#' List available series
#'
#' Returns a tibble (or data frame) with one row per series, including name,
#' title, source, namespace, and description.
#'
#' @param source Optional source filter, e.g. `"Stats NZ"` or `"OECD"`.
#'   Use [vs_list_statsnz()], [vs_list_oecd()] etc. as convenient shortcuts.
#' @param base_url Override the API base URL (useful for testing).
#' @return A tibble (if the `tibble` package is installed) or a `data.frame`.
#' @export
#' @examples
#' \dontrun{
#' vs_key("vs_your_key")
#' vs_list()
#' vs_list("Stats NZ")
#' }
vs_list <- function(source = NULL, base_url = VSW_BASE_URL) {
  resp <- vsw_get("/v1/series", base_url = base_url)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  df   <- as.data.frame(body$series %||% body)

  if (!is.null(source)) {
    df <- df[!is.na(df$source) & df$source == source, ]
    rownames(df) <- NULL
  }

  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(df) else df
}


#' Get metadata for a single series
#'
#' @param name Series identifier, e.g. `"nz_cpi"`.
#' @param base_url Override the API base URL (useful for testing).
#' @return A named list with series metadata.
#' @export
#' @examples
#' \dontrun{
#' vs_key("vs_your_key")
#' vs_info("nz_cpi")
#' }
vs_info <- function(name, base_url = VSW_BASE_URL) {
  resp <- vsw_get(paste0("/v1/series/", name), base_url = base_url)
  httr2::resp_body_json(resp)
}


#' Fetch time-series data
#'
#' The generic workhorse — use [vs_get_statsnz()], [vs_get_oecd()] etc. for
#' source-tagged results and a nicer print output.
#'
#' @param name Series identifier, e.g. `"nz_cpi"`.
#' @param start ISO date lower bound, e.g. `"2020-01-01"`. Optional.
#' @param end   ISO date upper bound, e.g. `"2024-12-31"`. Optional.
#' @param base_url Override the API base URL (useful for testing).
#' @return A `vs_series` data frame with `date` coerced to `Date`.
#' @export
#' @examples
#' \dontrun{
#' vs_key("vs_your_key")
#' df <- vs_get("nz_cpi", start = "2020-01-01")
#' vs_plot(df)
#' }
vs_get <- function(name, start = NULL, end = NULL, limit = NULL,
                   as_sf = NULL, base_url = VSW_BASE_URL) {
  # Server-side: limit=0 means "as many rows as your plan allows" (50,000 cap on
  # Free/Starter, unlimited on Pro). NULL on the R side maps to limit=0.
  params <- list()
  if (!is.null(start)) params$start <- start
  if (!is.null(end))   params$end   <- end
  params$limit <- if (is.null(limit)) 0L else as.integer(limit)

  resp <- do.call(vsw_get,
    c(list(paste0("/v1/series/", name, "/data"), base_url = base_url), params))
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  df   <- as.data.frame(body$data %||% body)

  if ("date" %in% names(df)) df$date <- as.Date(df$date)

  result <- new_vs_series(df, name = name)

  # Optional sf conversion. as_sf=NULL auto-converts when (a) geometry_wkt is
  # present AND (b) the sf package is installed. as_sf=TRUE forces and errors
  # if sf is missing; as_sf=FALSE keeps the raw WKT string column.
  if (!isFALSE(as_sf) && "geometry_wkt" %in% names(result)) {
    result <- .vs_to_sf(result, force = isTRUE(as_sf))
  }

  result
}


# Internal: convert a data frame with a geometry_wkt column to an sf object
# (CRS = WGS84 / EPSG:4326). Returns the original df if sf isn't installed and
# `force` is FALSE; errors if `force` is TRUE.
.vs_to_sf <- function(df, force = FALSE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    if (force) {
      stop("The 'sf' package is required to return geospatial datasets as sf ",
           "objects. Install with: install.packages('sf')", call. = FALSE)
    }
    return(df)
  }
  # Preserve vs_series-style metadata so source/name attrs survive the conversion
  vs_name   <- attr(df, "vs_name")
  vs_source <- attr(df, "vs_source")
  # Convert to plain data.frame first — sf::st_as_sf doesn't reliably drop the
  # WKT column when called on a class-extended data frame (e.g. vs_series).
  plain <- as.data.frame(df)
  result <- sf::st_as_sf(plain, wkt = "geometry_wkt", crs = 4326)
  # st_as_sf with wkt= converts the column in place, keeping its name. Rename
  # to "geometry" for consistency with the rest of the sf ecosystem.
  if (identical(attr(result, "sf_column"), "geometry_wkt")) {
    names(result)[names(result) == "geometry_wkt"] <- "geometry"
    attr(result, "sf_column") <- "geometry"
  }
  if (!is.null(vs_name))   attr(result, "vs_name")   <- vs_name
  if (!is.null(vs_source)) attr(result, "vs_source") <- vs_source
  result
}

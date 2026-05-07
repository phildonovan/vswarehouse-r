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
                   base_url = VSW_BASE_URL) {
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

  new_vs_series(df, name = name)
}

#' List all available series
#'
#' Returns a data frame with one row per series, including name, title,
#' source, namespace, and description.
#'
#' @param base_url Override the API base URL (useful for testing).
#' @return A `data.frame`.
#' @export
#' @examples
#' \dontrun{
#' vs_key("vs_your_key")
#' series <- vs_list()
#' head(series)
#' }
vs_list <- function(base_url = VSW_BASE_URL) {
  resp <- vsw_get("/v1/series", base_url = base_url)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  as.data.frame(body$series %||% body)
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


#' Fetch time-series data as a data frame
#'
#' @param name Series identifier, e.g. `"nz_cpi"`.
#' @param start ISO date lower bound, e.g. `"2020-01-01"`. Optional.
#' @param end ISO date upper bound, e.g. `"2024-12-31"`. Optional.
#' @param base_url Override the API base URL (useful for testing).
#' @return A `data.frame` with columns `date`, `period`, `value`, and any
#'   extra dimension columns. `date` is coerced to `Date`.
#' @export
#' @examples
#' \dontrun{
#' vs_key("vs_your_key")
#' df <- vs_get("nz_cpi", start = "2020-01-01")
#' plot(df$date, df$value, type = "l")
#' }
vs_get <- function(name, start = NULL, end = NULL, base_url = VSW_BASE_URL) {
  params <- list()
  if (!is.null(start)) params$start <- start
  if (!is.null(end))   params$end   <- end

  resp <- do.call(vsw_get, c(list(paste0("/v1/series/", name, "/data"), base_url = base_url), params))
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  df <- as.data.frame(body$data %||% body)

  if ("date" %in% names(df)) {
    df$date <- as.Date(df$date)
  }
  df
}

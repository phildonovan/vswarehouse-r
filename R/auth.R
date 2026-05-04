# Package-level environment to hold the API key
.vsw_env <- new.env(parent = emptyenv())

#' Set your vs-warehouse API key
#'
#' Stores the key for the duration of the R session. Alternatively, set the
#' `VS_API_KEY` environment variable and omit this call entirely.
#'
#' @param key A `vs_...` API key from <https://api.virtus-solutions.io>.
#' @return The key, invisibly.
#' @export
#' @examples
#' \dontrun{
#' vs_key("vs_your_key_here")
#' }
vs_key <- function(key) {
  .vsw_env$key <- key
  invisible(key)
}

vsw_get_key <- function() {
  key <- .vsw_env$key %||% Sys.getenv("VS_API_KEY", unset = "")
  if (nchar(key) == 0) {
    stop(
      "No API key found. Call vs_key(\"vs_...\") or set the VS_API_KEY ",
      "environment variable. Get a free key at https://api.virtus-solutions.io",
      call. = FALSE
    )
  }
  key
}

`%||%` <- function(x, y) if (!is.null(x)) x else y

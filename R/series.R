# vs_series S3 class — a data frame with name/source metadata attached

new_vs_series <- function(df, name, source = NULL) {
  structure(df,
    vs_name   = name,
    vs_source = source,
    class     = c("vs_series", class(df))
  )
}

#' @export
print.vs_series <- function(x, ...) {
  name   <- attr(x, "vs_name")
  source <- attr(x, "vs_source")
  header <- paste0("# vs_series: ", name)
  if (!is.null(source) && nchar(source) > 0)
    header <- paste0(header, " [", source, "]")
  cat(header, "\n")
  cat(sprintf("# %d rows\n", nrow(x)))
  class(x) <- setdiff(class(x), "vs_series")
  print(x, ...)
  invisible(x)
}


#' Quick line plot for a vs_series
#'
#' A thin ggplot2 wrapper that returns a `ggplot` object — add further layers
#' with `+` as usual.
#'
#' @param x A `vs_series` returned by any `vs_get_*()` function.
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' vs_key("vs_your_key")
#' df <- vs_get_statsnz("nz_cpi", start = "2015-01-01")
#' vs_plot(df)
#' }
vs_plot <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required. Install with install.packages('ggplot2')", call. = FALSE)

  name   <- attr(x, "vs_name")   %||% "Series"
  source <- attr(x, "vs_source") %||% ""

  date_col  <- if ("date"  %in% names(x)) "date"  else names(x)[1]
  value_col <- if ("value" %in% names(x)) "value" else names(x)[2]

  caption <- if (nchar(source) > 0)
    paste0("Source: ", source, " · api.virtus-solutions.io")
  else
    "api.virtus-solutions.io"

  ggplot2::ggplot(x, ggplot2::aes(x = .data[[date_col]], y = .data[[value_col]])) +
    ggplot2::geom_line(colour = "#2563eb", linewidth = 0.8) +
    ggplot2::labs(
      title   = name,
      x       = NULL,
      y       = NULL,
      caption = caption
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(colour = "#9ca3af", size = 9)
    )
}

library(testthat)
library(httr2)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

httr2_mock_resp <- function(body, status = 200L) {
  structure(
    list(
      method = "GET",
      url = "https://api.virtus-solutions.io/test",
      status_code = status,
      headers = structure(list(`content-type` = "application/json"), class = "httr2_headers"),
      body = charToRaw(body),
      cache = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
}

# Mock vsw_perform (low-level) so error-checking logic still runs
with_mock_vsw <- function(body, status = 200L, code) {
  ns <- getNamespace("vswarehouse")
  # Set a key so vsw_get_key() doesn't error
  assign("key", "vs_testkey", envir = ns$.vsw_env)
  local_mocked_bindings(
    vsw_perform = function(...) httr2_mock_resp(body, status),
    .env = ns
  )
  code
}

# ---------------------------------------------------------------------------
# vs_key / auth
# ---------------------------------------------------------------------------

test_that("vs_key returns key invisibly", {
  result <- withVisible(vs_key("vs_testkey"))
  expect_false(result$visible)
  expect_equal(result$value, "vs_testkey")
})

test_that("vsw_get_key reads VS_API_KEY env var when no session key", {
  ns <- getNamespace("vswarehouse")
  assign("key", NULL, envir = ns$.vsw_env)
  withr::with_envvar(c(VS_API_KEY = "vs_from_env"), {
    expect_equal(ns$vsw_get_key(), "vs_from_env")
  })
})

test_that("vsw_get_key errors when no key set anywhere", {
  ns <- getNamespace("vswarehouse")
  assign("key", NULL, envir = ns$.vsw_env)
  withr::with_envvar(c(VS_API_KEY = ""), {
    expect_error(ns$vsw_get_key(), "No API key found")
  })
})

# ---------------------------------------------------------------------------
# vs_list
# ---------------------------------------------------------------------------

test_that("vs_list returns a data frame", {
  with_mock_vsw('{"series":[{"name":"nz_cpi","title":"NZ CPI","source":"OECD"}]}', code = {
    result <- vs_list()
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1L)
    expect_equal(result$name, "nz_cpi")
  })
})

# ---------------------------------------------------------------------------
# vs_info
# ---------------------------------------------------------------------------

test_that("vs_info returns a list", {
  with_mock_vsw('{"name":"nz_cpi","title":"NZ Consumer Price Index","source":"OECD"}', code = {
    result <- vs_info("nz_cpi")
    expect_type(result, "list")
    expect_equal(result$name, "nz_cpi")
  })
})

# ---------------------------------------------------------------------------
# vs_get
# ---------------------------------------------------------------------------

test_that("vs_get returns a data frame with Date column", {
  body <- '{"data":[{"date":"2023-01-01","period":"2023Q1","value":100.0},{"date":"2023-04-01","period":"2023Q2","value":101.5}]}'
  with_mock_vsw(body, code = {
    df <- vs_get("nz_cpi")
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 2L)
    expect_s3_class(df$date, "Date")
    expect_equal(df$value, c(100.0, 101.5))
  })
})

# ---------------------------------------------------------------------------
# Error handling
# ---------------------------------------------------------------------------

test_that("401 raises authentication error", {
  with_mock_vsw('{"detail":"Unauthorised"}', status = 401L, code = {
    expect_error(vs_get("nz_cpi"), "Authentication error")
  })
})

test_that("429 raises rate limit error", {
  with_mock_vsw('{"detail":"Daily limit"}', status = 429L, code = {
    expect_error(vs_get("nz_cpi"), "Rate limit")
  })
})

test_that("404 raises not found error", {
  with_mock_vsw('{"detail":"Series not found"}', status = 404L, code = {
    expect_error(vs_get("bad_series"), "Not found")
  })
})

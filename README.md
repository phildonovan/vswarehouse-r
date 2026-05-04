# vswarehouse <img src="https://img.shields.io/badge/R-package-blue" align="right"/>

R client for the [vs-warehouse](https://api.virtus-solutions.io) statistical data API — macro and economic time series for New Zealand, Australia, and more.

## Installation

```r
# Install from GitHub (until CRAN submission)
remotes::install_github("phildonovan/vswarehouse-r")
```

## Quick start

```r
library(vswarehouse)

vs_key("vs_your_api_key")  # or set VS_API_KEY env var

# List all available series
series <- vs_list()

# Get metadata for a series
vs_info("nz_cpi")

# Fetch data as a data frame
df <- vs_get("nz_cpi", start = "2020-01-01")
plot(df$date, df$value, type = "l", main = "NZ CPI")
```

## Authentication

```r
vs_key("vs_your_key")   # session-scoped
```

Or set permanently in `.Renviron`:

```
VS_API_KEY=vs_your_key
```

Get a free API key at [api.virtus-solutions.io](https://api.virtus-solutions.io).

## Functions

| Function | Returns | Description |
|---|---|---|
| `vs_key(key)` | key (invisibly) | Store API key for the session |
| `vs_list()` | `data.frame` | All available series with metadata |
| `vs_info(name)` | `list` | Metadata for a single series |
| `vs_get(name, start, end)` | `data.frame` | Time-series data |

## License

MIT

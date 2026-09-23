cran_packages <- c(
  "shiny", "bslib", "dplyr", "DT", "lubridate", "readr", "stringr",
  "tibble", "googlesheets4", "nflreadr", "openssl", "remotes", "testthat", "withr"
)

missing <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

if (utils::packageVersion("bslib") < "0.9.0") {
  install.packages("bslib", repos = "https://cloud.r-project.org")
}

if (!requireNamespace("oddsapiR", quietly = TRUE) || utils::packageVersion("oddsapiR") < "1.0.1") {
  remotes::install_github("sportsdataverse/oddsapiR", upgrade = "never", dependencies = FALSE)
}

message("Setup complete. Add ODDS_API_KEY to ~/.Renviron, restart R, authorize Google Sheets, and run the app.")

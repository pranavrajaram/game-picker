default_sheet_url <- Sys.getenv("GOOGLE_SHEET_URL", unset = "")
if (!nzchar(default_sheet_url) && file.exists(".google-sheet-url")) {
  default_sheet_url <- trimws(readLines(".google-sheet-url", warn = FALSE, n = 1))
}
if (!nzchar(default_sheet_url)) {
  stop("GOOGLE_SHEET_URL is not configured. Add it as an environment variable before starting the app.")
}

current_nfl_season <- function(today = Sys.Date()) {
  year <- as.integer(format(today, "%Y"))
  month <- as.integer(format(today, "%m"))
  if (month <= 2) year - 1L else year
}

sheet_gid_from_url <- function(sheet_url) {
  match <- stringr::str_match(sheet_url, "(?:[?#&]gid=)([0-9]+)")
  if (is.na(match[1, 2])) NA_integer_ else as.integer(match[1, 2])
}

sheet_tab_from_url <- function(sheet_url) {
  gid <- sheet_gid_from_url(sheet_url)
  properties <- googlesheets4::sheet_properties(sheet_url)

  if (is.na(gid)) {
    return(properties$name[[1]])
  }

  matched <- properties$name[properties$id == gid]
  if (!length(matched)) {
    stop(
      "The URL's gid (", gid, ") was not found. Available tabs: ",
      paste(properties$name, collapse = ", ")
    )
  }
  matched[[1]]
}

read_rankings_source <- function(sheet_url = default_sheet_url, fallback_path = file.path("data", "rankings-fallback.csv")) {
  sheet_error <- NULL

  rankings <- tryCatch(
    {
      if (!googlesheets4::gs4_has_token()) {
        stop("Google Sheets is not authorized. Run googlesheets4::gs4_auth(cache = TRUE) once, then restart the app.")
      }
      read_google_rankings(sheet_url)
    },
    error = function(error) {
      sheet_error <<- conditionMessage(error)
      NULL
    }
  )

  if (!is.null(rankings)) {
    return(list(data = rankings, source = "Google Sheet", warning = NULL))
  }

  if (!file.exists(fallback_path)) {
    stop("Google Sheets could not be read and no local rankings fallback was found. ", sheet_error)
  }

  list(
    data = readr::read_csv(fallback_path, show_col_types = FALSE),
    source = "Local rankings.csv fallback",
    warning = paste("Google Sheets was unavailable:", sheet_error)
  )
}

connect_google_sheets <- function() {
  credential_path <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON_PATH", unset = "")
  if (!nzchar(credential_path) && file.exists(".service-account-path")) {
    credential_path <- trimws(readLines(".service-account-path", warn = FALSE, n = 1))
  }

  encoded_credential <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON_BASE64", unset = "")
  if (nzchar(encoded_credential)) {
    credential_path <- tempfile(fileext = ".json")
    writeBin(openssl::base64_decode(encoded_credential), credential_path)
  }

  if (nzchar(credential_path)) {
    credential_path <- path.expand(credential_path)
    if (!file.exists(credential_path)) stop("Google service-account JSON was not found at the configured path.")
    googlesheets4::gs4_auth(
      path = credential_path,
      scopes = "https://www.googleapis.com/auth/spreadsheets"
    )
    return(invisible(googlesheets4::gs4_has_token()))
  }

  googlesheets4::gs4_auth(
    scopes = "https://www.googleapis.com/auth/spreadsheets",
    cache = TRUE
  )
  invisible(googlesheets4::gs4_has_token())
}

read_google_rankings <- function(sheet_url = default_sheet_url) {
  if (!googlesheets4::gs4_has_token()) {
    stop("Google Sheets is not connected. Click Connect Google Sheet first.")
  }

  tab_name <- sheet_tab_from_url(sheet_url)
  rankings <- googlesheets4::read_sheet(ss = sheet_url, sheet = tab_name)
  attr(rankings, "sheet_tab") <- tab_name
  rankings
}

load_local_odds_key <- function(session_key = "") {
  if (nzchar(session_key)) {
    return(list(key = session_key, source = "session field"))
  }

  candidates <- unique(c(
    Sys.getenv("R_ENVIRON_USER", unset = ""),
    path.expand("~/.Renviron"),
    file.path(dirname(getwd()), ".Renviron"),
    file.path(getwd(), ".Renviron")
  ))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  for (path in candidates) readRenviron(path)

  key <- Sys.getenv("ODDS_API_KEY")
  source <- if (length(candidates)) tail(candidates, 1) else "R environment"
  list(key = key, source = source)
}

neutral_site_overrides <- function() {
  c(
    "2026_05_PHI_JAX",
    "2026_06_HOU_JAX",
    "2026_07_PIT_NO",
    "2026_09_CIN_ATL",
    "2026_10_NE_DET",
    "2026_11_MIN_SF"
  )
}

fetch_schedule <- function(season) {
  schedule <- nflreadr::load_schedules(season)
  if (nrow(schedule) == 0) {
    stop("The NFL schedule could not be downloaded.")
  }

  schedule |>
    dplyr::filter(.data$season == season, .data$game_type == "REG") |>
    dplyr::transmute(
      event_id = as.character(.data$game_id),
      season = .data$season,
      week = .data$week,
      kickoff = lubridate::ymd_hm(
        paste(.data$gameday, dplyr::coalesce(.data$gametime, "00:00")),
        tz = "America/New_York",
        quiet = TRUE
      ),
      away_team = as.character(.data$away_team),
      home_team = as.character(.data$home_team),
      neutral_site = dplyr::coalesce(.data$location == "Neutral", FALSE) |
        .data$game_id %in% neutral_site_overrides(),
      away_score = as.numeric(.data$away_score),
      home_score = as.numeric(.data$home_score),
      completed = !is.na(.data$away_score) & !is.na(.data$home_score),
      nflverse_closing_home_line = as.numeric(.data$spread_line)
    )
}

parse_odds <- function(raw, bookmaker_key = "fanduel") {
  selected <- raw |>
    dplyr::filter(
      .data$bookmaker_key == .env$bookmaker_key,
      .data$market_key == "spreads",
      .data$outcomes_name == .data$home_team
    ) |>
    dplyr::mutate(market_last_update = lubridate::ymd_hms(.data$market_last_update, quiet = TRUE)) |>
    dplyr::group_by(.data$id) |>
    dplyr::slice_max(.data$market_last_update, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      odds_event_id = .data$id,
      kickoff_odds = lubridate::ymd_hms(.data$commence_time, quiet = TRUE),
      away_team = canonical_team(.data$away_team),
      home_team = canonical_team(.data$home_team),
      market_home_line = as.numeric(.data$outcomes_point),
      market_price = as.numeric(.data$outcomes_price),
      market_updated = .data$market_last_update,
      bookmaker = .data$bookmaker
    )

  if (nrow(selected) == 0) {
    available <- sort(unique(raw$bookmaker_key))
    stop(
      "No ", bookmaker_key, " home spread rows were returned. Available books: ",
      paste(available, collapse = ", ")
    )
  }

  selected
}

fetch_odds <- function(bookmaker_key = "fanduel", api_key = NULL) {
  key <- if (!is.null(api_key) && nzchar(api_key)) api_key else Sys.getenv("ODDS_API_KEY")
  if (!nzchar(key)) {
    stop("No Odds API key was found. Enter it in the sidebar or add ODDS_API_KEY to ~/.Renviron, then click Refresh lines.")
  }

  raw <- withr::with_envvar(
    c(ODDS_API_KEY = key),
    oddsapiR::toa_sports_odds(
      sport_key = "americanfootball_nfl",
      regions = "us",
      markets = "spreads",
      odds_format = "american",
      date_format = "iso"
    )
  )

  selected <- parse_odds(raw, bookmaker_key)

  attr(selected, "quota") <- oddsapiR::toa_quota()
  selected
}

read_odds_cache <- function(bookmaker = "fanduel", cache_dir = "cache") {
  if (exists("read_shared_odds_cache", mode = "function")) {
    shared <- tryCatch(read_shared_odds_cache(), error = function(error) NULL)
    if (!is.null(shared) && nrow(shared)) return(shared)
  }

  path <- file.path(cache_dir, paste0("odds_", bookmaker, ".rds"))
  if (!file.exists(path)) return(NULL)

  cached <- readRDS(path)
  if (nrow(cached) == 0) return(NULL)
  attr(cached, "from_cache") <- TRUE
  attr(cached, "cache_saved_at") <- file.info(path)$mtime
  cached
}

fetch_odds_cached <- function(bookmaker = "fanduel", api_key = NULL, force = FALSE, cache_dir = "cache", max_age_minutes = 60) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cache_dir, paste0("odds_", bookmaker, ".rds"))

  if (!force) {
    cached <- read_odds_cache(bookmaker, cache_dir)
    saved_at <- if (is.null(cached)) NULL else attr(cached, "cache_saved_at")
    if (!is.null(saved_at) && difftime(Sys.time(), saved_at, units = "mins") < max_age_minutes) return(cached)
  }

  odds <- fetch_odds(bookmaker, api_key = api_key)
  saveRDS(odds, path)
  attr(odds, "from_cache") <- FALSE
  attr(odds, "cache_saved_at") <- Sys.time()
  if (exists("write_shared_odds_cache", mode = "function")) {
    try(write_shared_odds_cache(odds), silent = TRUE)
  }
  odds
}

join_schedule_and_odds <- function(schedule, odds) {
  odds_for_join <- odds |>
    dplyr::select(
      home_team,
      away_team,
      market_home_line,
      market_price,
      market_updated,
      bookmaker
    )

  schedule |>
    dplyr::mutate(
      home_team = canonical_team(.data$home_team),
      away_team = canonical_team(.data$away_team)
    ) |>
    dplyr::left_join(odds_for_join, by = c("home_team", "away_team"))
}

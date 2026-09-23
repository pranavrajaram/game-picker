storage_sheet_names <- c(
  picks = "App Picks",
  history = "Ranking History",
  odds = "Odds Cache"
)

shared_sheet_exists <- function(sheet_name) {
  sheet_name %in% googlesheets4::sheet_properties(default_sheet_url)$name
}

ensure_shared_sheet <- function(sheet_name) {
  if (!googlesheets4::gs4_has_token()) connect_google_sheets()
  if (!shared_sheet_exists(sheet_name)) {
    googlesheets4::sheet_add(default_sheet_url, sheet = sheet_name)
  }
  invisible(sheet_name)
}

read_shared_table <- function(sheet_name) {
  if (!googlesheets4::gs4_has_token()) connect_google_sheets()
  if (!shared_sheet_exists(sheet_name)) return(NULL)
  googlesheets4::read_sheet(default_sheet_url, sheet = sheet_name, .name_repair = "minimal")
}

write_shared_table <- function(sheet_name, data) {
  ensure_shared_sheet(sheet_name)
  googlesheets4::sheet_write(data, ss = default_sheet_url, sheet = sheet_name)
  invisible(data)
}

coerce_posix <- function(x, tz) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = tz))
  suppressWarnings(lubridate::ymd_hms(as.character(x), tz = tz, quiet = TRUE))
}

read_shared_tracked_picks <- function() {
  data <- read_shared_table(storage_sheet_names[["picks"]])
  if (is.null(data)) return(NULL)
  template <- empty_tracked_picks()
  if (!nrow(data)) return(template)
  missing <- setdiff(names(template), names(data))
  for (column in missing) data[[column]] <- template[[column]]
  data |>
    dplyr::transmute(
      event_id = as.character(.data$event_id), season = as.integer(.data$season), week = as.integer(.data$week),
      kickoff = coerce_posix(.data$kickoff, "America/New_York"), away_team = as.character(.data$away_team),
      home_team = as.character(.data$home_team), pick_team = as.character(.data$pick_team),
      pick_line = as.numeric(.data$pick_line), edge = as.numeric(.data$edge),
      model_home_line = as.numeric(.data$model_home_line), market_home_line = as.numeric(.data$market_home_line),
      away_rating = as.numeric(.data$away_rating), home_rating = as.numeric(.data$home_rating),
      bookmaker = as.character(.data$bookmaker), market_updated = coerce_posix(.data$market_updated, "UTC"),
      rankings_source = as.character(.data$rankings_source), tracked_at = coerce_posix(.data$tracked_at, "America/New_York")
    )
}

write_shared_tracked_picks <- function(data) {
  write_shared_table(storage_sheet_names[["picks"]], data)
}

read_shared_ranking_history <- function() {
  data <- read_shared_table(storage_sheet_names[["history"]])
  if (is.null(data)) return(NULL)
  if (!nrow(data)) return(empty_ranking_history())
  data |>
    dplyr::transmute(
      season = as.integer(.data$season), week = as.integer(.data$week), rank = as.integer(.data$rank),
      team = as.character(.data$team), rating = as.numeric(.data$rating), tier = as.character(.data$tier),
      source = as.character(.data$source), captured_at = coerce_posix(.data$captured_at, "America/New_York")
    )
}

write_shared_ranking_history <- function(data) {
  write_shared_table(storage_sheet_names[["history"]], data)
}

read_shared_odds_cache <- function() {
  data <- read_shared_table(storage_sheet_names[["odds"]])
  if (is.null(data) || !nrow(data)) return(NULL)
  saved_at <- coerce_posix(data$cache_saved_at[[1]], "UTC")
  result <- data |>
    dplyr::transmute(
      odds_event_id = as.character(.data$odds_event_id), kickoff_odds = coerce_posix(.data$kickoff_odds, "UTC"),
      away_team = as.character(.data$away_team), home_team = as.character(.data$home_team),
      market_home_line = as.numeric(.data$market_home_line), market_price = as.numeric(.data$market_price),
      market_updated = coerce_posix(.data$market_updated, "UTC"), bookmaker = as.character(.data$bookmaker)
    )
  attr(result, "from_cache") <- TRUE
  attr(result, "cache_saved_at") <- saved_at
  result
}

write_shared_odds_cache <- function(data) {
  saved_at <- attr(data, "cache_saved_at")
  if (is.null(saved_at)) saved_at <- Sys.time()
  payload <- dplyr::mutate(data, cache_saved_at = saved_at)
  write_shared_table(storage_sheet_names[["odds"]], payload)
}

ranking_history_path <- file.path("data", "ranking-history.rds")

empty_ranking_history <- function() {
  tibble::tibble(
    season = integer(),
    week = integer(),
    rank = integer(),
    team = character(),
    rating = numeric(),
    tier = character(),
    source = character(),
    captured_at = as.POSIXct(character(), tz = "America/New_York")
  )
}

read_ranking_history <- function(path = ranking_history_path) {
  if (exists("read_shared_ranking_history", mode = "function")) {
    shared <- tryCatch(read_shared_ranking_history(), error = function(error) NULL)
    if (!is.null(shared)) return(shared)
  }
  if (!file.exists(path)) return(empty_ranking_history())
  readRDS(path)
}

write_ranking_history <- function(history, path = ranking_history_path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(history, path)
  readr::write_csv(history, sub("\\.rds$", ".csv", path))
  if (exists("write_shared_ranking_history", mode = "function")) {
    write_shared_ranking_history(history)
  }
  invisible(history)
}

ranking_snapshot_week <- function(schedule, current_week, now = Sys.time()) {
  current_week <- as.integer(current_week)
  available_weeks <- sort(unique(as.integer(schedule$week)))
  week_games <- schedule |>
    dplyr::filter(.data$week == .env$current_week) |>
    dplyr::mutate(
      local_kickoff = lubridate::with_tz(.data$kickoff, "America/New_York"),
      local_date = as.Date(.data$local_kickoff),
      weekday = lubridate::wday(.data$local_kickoff)
    )

  sunday_dates <- week_games$local_date[week_games$weekday == 1]
  if (!length(sunday_dates)) return(current_week)

  sunday_cutoff <- as.POSIXct(
    paste(min(sunday_dates), "13:00:00"),
    tz = "America/New_York"
  )
  next_weeks <- available_weeks[available_weeks > current_week]

  if (as.POSIXct(now, tz = "America/New_York") >= sunday_cutoff && length(next_weeks)) {
    min(next_weeks)
  } else {
    current_week
  }
}

create_ranking_snapshot <- function(rankings, season, week, source) {
  raw <- rankings
  names(raw) <- stringr::str_to_lower(stringr::str_trim(names(raw)))
  team_column <- intersect(c("team", "teams"), names(raw))[1]
  tier_column <- intersect(c("tier", "tiers"), names(raw))[1]

  tiers <- if (is.na(tier_column)) {
    tibble::tibble(team = character(), tier = character())
  } else {
    raw |>
      dplyr::transmute(
        team = canonical_team(.data[[team_column]]),
        tier = as.character(.data[[tier_column]])
      ) |>
      dplyr::filter(!is.na(.data$team)) |>
      dplyr::distinct(.data$team, .keep_all = TRUE)
  }

  normalize_rankings(rankings) |>
    dplyr::select(.data$team, .data$rating) |>
    dplyr::left_join(tiers, by = "team") |>
    dplyr::arrange(dplyr::desc(.data$rating)) |>
    dplyr::mutate(
      season = as.integer(season),
      week = as.integer(week),
      rank = dplyr::row_number(),
      source = as.character(source),
      captured_at = Sys.time()
    ) |>
    dplyr::select(.data$season, .data$week, .data$rank, .data$team, .data$rating, .data$tier, .data$source, .data$captured_at)
}

recalculate_ranking_history_ranks <- function(history) {
  history |>
    dplyr::group_by(.data$season, .data$week) |>
    dplyr::arrange(dplyr::desc(.data$rating), .by_group = TRUE) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::ungroup()
}

upsert_ranking_snapshot <- function(history, snapshot) {
  history |>
    dplyr::filter(!(.data$season == snapshot$season[[1]] & .data$week == snapshot$week[[1]])) |>
    dplyr::bind_rows(snapshot) |>
    recalculate_ranking_history_ranks() |>
    dplyr::arrange(.data$season, .data$week, .data$rank)
}

ranking_history_week <- function(history, season, week) {
  selected_season <- as.integer(season)
  selected_week <- as.integer(week)
  ranked_history <- recalculate_ranking_history_ranks(history)

  current <- ranked_history |>
    dplyr::filter(
      .data$season == .env$selected_season,
      .data$week == .env$selected_week
    )

  prior_weeks <- ranked_history |>
    dplyr::filter(
      .data$season == .env$selected_season,
      .data$week < .env$selected_week
    ) |>
    dplyr::pull(.data$week) |>
    unique()

  if (!length(prior_weeks)) {
    return(current |>
      dplyr::mutate(rating_change = NA_real_, rank_change = NA_integer_))
  }

  previous_week <- max(prior_weeks)
  previous <- ranked_history |>
    dplyr::filter(
      .data$season == .env$selected_season,
      .data$week == .env$previous_week
    ) |>
    dplyr::select(.data$team, previous_rating = .data$rating, previous_rank = .data$rank)

  current |>
    dplyr::left_join(previous, by = "team") |>
    dplyr::mutate(
      rating_change = .data$rating - .data$previous_rating,
      rank_change = .data$previous_rank - .data$rank
    )
}

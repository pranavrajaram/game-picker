tracked_picks_path <- file.path("data", "tracked-picks.rds")

empty_tracked_picks <- function() {
  tibble::tibble(
    event_id = character(),
    season = integer(),
    week = integer(),
    kickoff = as.POSIXct(character(), tz = "America/New_York"),
    away_team = character(),
    home_team = character(),
    pick_team = character(),
    pick_line = numeric(),
    edge = numeric(),
    model_home_line = numeric(),
    market_home_line = numeric(),
    away_rating = numeric(),
    home_rating = numeric(),
    bookmaker = character(),
    market_updated = as.POSIXct(character(), tz = "UTC"),
    rankings_source = character(),
    tracked_at = as.POSIXct(character(), tz = "America/New_York")
  )
}

read_tracked_picks <- function(path = tracked_picks_path) {
  if (exists("read_shared_tracked_picks", mode = "function")) {
    shared <- tryCatch(read_shared_tracked_picks(), error = function(error) NULL)
    if (!is.null(shared)) return(shared)
  }
  if (!file.exists(path)) return(empty_tracked_picks())
  readRDS(path)
}

write_tracked_picks <- function(picks, path = tracked_picks_path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(picks, path)
  readr::write_csv(picks, sub("\\.rds$", ".csv", path))
  if (exists("write_shared_tracked_picks", mode = "function")) {
    write_shared_tracked_picks(picks)
  }
  invisible(picks)
}

track_picks <- function(existing, candidates, rankings_source) {
  actionable <- candidates |>
    dplyr::filter(
      !.data$pick_team %in% c("Pass", "Waiting for line", "Missing rating"),
      !is.na(.data$pick_line)
    ) |>
    dplyr::transmute(
      event_id = as.character(.data$event_id),
      season = as.integer(.data$season),
      week = as.integer(.data$week),
      kickoff = .data$kickoff,
      away_team = .data$away_team,
      home_team = .data$home_team,
      pick_team = .data$pick_team,
      pick_line = .data$pick_line,
      edge = .data$edge,
      model_home_line = .data$model_home_line,
      market_home_line = .data$market_home_line,
      away_rating = .data$away_rating,
      home_rating = .data$home_rating,
      bookmaker = dplyr::coalesce(.data$bookmaker, "FanDuel"),
      market_updated = .data$market_updated,
      rankings_source = rankings_source,
      tracked_at = Sys.time()
    )

  new_rows <- actionable |>
    dplyr::anti_join(dplyr::select(existing, .data$event_id), by = "event_id")

  list(
    picks = dplyr::bind_rows(existing, new_rows) |>
      dplyr::arrange(.data$season, .data$week, .data$kickoff),
    added = nrow(new_rows),
    skipped = nrow(actionable) - nrow(new_rows)
  )
}

grade_tracked_picks <- function(tracked, schedule) {
  if (!nrow(tracked)) {
    return(tracked |>
      dplyr::mutate(
        final_away_score = numeric(),
        final_home_score = numeric(),
        game_completed = logical(),
        picked_home = logical(),
        team_margin = numeric(),
        ats_margin = numeric(),
        result = character()
      ))
  }

  finals <- schedule |>
    dplyr::select(
      .data$event_id,
      final_away_score = .data$away_score,
      final_home_score = .data$home_score,
      game_completed = .data$completed
    )

  tracked |>
    dplyr::left_join(finals, by = "event_id") |>
    dplyr::mutate(
      picked_home = .data$pick_team == .data$home_team,
      team_margin = dplyr::if_else(
        .data$picked_home,
        .data$final_home_score - .data$final_away_score,
        .data$final_away_score - .data$final_home_score
      ),
      ats_margin = .data$team_margin + .data$pick_line,
      result = dplyr::case_when(
        !dplyr::coalesce(.data$game_completed, FALSE) ~ "Pending",
        .data$ats_margin > 0 ~ "Win",
        .data$ats_margin < 0 ~ "Loss",
        TRUE ~ "Push"
      )
    )
}

edge_bucket <- function(edge) {
  cut(
    edge,
    breaks = c(-Inf, 1.99, 2.99, 3.99, Inf),
    labels = c("Under 2", "2–2.5", "3–3.5", "4+"),
    right = TRUE
  )
}

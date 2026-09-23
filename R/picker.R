team_dictionary <- function() {
  tibble::tribble(
    ~abbr, ~api_name, ~sheet_name,
    "ARI", "Arizona Cardinals", "Cardinals",
    "ATL", "Atlanta Falcons", "Falcons",
    "BAL", "Baltimore Ravens", "Ravens",
    "BUF", "Buffalo Bills", "Bills",
    "CAR", "Carolina Panthers", "Panthers",
    "CHI", "Chicago Bears", "Bears",
    "CIN", "Cincinnati Bengals", "Bengals",
    "CLE", "Cleveland Browns", "Browns",
    "DAL", "Dallas Cowboys", "Cowboys",
    "DEN", "Denver Broncos", "Broncos",
    "DET", "Detroit Lions", "Lions",
    "GB", "Green Bay Packers", "Packers",
    "HOU", "Houston Texans", "Texans",
    "IND", "Indianapolis Colts", "Colts",
    "JAX", "Jacksonville Jaguars", "Jaguars",
    "KC", "Kansas City Chiefs", "Chiefs",
    "LV", "Las Vegas Raiders", "Raiders",
    "LAC", "Los Angeles Chargers", "Chargers",
    "LA", "Los Angeles Rams", "Rams",
    "MIA", "Miami Dolphins", "Dolphins",
    "MIN", "Minnesota Vikings", "Vikings",
    "NE", "New England Patriots", "Patriots",
    "NO", "New Orleans Saints", "Saints",
    "NYG", "New York Giants", "Giants",
    "NYJ", "New York Jets", "Jets",
    "PHI", "Philadelphia Eagles", "Eagles",
    "PIT", "Pittsburgh Steelers", "Steelers",
    "SF", "San Francisco 49ers", "49ers",
    "SEA", "Seattle Seahawks", "Seahawks",
    "TB", "Tampa Bay Buccaneers", "Bucs",
    "TEN", "Tennessee Titans", "Titans",
    "WAS", "Washington Commanders", "Commanders"
  )
}

clean_team_key <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]", "")
}

canonical_team <- function(x) {
  dictionary <- team_dictionary()
  aliases <- dplyr::bind_rows(
    dplyr::transmute(dictionary, key = clean_team_key(.data$abbr), .data$abbr),
    dplyr::transmute(dictionary, key = clean_team_key(.data$api_name), .data$abbr),
    dplyr::transmute(dictionary, key = clean_team_key(.data$sheet_name), .data$abbr),
    tibble::tribble(
      ~key, ~abbr,
      "lar", "LA",
      "losangelesrams", "LA",
      "oak", "LV",
      "wsh", "WAS",
      "washingtonfootballteam", "WAS",
      "jacksonville", "JAX",
      "sanfrancisco", "SF",
      "tampabay", "TB"
    )
  ) |>
    dplyr::distinct(.data$key, .keep_all = TRUE)

  lookup <- stats::setNames(aliases$abbr, aliases$key)
  unname(lookup[clean_team_key(x)])
}

normalize_rankings <- function(rankings) {
  names(rankings) <- stringr::str_to_lower(stringr::str_trim(names(rankings)))
  team_column <- intersect(c("team", "teams"), names(rankings))[1]
  rating_column <- intersect(c("spread", "rating", "power_rating"), names(rankings))[1]

  if (is.na(team_column) || is.na(rating_column)) {
    stop("The rankings source must contain Team and Spread columns.")
  }

  normalized <- rankings |>
    dplyr::transmute(
      team_source = as.character(.data[[team_column]]),
      team = canonical_team(.data[[team_column]]),
      rating = suppressWarnings(as.numeric(.data[[rating_column]]))
    ) |>
    dplyr::filter(!is.na(.data$team_source), .data$team_source != "")

  if (any(is.na(normalized$team))) {
    unknown <- paste(normalized$team_source[is.na(normalized$team)], collapse = ", ")
    stop("Unrecognized team name(s) in rankings: ", unknown)
  }

  if (any(is.na(normalized$rating))) {
    bad <- paste(normalized$team_source[is.na(normalized$rating)], collapse = ", ")
    stop("Non-numeric spread rating(s) for: ", bad)
  }

  if (anyDuplicated(normalized$team)) {
    stop("The rankings contain duplicate teams after name matching.")
  }

  normalized
}

format_spread <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "—",
    abs(x) < 1e-9 ~ "PK",
    x > 0 ~ paste0("+", formatC(x, format = "f", digits = 1)),
    TRUE ~ formatC(x, format = "f", digits = 1)
  )
}

calculate_picks <- function(games, rankings, home_field = 2, min_edge = 1) {
  ratings <- normalize_rankings(rankings)

  games |>
    dplyr::mutate(
      home_team = canonical_team(.data$home_team),
      away_team = canonical_team(.data$away_team)
    ) |>
    dplyr::left_join(
      dplyr::select(ratings, home_team = team, home_rating = rating),
      by = "home_team"
    ) |>
    dplyr::left_join(
      dplyr::select(ratings, away_team = team, away_rating = rating),
      by = "away_team"
    ) |>
    dplyr::mutate(
      applied_hfa = dplyr::if_else(
        dplyr::coalesce(.data$neutral_site, FALSE),
        0,
        as.numeric(home_field)
      ),
      model_home_margin = .data$home_rating - .data$away_rating + .data$applied_hfa,
      model_home_line = -.data$model_home_margin,
      home_edge = .data$market_home_line - .data$model_home_line,
      edge = abs(.data$home_edge),
      pick_team = dplyr::case_when(
        is.na(.data$market_home_line) ~ "Waiting for line",
        is.na(.data$home_rating) | is.na(.data$away_rating) ~ "Missing rating",
        .data$edge < min_edge ~ "Pass",
        .data$home_edge > 0 ~ .data$home_team,
        TRUE ~ .data$away_team
      ),
      pick_line = dplyr::case_when(
        .data$pick_team == .data$home_team ~ .data$market_home_line,
        .data$pick_team == .data$away_team ~ -.data$market_home_line,
        TRUE ~ NA_real_
      )
    )
}

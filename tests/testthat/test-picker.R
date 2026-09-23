source(file.path("..", "..", "R", "picker.R"), local = TRUE)
source(file.path("..", "..", "R", "sources.R"), local = TRUE)

testthat::test_that("team names normalize across sources", {
  testthat::expect_equal(
    canonical_team(c("Kansas City Chiefs", "Chiefs", "KC", "49ers", "San Francisco 49ers")),
    c("KC", "KC", "KC", "SF", "SF")
  )
})

testthat::test_that("Bears at Chiefs produces the expected model line", {
  rankings <- tibble::tribble(
    ~Team, ~Spread,
    "Bears", 1.5,
    "Chiefs", 3.5
  )
  games <- tibble::tibble(
    home_team = "Kansas City Chiefs",
    away_team = "Chicago Bears",
    market_home_line = -3.5,
    neutral_site = FALSE
  )

  result <- calculate_picks(games, rankings, home_field = 2.5, min_edge = 1)
  testthat::expect_equal(result$model_home_line, -4.5)
  testthat::expect_equal(result$home_edge, 1)
  testthat::expect_equal(result$pick_team, "KC")
  testthat::expect_equal(result$pick_line, -3.5)
})

testthat::test_that("small edges are passes", {
  rankings <- tibble::tribble(
    ~Team, ~Spread,
    "Bears", 1.5,
    "Chiefs", 3.5
  )
  games <- tibble::tibble(
    home_team = "Chiefs",
    away_team = "Bears",
    market_home_line = -4,
    neutral_site = FALSE
  )

  result <- calculate_picks(games, rankings, home_field = 2.5, min_edge = 1)
  testthat::expect_equal(result$pick_team, "Pass")
})

testthat::test_that("schedule and odds join on canonical team names", {
  schedule <- tibble::tibble(
    home_team = "KC",
    away_team = "CHI",
    week = 1L,
    neutral_site = FALSE
  )
  odds <- tibble::tibble(
    home_team = "KC",
    away_team = "CHI",
    market_home_line = -3.5,
    market_price = -110,
    market_updated = as.POSIXct("2026-09-01 12:00:00", tz = "UTC"),
    bookmaker = "FanDuel"
  )

  joined <- join_schedule_and_odds(schedule, odds)
  testthat::expect_equal(nrow(joined), 1)
  testthat::expect_equal(joined$market_home_line, -3.5)
})

testthat::test_that("FanDuel rows are selected by bookmaker key", {
  raw <- tibble::tibble(
    id = c("game-1", "game-1", "game-1", "game-1"),
    commence_time = rep("2026-09-27T17:00:00Z", 4),
    home_team = rep("Kansas City Chiefs", 4),
    away_team = rep("Chicago Bears", 4),
    bookmaker_key = c("fanduel", "fanduel", "draftkings", "draftkings"),
    bookmaker = c("FanDuel", "FanDuel", "DraftKings", "DraftKings"),
    market_key = rep("spreads", 4),
    market_last_update = rep("2026-09-22T01:00:00Z", 4),
    outcomes_name = c("Kansas City Chiefs", "Chicago Bears", "Kansas City Chiefs", "Chicago Bears"),
    outcomes_price = rep(-110, 4),
    outcomes_point = c(-3.5, 3.5, -4, 4)
  )

  selected <- parse_odds(raw, "fanduel")
  testthat::expect_equal(nrow(selected), 1)
  testthat::expect_equal(selected$market_home_line, -3.5)
  testthat::expect_equal(selected$bookmaker, "FanDuel")
})

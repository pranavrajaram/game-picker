library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(lubridate)

source(file.path("R", "picker.R"), local = TRUE)
source(file.path("R", "sources.R"), local = TRUE)
source(file.path("R", "tracking.R"), local = TRUE)
source(file.path("R", "history.R"), local = TRUE)
source(file.path("R", "cloud-storage.R"), local = TRUE)

season_default <- current_nfl_season()

format_app_time <- function(x, include_weekday = TRUE) {
  pattern <- if (include_weekday) "%a %b %d, %I:%M %p" else "%b %d, %I:%M %p"
  formatted <- format(x, pattern)
  formatted <- stringr::str_replace(formatted, " 0([0-9]),", " \\1,")
  stringr::str_replace(formatted, ", 0([0-9]):", ", \\1:")
}

team_name_lookup <- function() {
  dictionary <- team_dictionary()
  stats::setNames(dictionary$sheet_name, dictionary$abbr)
}

chalk_theme <- bs_theme(
  version = 5,
  bg = "#17382b",
  fg = "#f3eddd",
  primary = "#e7ca78",
  secondary = "#c8c0aa",
  base_font = font_collection("Arial", "Helvetica", "sans-serif"),
  heading_font = font_collection("Arial", "Helvetica", "sans-serif"),
  border_radius = "0"
)

status_strip <- function(...) div(class = "score-strip", ...)

status_item <- function(label, output_id) {
  div(
    class = "score-item",
    div(class = "score-label", label),
    div(class = "score-value", textOutput(output_id, inline = TRUE))
  )
}

connection_status <- function() {
  div(class = "connection-block", uiOutput("source_status"), uiOutput("odds_status"))
}

board_page <- div(
  class = "page-grid",
  tags$aside(
    class = "control-rail",
    div(class = "rail-heading", "Board settings"),
    numericInput("season", "Season", value = season_default, min = 2020, max = season_default + 1, step = 1),
    selectInput("week", "NFL week", choices = NULL),
    numericInput("home_field", "Home-field advantage", value = 2, min = 0, max = 6, step = 0.5),
    numericInput("min_edge", "Minimum edge", value = 1, min = 0, max = 10, step = 0.5),
    div(
      class = "rail-actions",
      actionButton("refresh", "Refresh FanDuel lines", class = "btn-chalk-primary"),
      actionButton("connect_google", "Sync Google rankings", class = "btn-chalk")
    ),
    connection_status()
  ),
  tags$main(
    class = "main-board",
    tags$section(
      class = "chalk-panel board-panel",
      div(
        class = "section-heading",
        h2("Weekly board")
      ),
      div(
        class = "table-toolbar",
        span(class = "selection-note", textOutput("selection_count", inline = TRUE)),
        div(
          class = "table-actions",
          actionButton("track_selected", "Track", class = "btn-chalk-primary"),
          actionButton("untrack_selected", "Untrack", class = "btn-chalk-danger"),
          actionButton("track_all", "Track all picks", class = "btn-chalk")
        )
      ),
      DTOutput("board")
    )
  )
)

results_page <- div(
  class = "results-page",
  div(
    class = "results-toolbar",
    h2("Tracked results"),
    div(
      class = "result-filters",
      selectInput("results_week", "Week", choices = c("All weeks" = "all"), selected = "all"),
      numericInput("results_min_edge", "Minimum displayed edge", value = 0, min = 0, max = 10, step = 0.5),
      actionButton("refresh_results", "Update final scores", class = "btn-chalk")
    )
  ),
  status_strip(
    status_item("Record", "results_record"),
    status_item("ATS win rate", "results_win_rate"),
    status_item("Completed", "results_completed"),
    status_item("Pending", "results_pending")
  ),
  div(
    class = "results-grid",
    tags$section(
      class = "chalk-panel",
      div(
        class = "section-heading",
        h3("Pick ledger"),
        actionButton("remove_results_selected", "Untrack selected", class = "btn-chalk-danger")
      ),
      DTOutput("results_table")
    ),
    tags$section(
      class = "chalk-panel edge-panel",
      div(class = "section-heading", h3("Performance by edge")),
      DTOutput("edge_table")
    )
  )
)

history_page <- div(
  class = "history-page",
  div(
    class = "history-toolbar",
    h2("Ranking history"),
    selectInput("history_week", "Week", choices = NULL)
  ),
  tags$section(
    class = "chalk-panel history-panel",
    DTOutput("history_table")
  )
)

ui <- page_fluid(
  theme = chalk_theme,
  tags$head(
    tags$title("The House Grid Always Wins"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=history1")
  ),
  div(
    class = "app-header",
    div(class = "brand-name", "The ", tags$s("House"), " Grid Always Wins")
  ),
  navset_tab(
    id = "main_page",
    nav_panel("BOARD", board_page),
    nav_panel("RESULTS", results_page),
    nav_panel("RANKING HISTORY", history_page)
  )
)

server <- function(input, output, session) {
  try(connect_google_sheets(), silent = TRUE)
  state <- reactiveValues(
    rankings = NULL,
    rankings_source = NULL,
    rankings_warning = NULL,
    rankings_synced_at = NULL,
    schedule = NULL,
    current_week = NULL,
    schedule_error = NULL,
    odds = NULL,
    odds_error = NULL,
    odds_from_cache = FALSE,
    quota = NULL,
    api_key_source = NULL
  )
  tracked <- reactiveVal(read_tracked_picks())
  ranking_history <- reactiveVal(read_ranking_history())

  load_rankings <- function(google_only = FALSE) {
    result <- tryCatch(
      {
        if (!googlesheets4::gs4_has_token()) try(connect_google_sheets(), silent = TRUE)
        if (google_only) {
          data <- read_google_rankings(default_sheet_url)
          list(data = data, source = paste0("Google Sheet · ", attr(data, "sheet_tab")), warning = NULL)
        } else {
          read_rankings_source(default_sheet_url)
        }
      },
      error = function(error) list(data = NULL, source = NULL, warning = conditionMessage(error))
    )

    if (!is.null(result$data)) {
      normalize_rankings(result$data)
      state$rankings <- result$data
      state$rankings_source <- result$source
      state$rankings_warning <- result$warning
      state$rankings_synced_at <- Sys.time()
    } else {
      state$rankings_warning <- result$warning
    }
    invisible(result)
  }

  load_schedule <- function(season) {
    result <- tryCatch(
      fetch_schedule(season),
      error = function(error) {
        state$schedule_error <- conditionMessage(error)
        NULL
      }
    )

    if (!is.null(result)) {
      state$schedule <- result
      state$schedule_error <- NULL
      available_weeks <- sort(unique(result$week))
      upcoming <- result |> filter(.data$kickoff >= Sys.time()) |> pull(.data$week)
      selected_week <- if (length(upcoming)) min(upcoming) else max(available_weeks)
      state$current_week <- selected_week
      visible_weeks <- available_weeks[available_weeks >= selected_week]
      updateSelectInput(session, "week", choices = visible_weeks, selected = selected_week)
    }
    invisible(result)
  }

  observeEvent(input$season, {
    load_schedule(input$season)
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

  observeEvent(input$connect_google, {
    tryCatch(
      {
        connect_google_sheets()
        load_rankings(google_only = TRUE)
        showNotification("Rankings synced from Google Sheets.", type = "message")
      },
      error = function(error) {
        state$rankings_warning <- conditionMessage(error)
        showNotification(paste("Google sync failed:", conditionMessage(error)), type = "error", duration = NULL)
      }
    )
  })

  observeEvent(input$refresh, {
    load_rankings(google_only = googlesheets4::gs4_has_token())
    key_info <- load_local_odds_key()
    state$api_key_source <- if (nzchar(key_info$key)) key_info$source else NULL

    odds_result <- tryCatch(
      if (input$refresh == 0) {
        cached <- read_odds_cache("fanduel")
        if (is.null(cached)) stop("No cached odds yet. Click Refresh FanDuel lines once.")
        cached
      } else {
        fetch_odds_cached("fanduel", api_key = key_info$key, force = FALSE, max_age_minutes = 60)
      },
      error = function(error) {
        state$odds_error <- conditionMessage(error)
        NULL
      }
    )

    if (!is.null(odds_result)) {
      state$odds <- odds_result
      state$odds_error <- NULL
      state$odds_from_cache <- isTRUE(attr(odds_result, "from_cache"))
      state$quota <- attr(odds_result, "quota")
    }
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

  board_data <- reactive({
    req(state$schedule, state$rankings, input$week)
    schedule_week <- state$schedule |> filter(.data$week == as.integer(input$week))
    games <- if (is.null(state$odds)) {
      schedule_week |>
        mutate(
          market_home_line = NA_real_, market_price = NA_real_,
          market_updated = as.POSIXct(NA), bookmaker = "FanDuel"
        )
    } else {
      join_schedule_and_odds(schedule_week, state$odds)
    }

    calculate_picks(games, state$rankings, input$home_field, input$min_edge) |>
      arrange(.data$kickoff)
  })

  board_view <- reactive({
    names_lookup <- team_name_lookup()
    saved_ids <- tracked()$event_id
    board_data() |>
      mutate(
        tracked_display = if_else(.data$event_id %in% saved_ids, "TRACKED", "—"),
        kickoff_display = format_app_time(with_tz(.data$kickoff, "America/New_York")),
        matchup = paste0(names_lookup[.data$away_team], " @ ", names_lookup[.data$home_team]),
        model_display = paste0(names_lookup[.data$home_team], " ", format_spread(.data$model_home_line)),
        market_display = paste0(names_lookup[.data$home_team], " ", format_spread(.data$market_home_line)),
        pick_display = case_when(
          .data$pick_team %in% c("Pass", "Waiting for line", "Missing rating") ~ .data$pick_team,
          TRUE ~ paste0(names_lookup[.data$pick_team], " ", format_spread(.data$pick_line))
        ),
        edge_display = if_else(is.na(.data$edge), "—", formatC(.data$edge, format = "f", digits = 1))
      )
  })

  board_display <- reactive({
    board_view() |>
      select(
        Status = tracked_display, Kickoff = kickoff_display, Matchup = matchup,
        `Away rtg` = away_rating, `Home rtg` = home_rating,
        `Our line` = model_display, FanDuel = market_display,
        Pick = pick_display, Edge = edge_display
      )
  })

  output$board <- renderDT({
    datatable(
      board_display(), rownames = FALSE,
      selection = list(mode = "multiple", target = "row"),
      options = list(
        dom = "t", ordering = FALSE, paging = FALSE, scrollX = TRUE, autoWidth = FALSE,
        columnDefs = list(
          list(width = "58px", targets = 0), list(width = "140px", targets = 1),
          list(width = "170px", targets = 2), list(width = "72px", targets = c(3, 4)),
          list(width = "115px", targets = c(5, 6, 7)), list(width = "55px", targets = 8)
        )
      ),
      class = "chalk-table"
    ) |>
      formatStyle("Status", color = "#e7ca78", fontWeight = "700") |>
      formatStyle(c("Away rtg", "Home rtg"), color = "#f3eddd", fontSize = "13px", fontWeight = "700") |>
      formatStyle(c("Our line", "FanDuel"), color = "#f3eddd", fontSize = "13px", fontWeight = "700") |>
      formatStyle("Pick", color = "#e7ca78", fontSize = "13px", fontWeight = "800") |>
      formatStyle("Pick", color = styleEqual(c("Pass", "Waiting for line"), c("#aca48e", "#aca48e"))) |>
      formatStyle(
        "Edge",
        color = styleInterval(c(0.99, 1.99, 2.99), c("#aca48e", "#f3eddd", "#e7ca78", "#f1b96a")),
        fontSize = "14px", fontWeight = "800"
      )
  })

  save_candidates <- function(candidates) {
    latest <- read_tracked_picks()
    result <- track_picks(latest, candidates, state$rankings_source)
    tracked(result$picks)
    write_tracked_picks(result$picks)
    showNotification(
      paste0(result$added, " pick", if (result$added == 1) "" else "s", " added", if (result$skipped) paste0("; ", result$skipped, " already tracked") else "", "."),
      type = "message"
    )
  }

  observeEvent(input$track_selected, {
    rows <- input$board_rows_selected
    if (!length(rows)) {
      showNotification("Select at least one actionable row first.", type = "warning")
      return()
    }
    save_candidates(board_view()[rows, , drop = FALSE])
    selectRows(dataTableProxy("board"), NULL)
  })

  remove_tracked_ids <- function(ids) {
    ids <- unique(stats::na.omit(as.character(ids)))
    existing <- read_tracked_picks()
    removed <- sum(existing$event_id %in% ids)
    if (!removed) {
      showNotification("None of the selected rows are currently tracked.", type = "warning")
      return(invisible(FALSE))
    }
    updated <- existing |> filter(!.data$event_id %in% ids)
    tracked(updated)
    write_tracked_picks(updated)
    showNotification(paste0(removed, " pick", if (removed == 1) "" else "s", " removed from tracking."), type = "message")
    invisible(TRUE)
  }

  observeEvent(input$untrack_selected, {
    rows <- input$board_rows_selected
    if (!length(rows)) {
      showNotification("Select at least one tracked row first.", type = "warning")
      return()
    }
    remove_tracked_ids(board_view()$event_id[rows])
    selectRows(dataTableProxy("board"), NULL)
  })

  observeEvent(input$track_all, {
    save_candidates(board_view())
  })

  observeEvent(list(state$rankings, state$current_week), {
    req(state$rankings, state$current_week, input$season)
    req(grepl("^Google Sheet", state$rankings_source))
    snapshot <- create_ranking_snapshot(
      state$rankings, input$season, state$current_week, state$rankings_source
    )
    updated <- upsert_ranking_snapshot(read_ranking_history(), snapshot)
    ranking_history(updated)
    write_ranking_history(updated)
  }, ignoreInit = FALSE)

  observeEvent(input$refresh_results, {
    refreshed <- load_schedule(input$season)
    if (!is.null(refreshed)) showNotification("Final scores updated from nflverse.", type = "message")
  })

  graded_results <- reactive({
    req(state$schedule)
    grade_tracked_picks(tracked(), state$schedule)
  })

  observe({
    weeks <- sort(unique(tracked()$week))
    week_choices <- if (length(weeks)) stats::setNames(as.character(weeks), paste("Week", weeks)) else character()
    choices <- c("All weeks" = "all", week_choices)
    selected <- isolate(input$results_week)
    if (is.null(selected) || !selected %in% unname(choices)) selected <- "all"
    updateSelectInput(session, "results_week", choices = choices, selected = selected)
  })

  filtered_results <- reactive({
    data <- graded_results() |> filter(.data$edge >= input$results_min_edge)
    if (!is.null(input$results_week) && input$results_week != "all") {
      data <- data |> filter(.data$week == as.integer(input$results_week))
    }
    data
  })

  results_display <- reactive({
    names_lookup <- team_name_lookup()
    filtered_results() |>
      mutate(
        matchup = paste0(names_lookup[.data$away_team], " @ ", names_lookup[.data$home_team]),
        pick_display = paste0(names_lookup[.data$pick_team], " ", format_spread(.data$pick_line)),
        final_display = if_else(
          .data$result == "Pending", "—",
          paste0(names_lookup[.data$away_team], " ", .data$final_away_score, "–", .data$final_home_score, " ", names_lookup[.data$home_team])
        ),
        ats_display = if_else(.data$result == "Pending", "—", formatC(.data$ats_margin, format = "f", digits = 1))
      ) |>
      arrange(desc(.data$week), .data$kickoff) |>
      select(Week = week, Matchup = matchup, Pick = pick_display, Edge = edge, Final = final_display, `ATS margin` = ats_display, Result = result)
  })

  output$results_table <- renderDT({
    datatable(
      results_display(), rownames = FALSE, selection = list(mode = "multiple", target = "row"),
      options = list(dom = "t", ordering = FALSE, paging = FALSE, scrollX = TRUE),
      class = "chalk-table results-table"
    ) |>
      formatRound("Edge", digits = 1) |>
      formatStyle(
        "Result",
        color = styleEqual(c("Win", "Loss", "Push", "Pending"), c("#d7e99b", "#f0a39a", "#e7ca78", "#aca48e")),
        fontWeight = "700"
      )
  })

  observeEvent(input$remove_results_selected, {
    rows <- input$results_table_rows_selected
    if (!length(rows)) {
      showNotification("Select at least one ledger row first.", type = "warning")
      return()
    }
    ordered <- filtered_results() |> arrange(desc(.data$week), .data$kickoff)
    remove_tracked_ids(ordered$event_id[rows])
    selectRows(dataTableProxy("results_table"), NULL)
  })

  edge_performance <- reactive({
    filtered_results() |>
      filter(.data$result != "Pending") |>
      mutate(`Edge band` = edge_bucket(.data$edge)) |>
      group_by(.data$`Edge band`) |>
      summarise(
        Picks = dplyr::n(), Wins = sum(.data$result == "Win"),
        Losses = sum(.data$result == "Loss"), Pushes = sum(.data$result == "Push"),
        `Win rate` = if_else(Wins + Losses > 0, Wins / (Wins + Losses), NA_real_),
        .groups = "drop"
      )
  })

  output$edge_table <- renderDT({
    datatable(
      edge_performance(), rownames = FALSE, selection = "none",
      options = list(dom = "t", ordering = FALSE, paging = FALSE), class = "chalk-table"
    ) |>
      formatPercentage("Win rate", digits = 1)
  })

  observe({
    available <- ranking_history() |>
      filter(.data$season == input$season) |>
      distinct(.data$week) |>
      arrange(.data$week) |>
      pull(.data$week)
    choices <- stats::setNames(as.character(available), paste("Week", available))
    selected <- isolate(input$history_week)
    if (!length(available)) selected <- character()
    else if (is.null(selected) || !selected %in% unname(choices)) selected <- as.character(max(available))
    updateSelectInput(session, "history_week", choices = choices, selected = selected)
  })

  history_display <- reactive({
    req(input$history_week)
    names_lookup <- team_name_lookup()
    display <- ranking_history_week(ranking_history(), input$season, as.integer(input$history_week)) |>
      arrange(.data$rank) |>
      mutate(
        team_name = names_lookup[.data$team],
        rating_display = formatC(.data$rating, format = "f", digits = 1),
        rating_change_display = if_else(
          is.na(.data$rating_change), "—",
          sprintf("%+.1f", .data$rating_change)
        ),
        rank_change_display = if_else(
          is.na(.data$rank_change), "—",
          sprintf("%+d", .data$rank_change)
        )
      ) |>
      select(
        Rank = rank, Team = team_name, Rating = rating_display, Tier = tier,
        `Rating change` = rating_change_display, `Rank change` = rank_change_display
      )

    if (all(is.na(display$Tier) | display$Tier == "")) {
      display <- display |> select(-Tier)
    }
    display
  })

  output$history_table <- renderDT({
    datatable(
      history_display(), rownames = FALSE, selection = "none",
      options = list(dom = "t", ordering = FALSE, paging = FALSE),
      class = "chalk-table history-table"
    ) |>
      formatStyle(c("Rank", "Rating"), color = "#f3eddd", fontSize = "14px", fontWeight = "800") |>
      formatStyle("Team", fontSize = "13px", fontWeight = "700") |>
      formatStyle(
        c("Rating change", "Rank change"),
        color = JS("value === '—' ? '#aca48e' : (value.charAt(0) === '+' ? '#d7e99b' : (value.charAt(0) === '-' ? '#f0a39a' : '#f3eddd'))"),
        fontWeight = "700"
      )
  })

  output$selection_count <- renderText({
    count <- length(input$board_rows_selected)
    paste(count, "selected")
  })

  completed_results <- reactive(filtered_results() |> filter(.data$result != "Pending"))
  output$results_record <- renderText({
    data <- completed_results()
    paste0(sum(data$result == "Win"), "–", sum(data$result == "Loss"), "–", sum(data$result == "Push"))
  })
  output$results_win_rate <- renderText({
    data <- completed_results()
    decisions <- sum(data$result %in% c("Win", "Loss"))
    if (!decisions) "—" else paste0(formatC(100 * sum(data$result == "Win") / decisions, format = "f", digits = 1), "%")
  })
  output$results_completed <- renderText(nrow(completed_results()))
  output$results_pending <- renderText(sum(filtered_results()$result == "Pending"))

  output$source_status <- renderUI({
    source_value <- if (is.null(state$rankings_source)) "Unavailable" else state$rankings_source
    tagList(
      div(class = "status-label", "RANKINGS"),
      div(class = "status-value", source_value),
      if (!is.null(state$rankings_warning)) div(class = "status-warning", state$rankings_warning)
    )
  })

  output$odds_status <- renderUI({
    if (!is.null(state$odds_error)) {
      return(tagList(div(class = "status-label", "MARKET"), div(class = "status-warning", state$odds_error)))
    }
    tagList(
      div(class = "status-label", "MARKET"),
      div(class = "status-value", if (state$odds_from_cache) "FanDuel · cached" else "FanDuel · updated"),
      if (!is.null(state$quota) && nrow(state$quota)) div(class = "status-note", paste0(state$quota$requests_remaining[[1]], " API credits left"))
    )
  })
}

shinyApp(ui, server)

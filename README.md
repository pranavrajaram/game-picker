# The House Grid Always Wins

A Shiny app that combines shared Google Sheet power ratings with FanDuel NFL spreads from The Odds API.

## One-time setup

1. Install or update the required packages from the project root:

   ```r
   source("game-picker/setup.R")
   ```

2. Add your Odds API key to your user-level `~/.Renviron` file:

   ```text
   ODDS_API_KEY=your-real-key
   ```

3. Keep the Google service-account JSON outside the project and place its path in `.service-account-path`. The spreadsheet must be shared with the service account as an Editor.

4. Restart R and run:

   ```r
   shiny::runApp("game-picker")
   ```

## Posit Connect Cloud deployment

Connect Cloud deploys this app from GitHub using `app.R` and `manifest.json`. Configure these encrypted variables in the content's Advanced settings:

- `ODDS_API_KEY`
- `GOOGLE_SERVICE_ACCOUNT_JSON_BASE64`
- `GOOGLE_SHEET_URL`

The service-account value is its JSON encoded as a single base64 string. Neither the credential file nor the private Sheet URL is committed or deployed as a file.

The app reads rankings from `For Sim` and stores shared application data in the hidden-compatible `App Picks`, `Ranking History`, and `Odds Cache` tabs. If Google authorization is unavailable, rankings fall back to `data/rankings-fallback.csv`.

## V1 behavior

- Pulls the NFL schedule with `nflreadr`.
- Pulls the featured point-spread market for US sportsbooks with `oddsapiR`.
- Uses FanDuel by default.
- Uses 2.0 points of home-field advantage by default.
- App startup reads cached odds without calling the API.
- Refresh attempts are limited to one API call per hour; repeated clicks reuse the cache.
- Calculates the fair home line as `-(home rating - away rating + HFA)`.
- Picks the side only when the absolute model edge meets the selected threshold.
- Lets you select individual picks or track every actionable model pick.
- Saves immutable pick, line, rating and edge snapshots to the shared `App Picks` sheet.
- Saves weekly rating snapshots to the shared `Ranking History` sheet.
- Shares the odds cache through Google Sheets so separate app instances do not make duplicate API calls unnecessarily.
- Grades saved picks against nflverse final scores on a separate Results page.

API keys and Google authorization tokens are intentionally excluded from source control.

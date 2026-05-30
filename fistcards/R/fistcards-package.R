#' fistcards: Logic helpers and Shiny app for the FIST card task
#'
#' The `fistcards` package defines a 27-card stimulus deck for the FIST card
#' task, provides rule-based helpers for scoring Phase 1 and Phase 2 trials,
#' generates valid trial tables, and launches an included Shiny app for running
#' the task interactively.
#'
#' @section Main functions:
#' - `fist_cards()` builds the card metadata table.
#' - `generate_phase1_trials()` and `generate_phase2_trials()` create valid
#'   trials from the deck.
#' - `correct_phase1_choice()` and `correct_phase2_choice()` score card choices.
#' - `run_fist_game()` starts the packaged Shiny app.
#'
#' @keywords internal
"_PACKAGE"

#' Run the FIST card game
#'
#' Starts the Shiny version of the FIST card task that is bundled in
#' `inst/app`. The app lets a researcher enter a participant ID, choose the
#' number of Phase 1 and Phase 2 trials, run the card task, and download the
#' recorded responses.
#'
#' @param ... Additional arguments passed to `shiny::runApp()`, such as
#'   `launch.browser`, `port`, or `host`.
#'
#' @return The result of `shiny::runApp()`. This function is called for its
#'   side effect of launching the Shiny application.
#' @export
#'
#' @examples
#' \dontrun{
#' run_fist_game()
#' }
run_fist_game <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The shiny package is required to run the game.", call. = FALSE)
  }

  app_dir <- system.file("app", package = "fistcards")
  shiny::runApp(app_dir, ...)
}

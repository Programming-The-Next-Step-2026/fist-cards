#' Run the FIST card game
#'
#' Starts the Shiny version of the FIST card task.
#'
#' @param ... Additional arguments passed to `shiny::runApp()`.
#'
#' @return The result of `shiny::runApp()`.
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

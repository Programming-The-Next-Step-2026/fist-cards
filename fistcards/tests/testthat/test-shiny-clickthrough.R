test_that("Shiny app can be clicked through in a browser", {
  skip_if_not_installed("shinytest2")

  expect_page_text <- function(app, text) {
    expect_match(app$get_text("body"), text, fixed = TRUE)
  }

  wait_for_screen <- function(app) {
    app$wait_for_value(output = "screen")
    app$wait_for_idle()
  }

  app_dir <- system.file("app", package = "fistcards")
  app <- shinytest2::AppDriver$new(
    app_dir,
    name = "fistcards-clickthrough",
    seed = 123,
    width = 1200,
    height = 900
  )

  wait_for_screen(app)
  expect_page_text(app, "FIST Cards")

  app$set_inputs(
    participant_id = "test-participant",
    phase1_trials = 1,
    phase2_trials = 1
  )
  app$click("start")
  wait_for_screen(app)

  expect_page_text(app, "First game")
  app$click("begin_phase1")
  wait_for_screen(app)

  expect_page_text(app, "Find the card that goes with this one")
  app$click("choice1")
  wait_for_screen(app)

  expect_page_text(app, "Second game")
  app$click("begin_phase2")
  wait_for_screen(app)

  expect_page_text(app, "Find the card that goes with both others")
  app$click("choice1")
  wait_for_screen(app)

  expect_page_text(app, "Finished")
  expect_page_text(app, "Thank you for your participation.")
  app$click("view_results")
  wait_for_screen(app)

  expect_page_text(app, "Results")
  expect_page_text(app, "Download results")
})

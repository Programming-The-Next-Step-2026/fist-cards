test_that("packaged Shiny app can be created", {
  app_dir <- system.file("app", package = "fistcards")

  expect_true(dir.exists(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))

  app <- shiny::shinyAppDir(app_dir)

  expect_s3_class(app, "shiny.appobj")
})

test_that("app helpers prepare trials and results", {
  app_dir <- system.file("app", package = "fistcards")
  app_env <- new.env(parent = globalenv())

  sys.source(file.path(app_dir, "app.R"), envir = app_env)

  expect_equal(app_env$valid_trial_count("3"), 3L)
  expect_equal(app_env$valid_trial_count("-2"), 0L)
  expect_equal(app_env$valid_trial_count("not a number"), 0L)
  expect_equal(app_env$sanitize_csv_text("=participant"), "'=participant")
  expect_equal(app_env$sanitize_csv_text("participant"), "participant")

  trials <- app_env$build_trials(2, 2)
  expect_equal(nrow(trials), 4)
  expect_equal(trials$phase, c(1L, 1L, 2L, 2L))
  expect_true(all(app_env$trial_columns %in% names(trials)))

  empty_summary <- app_env$results_summary(app_env$empty_results())
  expect_equal(empty_summary$total, 0L)
  expect_true(is.na(empty_summary$percent))
})

test_that("app records only one response per trial", {
  app_dir <- system.file("app", package = "fistcards")
  app_env <- new.env(parent = globalenv())
  sys.source(file.path(app_dir, "app.R"), envir = app_env)

  testServer(app_env$server, {
    session$setInputs(
      participant_id = "=participant",
      phase1_trials = 1,
      phase2_trials = 0
    )
    session$setInputs(start = 1)
    session$setInputs(begin_phase1 = 1)

    session$setInputs(choice1 = 1)
    session$setInputs(choice1 = 2)

    recorded <- results()
    expect_equal(nrow(recorded), 1)
    expect_equal(recorded$participant_id, "'=participant")
    expect_true(complete())
  })
})

test_that("fist_cards lists the full deck", {
  cards <- fist_cards()

  expect_equal(nrow(cards), 27)
  expect_equal(length(unique(cards$id)), 27)
  expect_equal(sort(unique(cards$colour)), c("blue", "red", "yellow"))
  expect_equal(sort(unique(cards$animal)), c("chicken", "frog", "snail"))
  expect_equal(sort(unique(cards$number)), 1:3)
})

test_that("fist_cards builds image paths for game assets", {
  cards <- fist_cards()

  expect_true("blue_frog_1" %in% cards$id)
  expect_true("cards/blue_frog_1.png" %in% cards$image)
})

test_that("each listed card has an app image asset", {
  cards <- fist_cards()
  asset_dir <- system.file("app", "www", package = "fistcards")
  image_paths <- file.path(asset_dir, cards$image)

  expect_true(dir.exists(asset_dir))
  expect_true(all(file.exists(image_paths)))
})

test_that("find_card returns one requested card", {
  card <- find_card("yellow", "snail", 3)

  expect_equal(nrow(card), 1)
  expect_equal(card$id, "yellow_snail_3")
})

test_that("find_card requires one value per feature", {
  expect_error(
    find_card(c("blue", "red"), "frog", 1),
    "must each be a single value",
    fixed = TRUE
  )
})

test_that("generate_phase1_trials creates trials with exactly one correct choice", {
  trials <- generate_phase1_trials(n = 25, seed = 123)
  cards <- fist_cards()

  expect_equal(nrow(trials), 25)
  expect_true(all(trials$correct_choice %in% c(1, 2)))

  for (index in seq_len(nrow(trials))) {
    trial <- trials[index, ]
    target <- cards[cards$id == trial$target_id, , drop = FALSE]
    choice1 <- cards[cards$id == trial$choice1_id, , drop = FALSE]
    choice2 <- cards[cards$id == trial$choice2_id, , drop = FALSE]

    expect_equal(correct_phase1_choice(choice1, choice2, target), trial$correct_choice)
    expect_equal(shared_feature_count(list(choice1, choice2)[[trial$correct_choice]], target), 1)
    expect_equal(shared_feature_count(list(choice1, choice2)[[3 - trial$correct_choice]], target), 0)
    expect_equal(shared_feature_count(choice1, choice2), 0)
  }
})

test_that("generate_phase1_trials avoids visually similar option pairs", {
  trials <- generate_phase1_trials()
  cards <- fist_cards()

  for (index in seq_len(nrow(trials))) {
    trial <- trials[index, ]
    choice1 <- cards[cards$id == trial$choice1_id, , drop = FALSE]
    choice2 <- cards[cards$id == trial$choice2_id, , drop = FALSE]

    expect_false(identical(choice1$animal, choice2$animal))
    expect_false(identical(choice1$colour, choice2$colour))
    expect_false(identical(choice1$number, choice2$number))
  }
})

test_that("generate_phase1_trials handles empty and invalid requests clearly", {
  expect_equal(nrow(generate_phase1_trials(cards = fist_cards()[0, ], n = 0)), 0)
  expect_error(generate_phase1_trials(n = -1), "non-negative", fixed = TRUE)
  expect_error(
    generate_phase1_trials(cards = data.frame(id = character())),
    "cards must contain columns",
    fixed = TRUE
  )
})

test_that("generate_phase2_trials creates trials with exactly one special card", {
  trials <- generate_phase2_trials(n = 25, seed = 123)
  cards <- fist_cards()

  expect_equal(nrow(trials), 25)
  expect_true(all(trials$correct_choice %in% c(1, 2, 3)))

  for (index in seq_len(nrow(trials))) {
    trial <- trials[index, ]
    card1 <- cards[cards$id == trial$card1_id, , drop = FALSE]
    card2 <- cards[cards$id == trial$card2_id, , drop = FALSE]
    card3 <- cards[cards$id == trial$card3_id, , drop = FALSE]

    expect_equal(correct_phase2_choice(card1, card2, card3), trial$correct_choice)

    cards_shown <- list(card1, card2, card3)
    special_card <- cards_shown[[trial$correct_choice]]
    other_cards <- cards_shown[-trial$correct_choice]
    shared_with_first <- shared_features(special_card, other_cards[[1]])
    shared_with_second <- shared_features(special_card, other_cards[[2]])

    expect_equal(shared_feature_count(special_card, other_cards[[1]]), 1)
    expect_equal(shared_feature_count(special_card, other_cards[[2]]), 1)
    expect_false(identical(shared_with_first, shared_with_second))
    expect_equal(shared_feature_count(other_cards[[1]], other_cards[[2]]), 0)
  }
})

test_that("generate_phase2_trials handles too-small and invalid requests clearly", {
  expect_equal(nrow(generate_phase2_trials(cards = fist_cards()[1:2, ], n = 0)), 0)
  expect_error(generate_phase2_trials(n = -1), "non-negative", fixed = TRUE)
  expect_error(
    generate_phase2_trials(cards = data.frame(id = character())),
    "cards must contain columns",
    fixed = TRUE
  )
})

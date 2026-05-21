test_that("shared_features finds one matching feature", {
  card1 <- list(colour = "red", animal = "frog", number = 2)
  card2 <- list(colour = "red", animal = "snail", number = 3)

  expect_equal(shared_features(card1, card2), "colour")
})

test_that("shared_features finds multiple matching features", {
  card1 <- list(colour = "blue", animal = "frog", number = 2)
  card2 <- list(colour = "blue", animal = "frog", number = 3)

  expect_equal(shared_features(card1, card2), c("colour", "animal"))
})

test_that("shared_features ignores image paths", {
  card1 <- list(
    image = "cards/first-image.png",
    colour = "yellow",
    animal = "chicken",
    number = 1
  )
  card2 <- list(
    image = "cards/second-image.png",
    colour = "yellow",
    animal = "snail",
    number = 3
  )

  expect_equal(shared_features(card1, card2), "colour")
})

test_that("correct_phase1_choice chooses card 1", {
  card1 <- list(colour = "red", animal = "frog", number = 1)
  card2 <- list(colour = "blue", animal = "snail", number = 2)
  card3 <- list(colour = "yellow", animal = "frog", number = 3)

  expect_equal(correct_phase1_choice(card1, card2, card3), 1)
})

test_that("correct_phase1_choice chooses card 2", {
  card1 <- list(colour = "red", animal = "frog", number = 1)
  card2 <- list(colour = "blue", animal = "snail", number = 2)
  card3 <- list(colour = "yellow", animal = "snail", number = 3)

  expect_equal(correct_phase1_choice(card1, card2, card3), 2)
})

test_that("correct_phase1_choice returns NA when neither card matches", {
  card1 <- list(colour = "red", animal = "frog", number = 1)
  card2 <- list(colour = "blue", animal = "snail", number = 2)
  card3 <- list(colour = "yellow", animal = "chicken", number = 3)

  expect_true(is.na(correct_phase1_choice(card1, card2, card3)))
})

test_that("correct_phase2_choice chooses the card that matches both others", {
  card1 <- list(colour = "red", animal = "frog", number = 1)
  card2 <- list(colour = "blue", animal = "frog", number = 2)
  card3 <- list(colour = "red", animal = "snail", number = 3)

  expect_equal(correct_phase2_choice(card1, card2, card3), 1)
})

test_that("correct_phase2_choice can choose card 2", {
  card1 <- list(colour = "red", animal = "frog", number = 1)
  card2 <- list(colour = "red", animal = "snail", number = 2)
  card3 <- list(colour = "blue", animal = "snail", number = 3)

  expect_equal(correct_phase2_choice(card1, card2, card3), 2)
})

test_that("correct_phase2_choice returns NA when there is no single special card", {
  card1 <- list(colour = "red", animal = "frog", number = 1)
  card2 <- list(colour = "blue", animal = "snail", number = 2)
  card3 <- list(colour = "yellow", animal = "chicken", number = 3)

  expect_true(is.na(correct_phase2_choice(card1, card2, card3)))
})

test_that("card_image_path builds an image path with glue", {
  card <- list(colour = "red", animal = "frog", number = 2)

  expect_equal(card_image_path(card), "cards/red_frog_2.png")
})

#' Find shared card features
#'
#' Compares two cards and returns the names of the features they have in common.
#'
#' @param card_a A named list with card features, such as colour, animal, and number.
#' @param card_b A named list with card features, such as colour, animal, and number.
#' @param features A character vector with the feature names to compare.
#'
#' @return A character vector with the names of the shared features.
#' @export
#'
#' @examples
#' card1 <- list(colour = "red", animal = "frog", number = 2)
#' card2 <- list(colour = "red", animal = "snail", number = 3)
#' shared_features(card1, card2)
shared_features <- function(card_a, card_b, features = c("colour", "animal", "number")) {
  features[vapply(features, function(feature) {
    identical(card_a[[feature]], card_b[[feature]])
  }, logical(1))]
}

#' Find the correct Phase 1 choice
#'
#' Finds which of the first two cards shares at least one feature with the third card.
#'
#' @param card1 A named list with card features.
#' @param card2 A named list with card features.
#' @param card3 A named list with card features.
#'
#' @return `1` if `card1` matches `card3`, `2` if `card2` matches `card3`, or `NA` if neither card matches.
#' @export
#'
#' @examples
#' card1 <- list(colour = "red", animal = "frog", number = 1)
#' card2 <- list(colour = "blue", animal = "snail", number = 2)
#' card3 <- list(colour = "yellow", animal = "frog", number = 3)
#' correct_phase1_choice(card1, card2, card3)
correct_phase1_choice <- function(card1, card2, card3) {
  shared_with_1 <- shared_features(card1, card3)
  shared_with_2 <- shared_features(card2, card3)

  if (length(shared_with_1) > 0) {
    1
  } else if (length(shared_with_2) > 0) {
    2
  } else {
    NA
  }
}

#' Find the correct Phase 2 choice
#'
#' Finds which card shares at least one feature with each of the other two cards.
#'
#' @param card1 A named list with card features.
#' @param card2 A named list with card features.
#' @param card3 A named list with card features.
#'
#' @return `1`, `2`, or `3` for the card that shares something with both other cards,
#'   or `NA` if no card matches the Phase 2 rule.
#' @export
#'
#' @examples
#' card1 <- list(colour = "red", animal = "frog", number = 1)
#' card2 <- list(colour = "blue", animal = "frog", number = 2)
#' card3 <- list(colour = "red", animal = "snail", number = 3)
#' correct_phase2_choice(card1, card2, card3)
correct_phase2_choice <- function(card1, card2, card3) {
  shares_with_both <- c(
    length(shared_features(card1, card2)) > 0 && length(shared_features(card1, card3)) > 0,
    length(shared_features(card2, card1)) > 0 && length(shared_features(card2, card3)) > 0,
    length(shared_features(card3, card1)) > 0 && length(shared_features(card3, card2)) > 0
  )

  matches <- which(shares_with_both)

  if (length(matches) == 1) {
    matches
  } else {
    NA
  }
}

#' Create a card image path
#'
#' Creates the image path for a card from its colour, animal, and number.
#'
#' @param card A named list with colour, animal, and number features.
#' @param folder The folder where card images are stored.
#' @param extension The image file extension.
#'
#' @return A character string with the image path.
#' @importFrom glue glue
#' @export
#'
#' @examples
#' card <- list(colour = "red", animal = "frog", number = 2)
#' card_image_path(card)
card_image_path <- function(card, folder = "cards", extension = "png") {
  as.character(glue("{folder}/{card$colour}_{card$animal}_{card$number}.{extension}"))
}

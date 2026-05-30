#' Find shared card features
#'
#' Compares two cards and returns the names of the requested features they have
#' in common. Image paths and card IDs are ignored unless they are explicitly
#' included in `features`.
#'
#' @param card_a A named list or one-row data frame with card features, such as
#'   colour, animal, and number.
#' @param card_b A named list or one-row data frame with card features, such as
#'   colour, animal, and number.
#' @param features A character vector with the feature names to compare.
#'
#' @return A character vector with the names of the shared features. Returns
#'   `character(0)` when no requested features match.
#' @export
#'
#' @examples
#' frog_card <- list(colour = "red", animal = "frog", number = 2)
#' snail_card <- list(colour = "red", animal = "snail", number = 3)
#' shared_features(frog_card, snail_card)
shared_features <- function(card_a, card_b, features = c("colour", "animal", "number")) {
  features[vapply(features, function(feature) {
    identical(card_a[[feature]], card_b[[feature]])
  }, logical(1))]
}

#' Find the correct Phase 1 choice
#'
#' Finds which of the first two cards shares exactly one feature with the target
#' card. A choice that shares zero features or multiple features is not treated
#' as correct.
#'
#' @param card1 A named list or one-row data frame for the first choice card.
#' @param card2 A named list or one-row data frame for the second choice card.
#' @param card3 A named list or one-row data frame for the target card.
#'
#' @return `1` if `card1` matches `card3`, `2` if `card2` matches `card3`, or `NA` if neither card matches.
#' @export
#'
#' @examples
#' matching_choice <- list(colour = "red", animal = "frog", number = 1)
#' distractor_choice <- list(colour = "blue", animal = "snail", number = 2)
#' target_card <- list(colour = "yellow", animal = "frog", number = 3)
#' correct_phase1_choice(matching_choice, distractor_choice, target_card)
correct_phase1_choice <- function(card1, card2, card3) {
  shared_with_1 <- shared_features(card1, card3)
  shared_with_2 <- shared_features(card2, card3)

  if (length(shared_with_1) == 1) {
    1
  } else if (length(shared_with_2) == 1) {
    2
  } else {
    NA
  }
}

#' Find the correct Phase 2 choice
#'
#' Finds which card shares exactly one feature with each of the other two cards.
#' The two shared features must be different, and the two non-special cards must
#' not share a feature with each other.
#'
#' @param card1 A named list or one-row data frame for the first card.
#' @param card2 A named list or one-row data frame for the second card.
#' @param card3 A named list or one-row data frame for the third card.
#'
#' @return `1`, `2`, or `3` for the card that matches both other cards,
#'   or `NA` if no card matches the Phase 2 rule.
#' @export
#'
#' @examples
#' special_card <- list(colour = "red", animal = "frog", number = 1)
#' animal_match <- list(colour = "blue", animal = "frog", number = 2)
#' colour_match <- list(colour = "red", animal = "snail", number = 3)
#' correct_phase2_choice(special_card, animal_match, colour_match)
correct_phase2_choice <- function(card1, card2, card3) {
  cards <- list(card1, card2, card3)

  matches <- which(vapply(seq_along(cards), function(index) {
    other_indexes <- setdiff(seq_along(cards), index)
    shared_with_first <- shared_features(cards[[index]], cards[[other_indexes[1]]])
    shared_with_second <- shared_features(cards[[index]], cards[[other_indexes[2]]])
    shared_between_others <- shared_features(cards[[other_indexes[1]]], cards[[other_indexes[2]]])

    length(shared_with_first) == 1 &&
      length(shared_with_second) == 1 &&
      !identical(shared_with_first, shared_with_second) &&
      length(shared_between_others) == 0
  }, logical(1))
  )

  if (length(matches) == 1) {
    matches
  } else {
    NA
  }
}

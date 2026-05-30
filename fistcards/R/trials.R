#' Convert a card table row to a card object
#'
#' Pulls one row from the card metadata table and returns it as a named list
#' with the fields used by the scoring helpers.
#'
#' @param cards A card metadata data frame, such as `fist_cards()`.
#' @param index Row number of the requested card.
#'
#' @return A named list with `id`, `colour`, `animal`, `number`, and `image`.
#' @keywords internal
card_row <- function(cards, index) {
  row <- cards[index, , drop = FALSE]
  list(
    id = row$id,
    colour = row$colour,
    animal = row$animal,
    number = row$number,
    image = row$image
  )
}

#' Count shared card features
#'
#' Counts how many task-relevant features two cards have in common.
#'
#' @param card_a,card_b Named lists or one-row data frames with card features.
#'
#' @return An integer count of shared features among colour, animal, and number.
#' @keywords internal
shared_feature_count <- function(card_a, card_b) {
  length(shared_features(card_a, card_b))
}

#' Check whether two option cards are fully distinct
#'
#' Tests whether two cards share no task-relevant features. This is used when
#' constructing Phase 1 answer options.
#'
#' @param card_a,card_b Named lists or one-row data frames with card features.
#'
#' @return `TRUE` when the cards share no colour, animal, or number; otherwise
#'   `FALSE`.
#' @keywords internal
cards_are_distinct_options <- function(card_a, card_b) {
  shared_feature_count(card_a, card_b) == 0
}

#' Sample generated trials
#'
#' Returns all generated trials or a reproducible random subset.
#'
#' @param trials A trial data frame.
#' @param n Optional number of rows to sample. If `NULL`, all rows are returned.
#' @param seed Optional random seed used before sampling.
#'
#' @return A data frame containing either all trials or up to `n` sampled rows.
#' @keywords internal
sample_trials <- function(trials, n, seed) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (is.null(n)) {
    return(trials)
  }

  count <- suppressWarnings(as.integer(n))

  if (length(count) != 1L || is.na(count) || count < 0L || !identical(as.numeric(count), as.numeric(n))) {
    stop("n must be NULL or a single non-negative number.", call. = FALSE)
  }

  trials[sample(seq_len(nrow(trials)), min(count, nrow(trials))), , drop = FALSE]
}

validate_card_deck <- function(cards) {
  required_columns <- c("id", "colour", "animal", "number", "image")
  missing_columns <- setdiff(required_columns, names(cards))

  if (length(missing_columns) > 0L) {
    stop(
      "cards must contain columns: ",
      paste(required_columns, collapse = ", "),
      call. = FALSE
    )
  }
}

empty_phase1_trials <- function() {
  data.frame(
    phase = integer(),
    trial_id = character(),
    target_id = character(),
    choice1_id = character(),
    choice2_id = character(),
    correct_choice = integer(),
    stringsAsFactors = FALSE
  )
}

empty_phase2_trials <- function() {
  data.frame(
    phase = integer(),
    trial_id = character(),
    card1_id = character(),
    card2_id = character(),
    card3_id = character(),
    correct_choice = integer(),
    stringsAsFactors = FALSE
  )
}

#' Generate Phase 1 trials
#'
#' Creates valid Phase 1 trials. Each trial has a target card, two choices, and
#' exactly one choice that shares exactly one feature with the target. The other
#' choice shares no features with the target, and the two choice cards do not
#' share any dimensions with each other.
#'
#' @param cards A card metadata data frame, such as `fist_cards()`.
#' @param n Optional number of trials to sample. If `NULL`, all valid trials are
#'   returned.
#' @param seed Optional random seed used when sampling trials.
#'
#' @return A data frame with columns `phase`, `trial_id`, `target_id`,
#'   `choice1_id`, `choice2_id`, and `correct_choice`.
#' @export
#'
#' @examples
#' trials <- generate_phase1_trials(n = 10, seed = 1)
#' nrow(trials)
#' head(trials)
generate_phase1_trials <- function(cards = fist_cards(), n = NULL, seed = NULL) {
  validate_card_deck(cards)

  trials <- list()
  trial_index <- 1

  for (target_index in seq_len(nrow(cards))) {
    target <- card_row(cards, target_index)
    other_indexes <- setdiff(seq_len(nrow(cards)), target_index)

    match_indexes <- other_indexes[vapply(other_indexes, function(index) {
      shared_feature_count(card_row(cards, index), target) == 1
    }, logical(1))]

    nonmatch_indexes <- other_indexes[vapply(other_indexes, function(index) {
      shared_feature_count(card_row(cards, index), target) == 0
    }, logical(1))]

    for (match_index in match_indexes) {
      for (nonmatch_index in nonmatch_indexes) {
        if (!cards_are_distinct_options(card_row(cards, match_index), card_row(cards, nonmatch_index))) {
          next
        }

        orderings <- list(c(match_index, nonmatch_index), c(nonmatch_index, match_index))

        for (ordering in orderings) {
          trials[[trial_index]] <- data.frame(
            phase = 1L,
            trial_id = sprintf("phase1_%04d", trial_index),
            target_id = target$id,
            choice1_id = cards$id[ordering[1]],
            choice2_id = cards$id[ordering[2]],
            correct_choice = which(ordering == match_index),
            stringsAsFactors = FALSE
          )
          trial_index <- trial_index + 1
        }
      }
    }
  }

  if (length(trials) == 0L) {
    return(sample_trials(empty_phase1_trials(), n, seed))
  }

  sample_trials(do.call(rbind, trials), n, seed)
}

#' Generate Phase 2 trials
#'
#' Creates valid Phase 2 trials. Each trial has three cards and exactly one card
#' that shares exactly one feature with each of the other two cards. The shared
#' feature must be different for each pair, and the two non-special cards must
#' not match each other.
#'
#' @param cards A card metadata data frame, such as `fist_cards()`.
#' @param n Optional number of trials to sample. If `NULL`, all valid trials are
#'   returned.
#' @param seed Optional random seed used when sampling trials.
#'
#' @return A data frame with columns `phase`, `trial_id`, `card1_id`,
#'   `card2_id`, `card3_id`, and `correct_choice`.
#' @export
#'
#' @examples
#' trials <- generate_phase2_trials(n = 10, seed = 1)
#' nrow(trials)
#' head(trials)
generate_phase2_trials <- function(cards = fist_cards(), n = NULL, seed = NULL) {
  validate_card_deck(cards)

  if (nrow(cards) < 3L) {
    return(sample_trials(empty_phase2_trials(), n, seed))
  }

  combinations <- utils::combn(seq_len(nrow(cards)), 3, simplify = FALSE)
  trials <- list()
  trial_index <- 1

  for (indexes in combinations) {
    correct_choice <- correct_phase2_choice(
      card_row(cards, indexes[1]),
      card_row(cards, indexes[2]),
      card_row(cards, indexes[3])
    )

    if (!is.na(correct_choice)) {
      trials[[trial_index]] <- data.frame(
        phase = 2L,
        trial_id = sprintf("phase2_%04d", trial_index),
        card1_id = cards$id[indexes[1]],
        card2_id = cards$id[indexes[2]],
        card3_id = cards$id[indexes[3]],
        correct_choice = correct_choice,
        stringsAsFactors = FALSE
      )
      trial_index <- trial_index + 1
    }
  }

  if (length(trials) == 0L) {
    return(sample_trials(empty_phase2_trials(), n, seed))
  }

  sample_trials(do.call(rbind, trials), n, seed)
}

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

shared_feature_count <- function(card_a, card_b) {
  length(shared_features(card_a, card_b))
}

cards_are_distinct_options <- function(card_a, card_b) {
  shared_feature_count(card_a, card_b) == 0
}

sample_trials <- function(trials, n, seed) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (is.null(n)) {
    return(trials)
  }

  trials[sample(seq_len(nrow(trials)), min(n, nrow(trials))), , drop = FALSE]
}

#' Generate Phase 1 trials
#'
#' Creates valid Phase 1 trials. Each trial has a target card, two choices,
#' and exactly one choice that shares exactly one feature with the target. The
#' two choice cards do not share any dimensions with each other.
#'
#' @param cards A card metadata data frame, such as `fist_cards()`.
#' @param n Optional number of trials to sample.
#' @param seed Optional random seed used when sampling trials.
#'
#' @return A data frame of Phase 1 trials.
#' @export
#'
#' @examples
#' trials <- generate_phase1_trials(n = 10, seed = 1)
#' nrow(trials)
generate_phase1_trials <- function(cards = fist_cards(), n = NULL, seed = NULL) {
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

  sample_trials(do.call(rbind, trials), n, seed)
}

#' Generate Phase 2 trials
#'
#' Creates valid Phase 2 trials. Each trial has three cards and exactly one
#' card that shares exactly one feature with each of the other two cards.
#'
#' @param cards A card metadata data frame, such as `fist_cards()`.
#' @param n Optional number of trials to sample.
#' @param seed Optional random seed used when sampling trials.
#'
#' @return A data frame of Phase 2 trials.
#' @export
#'
#' @examples
#' trials <- generate_phase2_trials(n = 10, seed = 1)
#' nrow(trials)
generate_phase2_trials <- function(cards = fist_cards(), n = NULL, seed = NULL) {
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

  sample_trials(do.call(rbind, trials), n, seed)
}

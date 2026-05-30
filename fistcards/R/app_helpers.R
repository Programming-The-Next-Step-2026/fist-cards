trial_columns <- c(
  "phase", "trial_id", "target_id", "choice1_id", "choice2_id",
  "card1_id", "card2_id", "card3_id", "correct_choice"
)

build_trials <- function(phase1_n, phase2_n) {
  phase1_trials <- generate_phase1_trials(n = phase1_n)
  phase2_trials <- generate_phase2_trials(n = phase2_n)

  phase1_trials$card1_id <- rep(NA_character_, nrow(phase1_trials))
  phase1_trials$card2_id <- rep(NA_character_, nrow(phase1_trials))
  phase1_trials$card3_id <- rep(NA_character_, nrow(phase1_trials))
  phase2_trials$target_id <- rep(NA_character_, nrow(phase2_trials))
  phase2_trials$choice1_id <- rep(NA_character_, nrow(phase2_trials))
  phase2_trials$choice2_id <- rep(NA_character_, nrow(phase2_trials))

  rbind(
    phase1_trials[, trial_columns],
    phase2_trials[, trial_columns]
  )
}

valid_trial_count <- function(value) {
  count <- suppressWarnings(as.integer(value))

  if (length(count) == 0L || is.na(count)) {
    return(0L)
  }

  max(0L, count)
}

sanitize_csv_text <- function(value) {
  value <- as.character(value)
  dangerous <- grepl("^[=+@-]", value)
  value[dangerous] <- paste0("'", value[dangerous])
  value
}

results_summary <- function(data) {
  if (nrow(data) == 0L) {
    return(list(total = 0L, correct = 0L, percent = NA_real_, mean_rt = NA_real_))
  }

  correct <- sum(data$accuracy, na.rm = TRUE)
  list(
    total = nrow(data),
    correct = correct,
    percent = round(100 * correct / nrow(data), 1),
    mean_rt = round(mean(data$reaction_time, na.rm = TRUE), 2)
  )
}

empty_results <- function() {
  data.frame(
    participant_id = character(),
    phase = integer(),
    trial_id = character(),
    trial_number = integer(),
    cards_shown = character(),
    correct_choice = integer(),
    chosen_choice = integer(),
    accuracy = logical(),
    reaction_time = numeric(),
    timestamp = character(),
    stringsAsFactors = FALSE
  )
}

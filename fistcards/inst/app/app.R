library(shiny)
library(fistcards)

card_deck <- fist_cards()
trial_columns <- c(
  "phase", "trial_id", "target_id", "choice1_id", "choice2_id",
  "card1_id", "card2_id", "card3_id", "correct_choice"
)

build_trials <- function(phase1_n, phase2_n) {
  phase1_trials <- generate_phase1_trials(n = phase1_n)
  phase2_trials <- generate_phase2_trials(n = phase2_n)

  phase1_trials$card1_id <- NA_character_
  phase1_trials$card2_id <- NA_character_
  phase1_trials$card3_id <- NA_character_
  phase2_trials$target_id <- NA_character_
  phase2_trials$choice1_id <- NA_character_
  phase2_trials$choice2_id <- NA_character_

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

card_by_id <- function(id) {
  card_deck[card_deck$id == id, , drop = FALSE]
}

card_button <- function(input_id, card) {
  actionButton(
    input_id,
    label = NULL,
    class = "card-button",
    icon = tags$img(src = card$image, alt = card$id)
  )
}

demo_card <- function(card, label = NULL, highlight = FALSE) {
  classes <- if (highlight) "demo-card demo-card-correct" else "demo-card"
  div(
    class = classes,
    tags$img(src = card$image, alt = card$id),
    if (!is.null(label)) div(class = "demo-label", label)
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

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: #f7f7f4;
        color: #242424;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }

      .app-shell {
        max-width: 980px;
        margin: 0 auto;
        padding: 24px 16px 40px;
      }

      .top-row {
        align-items: center;
        display: flex;
        justify-content: space-between;
        margin-bottom: 18px;
      }

      .trial-title {
        font-size: 22px;
        font-weight: 700;
        margin: 0;
      }

      .progress-text {
        color: #555;
        font-size: 15px;
      }

      .target-area,
      .choice-area,
      .demo-row {
        display: flex;
        gap: 22px;
        justify-content: center;
        margin: 24px 0;
      }

      .target-card {
        max-width: 310px;
        width: 42vw;
      }

      .target-card img {
        border: 2px solid #222;
        border-radius: 8px;
        width: 100%;
      }

      .card-button {
        background: #fff;
        border: 3px solid #1f1f1f;
        border-radius: 8px;
        box-shadow: 0 3px 0 #1f1f1f;
        height: auto;
        max-width: 260px;
        padding: 0;
        transition: transform 120ms ease, box-shadow 120ms ease;
        width: 30vw;
      }

      .card-button:hover,
      .card-button:focus {
        background: #fff;
        border-color: #0b6bcb;
        box-shadow: 0 3px 0 #0b6bcb;
        transform: translateY(-1px);
      }

      .card-button img {
        border-radius: 5px;
        display: block;
        width: 100%;
      }

      .instruction-panel {
        margin: 36px auto;
        max-width: 860px;
      }

      .instruction-panel h1 {
        font-size: 34px;
        margin-bottom: 14px;
      }

      .instruction-panel p {
        font-size: 20px;
        line-height: 1.45;
        margin: 8px 0;
      }

      .demo-card {
        background: #fff;
        border: 3px solid #222;
        border-radius: 8px;
        max-width: 230px;
        padding: 0;
        width: 28vw;
      }

      .demo-card-correct {
        border-color: #0b6bcb;
        box-shadow: 0 0 0 4px rgba(11, 107, 203, 0.18);
      }

      .demo-card img {
        border-radius: 5px;
        display: block;
        width: 100%;
      }

      .demo-label {
        background: #f0f5fb;
        border-top: 2px solid #d5e3f2;
        color: #1f1f1f;
        font-size: 15px;
        font-weight: 700;
        padding: 8px;
        text-align: center;
      }

      .start-panel,
      .done-panel {
        margin: 72px auto;
        max-width: 520px;
      }

      .trial-counts {
        display: grid;
        gap: 12px;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        margin-top: 12px;
      }

      .primary-action {
        margin-top: 14px;
      }

      @media (max-width: 700px) {
        .choice-area {
          gap: 12px;
        }

        .demo-row {
          gap: 12px;
        }

        .card-button {
          width: 44vw;
        }

        .target-card {
          width: 58vw;
        }

        .demo-card {
          width: 30vw;
        }

        .trial-counts {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  div(
    class = "app-shell",
    uiOutput("screen")
  )
)

server <- function(input, output, session) {
  started <- reactiveVal(FALSE)
  complete <- reactiveVal(FALSE)
  screen_mode <- reactiveVal("start")
  participant_id <- reactiveVal("")
  trial_index <- reactiveVal(1L)
  trial_started_at <- reactiveVal(Sys.time())
  active_trials <- reactiveVal(build_trials(8, 8))
  results <- reactiveVal(empty_results())

  observeEvent(input$start, {
    phase1_n <- valid_trial_count(input$phase1_trials)
    phase2_n <- valid_trial_count(input$phase2_trials)

    participant_id(input$participant_id)
    active_trials(build_trials(phase1_n, phase2_n))
    started(TRUE)
    complete(phase1_n + phase2_n == 0L)
    screen_mode(if (phase1_n > 0L) {
      "phase1_intro"
    } else if (phase2_n > 0L) {
      "phase2_intro"
    } else {
      "done"
    })
    trial_index(1L)
    results(empty_results())
  })

  observeEvent(input$begin_phase1, {
    screen_mode("trial")
    trial_started_at(Sys.time())
  })

  observeEvent(input$begin_phase2, {
    screen_mode("trial")
    trial_started_at(Sys.time())
  })

  current_trial <- reactive({
    active_trials()[trial_index(), , drop = FALSE]
  })

  shown_cards <- function(trial) {
    if (trial$phase == 1L) {
      c(trial$target_id, trial$choice1_id, trial$choice2_id)
    } else {
      c(trial$card1_id, trial$card2_id, trial$card3_id)
    }
  }

  record_choice <- function(choice) {
    trial <- current_trial()
    timestamp <- Sys.time()
    response <- data.frame(
      participant_id = participant_id(),
      phase = trial$phase,
      trial_id = trial$trial_id,
      trial_number = trial_index(),
      cards_shown = paste(shown_cards(trial), collapse = ","),
      correct_choice = trial$correct_choice,
      chosen_choice = choice,
      accuracy = choice == trial$correct_choice,
      reaction_time = as.numeric(difftime(timestamp, trial_started_at(), units = "secs")),
      timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )

    results(rbind(results(), response))

    if (trial_index() >= nrow(active_trials())) {
      complete(TRUE)
    } else {
      next_index <- trial_index() + 1L
      next_trial <- active_trials()[next_index, , drop = FALSE]
      trial_index(next_index)

      if (trial$phase == 1L && next_trial$phase == 2L) {
        screen_mode("phase2_intro")
      } else {
        trial_started_at(Sys.time())
      }
    }
  }

  observeEvent(input$choice1, record_choice(1L))
  observeEvent(input$choice2, record_choice(2L))
  observeEvent(input$choice3, record_choice(3L))

  output$screen <- renderUI({
    if (!started()) {
      return(div(
        class = "start-panel",
        h1("FIST Cards"),
        textInput("participant_id", "Participant ID", value = ""),
        div(
          class = "trial-counts",
          numericInput("phase1_trials", "First game trials", value = 8, min = 0, step = 1),
          numericInput("phase2_trials", "Second game trials", value = 8, min = 0, step = 1)
        ),
        actionButton("start", "Start", class = "btn-primary primary-action")
      ))
    }

    if (screen_mode() == "phase1_intro") {
      target <- card_by_id("blue_frog_1")
      correct <- card_by_id("yellow_frog_3")
      other <- card_by_id("red_snail_2")

      return(div(
        class = "instruction-panel",
        h1("First game"),
        p("Look at the card on top."),
        p("Pick the card below that is the same in only one way: colour, animal, or number."),
        div(class = "demo-row", demo_card(target, "Look at this card")),
        div(
          class = "demo-row",
          demo_card(correct, "Pick this one", highlight = TRUE),
          demo_card(other, "Not this one")
        ),
        p("In this example, the two frog cards go together because they have the same animal."),
        actionButton("begin_phase1", "Start first game", class = "btn-primary primary-action")
      ))
    }

    if (screen_mode() == "phase2_intro") {
      special <- card_by_id("red_frog_1")
      first <- card_by_id("blue_frog_2")
      second <- card_by_id("red_snail_3")

      return(div(
        class = "instruction-panel",
        h1("Second game"),
        p("Now find the card that goes with both other cards."),
        p("The answer matches one card in one way, and the other card in a different way."),
        div(
          class = "demo-row",
          demo_card(special, "Pick this one", highlight = TRUE),
          demo_card(first, "Same animal"),
          demo_card(second, "Same colour")
        ),
        p("In this example, the red frog goes with the blue frog because both are frogs, and with the red snail because both are red."),
        actionButton("begin_phase2", "Start second game", class = "btn-primary primary-action")
      ))
    }

    if (complete()) {
      return(div(
        class = "done-panel",
        h1("Finished"),
        downloadButton("download_results", "Download results")
      ))
    }

    trial <- current_trial()

    if (trial$phase == 1L) {
      target <- card_by_id(trial$target_id)
      choice1 <- card_by_id(trial$choice1_id)
      choice2 <- card_by_id(trial$choice2_id)

      return(tagList(
        div(
          class = "top-row",
          h2(class = "trial-title", "Find the card that goes with this one"),
          div(class = "progress-text", paste(trial_index(), "of", nrow(active_trials())))
        ),
        div(
          class = "target-area",
          div(class = "target-card", tags$img(src = target$image, alt = target$id))
        ),
        div(
          class = "choice-area",
          card_button("choice1", choice1),
          card_button("choice2", choice2)
        )
      ))
    }

    card1 <- card_by_id(trial$card1_id)
    card2 <- card_by_id(trial$card2_id)
    card3 <- card_by_id(trial$card3_id)

    tagList(
      div(
        class = "top-row",
        h2(class = "trial-title", "Find the card that goes with both others"),
        div(class = "progress-text", paste(trial_index(), "of", nrow(active_trials())))
      ),
      div(
        class = "choice-area",
        card_button("choice1", card1),
        card_button("choice2", card2),
        card_button("choice3", card3)
      )
    )
  })

  output$download_results <- downloadHandler(
    filename = function() {
      paste0("fist_results_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

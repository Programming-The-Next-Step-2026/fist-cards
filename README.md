# FIST-cards

Online version containing Phase 1 and Phase 2 of the FIST (Flexible Item Selection Task) card task for young
children.

The project is organized as an R package with a Shiny app. It includes helper
functions for creating the card deck, generating valid trials, checking correct
answers, running the app, and exporting participant results.

## What the app does

- Shows Phase 1 trials where the participant matches one card to a target card.
- Shows Phase 2 trials where the participant finds the card that matches both
  other cards in different ways.
- Records participant ID, phase, trial number, cards shown, selected choice,
  accuracy, reaction time, and timestamp.
- Shows a thank-you screen at the end of the game.
- Shows a results summary and lets the researcher download the full CSV.

## Installation

Install the package from GitHub with:

```r
install.packages("remotes")
remotes::install_github(
  "Programming-The-Next-Step-2026/fist-cards",
  subdir = "fistcards"
)
```

## Running the App

Run the game from R with:

```r
library(fistcards)
run_fist_game()
```

If you already downloaded the repository and are working from the repository
root, you can load the local package during development with:

```r
devtools::load_all("fistcards")
run_fist_game()
```

## Project Structure

- `fistcards/R/cards.R`: card metadata and image paths.
- `fistcards/R/card_logic.R`: Phase 1 and Phase 2 matching rules.
- `fistcards/R/trials.R`: valid trial generation.
- `fistcards/R/run_fist_game.R`: package function for launching the Shiny app.
- `fistcards/inst/app/app.R`: Shiny user interface and server logic.
- `fistcards/inst/app/www/cards/`: card image assets.
- `fistcards/tests/testthat/`: unit tests and Shiny click-through tests.
- `fistcards/vignettes/`: project report materials.

## Testing

Run the test suite with:

```r
devtools::test("fistcards")
```

Run the full package check, including the vignette, with:

```r
devtools::check("fistcards")
```

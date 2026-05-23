# fist-cards
Online version containing Phase 1 and 2 of the FIST card task for young children

## Week 3 game work

This branch adds the cropped FIST cards as app assets and includes helpers for
creating valid Phase 1 and Phase 2 trials.

From R, run the game with:

```r
devtools::load_all("fistcards")
run_fist_game()
```

The Shiny app records participant ID, phase, trial number, shown cards, selected
choice, accuracy, reaction time, and timestamp. Results can be downloaded as a
CSV at the end of the game.

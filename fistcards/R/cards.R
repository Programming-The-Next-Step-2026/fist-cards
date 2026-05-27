#' List the FIST card deck
#'
#' Creates metadata for the full set of FIST cards used by the game.
#'
#' @param image_folder Folder path used in the image column.
#'
#' @return A data frame with one row per card.
#' @export
#'
#' @examples
#' cards <- fist_cards()
#' nrow(cards)
fist_cards <- function(image_folder = "cards") {
  colours <- c("blue", "red", "yellow")
  animals <- c("frog", "snail", "chicken")
  numbers <- 1:3

  cards <- expand.grid(
    colour = colours,
    animal = animals,
    number = numbers,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  cards$id <- paste(cards$colour, cards$animal, cards$number, sep = "_")
  cards$image <- card_image_path(cards, folder = image_folder)
  cards[, c("id", "colour", "animal", "number", "image")]
}

#' Find a FIST card
#'
#' Finds one card in a FIST card deck by its features.
#'
#' @param colour,animal,number Card features.
#' @param cards A card metadata data frame, such as `fist_cards()`.
#'
#' @return A one-row data frame.
#' @export
#'
#' @examples
#' find_card("blue", "frog", 1)
find_card <- function(colour, animal, number, cards = fist_cards()) {
  match <- cards$colour == colour & cards$animal == animal & cards$number == number
  cards[match, , drop = FALSE]
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

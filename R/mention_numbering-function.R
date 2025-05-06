mention_numbering <- function(mentions){
  paste0(
      ifelse(
        mentions == mentions[length(mentions)] & length(mentions) > 1
        , "and (", "("
      )
      , LETTERS[seq(length(mentions))]
      , ") "
      , str_to_upper(mentions)
      , ifelse(mentions == mentions[length(mentions)], ". ", "; ")
    ) |>
    paste0(collapse = "")
}
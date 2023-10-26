#' Keyword-in-context searches
#'
#' For a text or a collection of texts stored in a tibble, return a
#' list of a keyword in its immediate context.
#' 
#' @param x A data frame or tibble that contains a column called "text" that contains the text that should be searched for keywords.
#' @param keyword a string that represents the keyword
#' @param window the number of context words to be displayed around the keyword
#'
#' @return a [tibble][tibble::tibble-package] 
#' @export
#' 
#' @example
#' text_data <- data.frame(author = c("William Shakespeare", "William Wordsworth"), 
#'.                        source = c("Hamlet", "The Tables Turned"),
#'                         text = c("To be, or not to be: that is the question.", 
#'                                   "Come forth into the light of things, Let Nature be your teacher."))
#'                              
#' tidy_kwic(text_data, 
#'           keyword = "be",
#'           window = 6)
tidy_kwic <- function(x, keyword, window = 6){
  
  # Ensure correct split of window
  if(window %% 2 != 0) {
    window <- window+1
  }
  
  # Create keyword position name
  target <- paste0("word", window/2)
  
  # Conduct actual keyword search
  kwic <- x |>
    tidytext::unnest_tokens(trigram, text, token = "ngrams", n = window) |>
    filter(str_detect(trigram, keyword)) |>
    separate(trigram, paste0("word", 1:window), sep = " ") |>
    filter(str_detect(!!as.name(target), keyword)) %>%
    unite(pre, c(paste0("word", 1:(window/2-1))), sep = " ") |>
    unite(post, c(paste0("word", (window/2+1):window)), sep = " ")
  
  # Format results
  result <- kwic |>
    as.data.frame() |>
    mutate(
      pre = format(pre, justify = "right"),
      s1 = rep("|", nrow(kwic)),
      target = format(!!as.name(target), justify = "centre"),
      s2 = rep("|", nrow(kwic)),
      post = format(post, justify = "left")
    ) |>
    select(pre, s1, target, s2, post, everything()) 
  
  return(result)
}

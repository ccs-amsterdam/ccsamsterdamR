#' Wrapper for the Open AI API
#'
#' Allows to access OPEN AI's GPT-models and run typical text analysis prompts using zero-shot classification.
#' 
#' @param txt A vector with strings representing text. 
#' @param prompt A string representing the prompt that is sent to the GPT model. Can be NULL. 
#' @param labels A vector with n values representing the classes the GPT should annotate the texts with. 
#' @param sep As GPT only text a string, the labels provided in the vector are collapsed into one string separated by this character. Defaults to " :||: ", which seems to work well and is unlikely to be part of a text. 
#' @param expertise A string that will be added to the prompt up front that could help to "prompt engineer" the model to do the task better, e.g., "You are an expert identifying topics from newspaper articles."
#' @param output A string in which you explain the type of output format you want to get. Default is "basic", which returns in standard data frame (tidy tibble format).
#' @param certainty If TRUE, the output contains a second column with a numerical value expressing GPT's certainty in classifying the text. 
#' @param justification If TRUE, the output contains another column that includes a short justification for the classification. 
#' @param n_justification Maxium length of the justification in words. Defaults to 20. 
#' @param model Which model should be used. Defaults to "gpt-3.5-turbo". For a list of available models, see: https://platform.openai.com/docs/models
#'
#' @examples
#' \dontrun{
#' Sys.setenv(OPENAI_API_KEY = 'XXX') # Provide access token, otherwise, it won't work
#' 
#' text = c("To be, or not to be: that is the question.", 
#'          "An atom is a particle that consists of a nucleus of protons and neutrons surrounded by an electromagnetically-bound cloud of electrons.",
#'          "Senate passes stopgap bill to avert government shutdown.")
#' 
#' gpt_api(txt = text, 
#'         labels = c("Poetry", "Politics", "Physics"))
#'
#'
#' text2 = c("This movie sucks. I really don't like it. Boring as hell, and no story whatsoever!", 
#'           "This film really struck a nerve for me. Fantastic acting too",
#'           "This movie is about a physicist who invented the atomic bomb.")
#'
#' gpt_sentiment(txt = text2)
#' }
#' @export
gpt_api <- function(txt, 
                     prompt = NULL,
                     labels = NULL,
                     sep = "|:||:|",
                     expertise = NULL,
                     certainty = FALSE,
                     justification = FALSE,
                     model = "gpt-3.5-turbo",
                     ...) {
  
  require(openai)
  require(tidyverse)
  
  # Prompt engineering
  if(is.null(prompt)) {
    prompt <- paste("You classify the texts given to you, which are separated by a", sep, "as either", paste(labels, collapse = ","), 
                    "Provide one label per text. As output, provide a data frame in csv format that can be read using 'read_csv' in R.
                     The first column labelled 'labels' should contain the classifications.")
  }
  
  if(!is.null(expertise)) {
    prompt <- paste(expertise, prompt)
  }
  
  if(isTRUE(certainty)) {
    prompt <- paste(prompt, "Provide the probability for your choice per text in another column called 'certainty'.")
  }
  
  if(isTRUE(justification)) {
    prompt <- paste(prompt, "Provide a justification for your choice in another column called 'justification'.  
                             Use only 15 words for this justification.")
  } 
  
  prompt <- paste(prompt, "The data set MUST include one label for ALL original texts. NEVER distinguish texts based on something else than the delimiter", sep, "
                           You ALWAYS classifiy all of the original input texts and never remove any classification from your output!")
  
  
  response <- create_chat_completion(
    model = model,
    messages = list(
      list(
        "role" = "system",
        "content" = prompt
      ),
      list(
        "role" = "user",
        "content" = paste(txt, collapse = sep)
      )
    ),
    ...
  )
  
  
  output <- read_csv(response$choices$message.content)
  
  return(output)
}



#' @export
gpt_sentiment <- function(txt, 
                          expertise = "You are an expert in classifying the sentiment of a text.",
                          sentiment = c("positive", "neutral", "negative"),
                          model = "gpt-3.5-turbo",
                          ...) {
  output <- gpt_api(txt = txt, 
                    expertise = expertise,
                    labels = sentiment, 
                    model = model,
                    ...)
  return(output)
}


#' @export
gpt_zeroshot <- function(txt, 
                         labels,
                         model = "gpt-3.5-turbo",
                         ...) {
  output <- gpt_api(txt = txt, 
                    labels = labels,
                    model = model,
                    ...)
  return(output)
}



#' @export
gpt_split_data <- function(data, n_per_group = 2) {
  
  if((nrow(data) %% 2) == 0) {
    lst <- data |> 
      mutate(group = c(rep(c(1:(nrow(data)/n_per_group)), each = n_per_group))) %>% 
      split(.[, "group"])
    
  } else {
    lst <- data |> 
      mutate(group = c(c(rep(c(1:(nrow(data)/n_per_group)), each = n_per_group)), nrow(data)/n_per_group+1)) %>% 
      split(.[, "group"])
  }
  
  return(lst)
  
}




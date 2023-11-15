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
#'          "Come forth into the light of things, Let Nature be your teacher.")
#' 
#' gpt_api(txt = text, 
#'         labels = c("Poetry", "Politics", "Science"),
#'         temperature = 2)
#' }
#' @export
gpt_api <- function(txt, 
                    prompt = NULL,
                    labels = NULL,
                    sep = " :||: ",
                    expertise = NULL,
                    output = "basic",
                    certainty = FALSE,
                    justification = FALSE,
                    n_justification = 20,
                    model = "gpt-3.5-turbo",
                    temperature = 1,
                    top_p = 1) {
  
    require(openai)
    require(tidyverse)
  
    # Prompt engineering
    if(is.null(prompt)) {
      prompt <- paste("Please classify the following texts, which are separated by a ", sep, ", as either", paste(labels, collapse = ","), "Only provide one label per text.")
    } else {
      prompt <- paste(prompt, paste(labels, collapse = ","))
    }
  
    if(output == "basic") {
      prompt <- paste(prompt, "As output, provide a data frame in csv format that can be read using 'read_csv' in R (the separator should be ','). 
                               The first column named 'labels' should contain the classifications.")
    } else {
      prompt <- paste(prompt, output)
    }
  
    if(is.null(expertise)) {
      prompt <- paste(expertise, prompt)
    }
  
    if(isTRUE(certainty)) {
      prompt <- paste(prompt, "A second column called 'certainty' contains a probability score reflecting your certainty in the choice (ranging from 0 to 1).")
    }
  
    if(isTRUE(justification) & isTRUE(certainty)) {
      prompt <- paste(prompt, "In a third column called 'justification', you provide a short justification or explanation for your choice. 
                               Keep it short and use at maximum", n_justification, "words.")
    } else if(isTRUE(justification) & isFALSE(certainty)) {
      prompt <- paste(prompt, "In a second column called 'justification' you provide a short justification or explanation for your choice. 
                               Keep it short and use at maximum", n_justification, "words.")
    } else {
      prompt <- paste(prompt, "Do not include any other columns next to the labels column.")
    }
  
    response <- create_chat_completion(
      model = model,
      messages = list(
        list(
          "role" = "system",
          "content" = prompt
          ),
        list(
          "role" = "user",
          "content" = paste(txt, collapse = "|")
        )
      ),
      temperature = temperature,
      top_p = top_p
    )
    
    if(output == "basic") {
      output <- read_csv(response$choices$message.content)
    } else {
      outpu <- response$choices$message.content
    }
    
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





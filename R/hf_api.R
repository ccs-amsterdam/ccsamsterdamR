#' Wrapper for the Huggingface Inference API
#'
#' Allows to access the Huggingface API and run typical text analysis prompts with huggingface models.
#' 
#' @param inputs A vector with strings representing text. 
#' @param url link to a model uploaded to huggingface (e.g., https://api-inference.huggingface.co/models/facebook/bart-large-mnli)
#' @param ... further parameters that to extract
#'
#' @examples
#' \dontrun{
#' Sys.setenv(HF_API_TOKEN = "XXX") # Provide access token, if not only very limited rates are available
#' 
#' text = c("To be, or not to be: that is the question.", 
#'          "An atom is a particle that consists of a nucleus of protons and neutrons surrounded by an electromagnetically-bound cloud of electrons.",
#'          "Senate passes stopgap bill to avert government shutdown.")
#' 
#' hf_api(inputs = text, 
#'        url = "https://api-inference.huggingface.co/models/MoritzLaurer/deberta-v3-large-zeroshot-v1", 
#'        parameters = list(candidate_labels = c("Poetry", "Politics", "Science")))
#' }
#' @export
hf_api = function(inputs = NULL, url = NULL, filename = NULL, ...){
  
  # Code adopted from: https://github.com/ccsmainz/hfapi/blob/main/R/hf_api.R
  # Original by Michael Scharkow
  
  require(httr);require(jsonlite)
  hf_opts = list()
  if (Sys.getenv("HF_API_TOKEN") != "") {
    config = add_headers(Authorization = paste("Bearer", Sys.getenv("HF_API_TOKEN")))
  } else {
    config = NULL
  }
  
  params = c(list(inputs = inputs), list(...))
  
  if(!is.null(filename)){
    body = read_file_raw(filename)
  } else {
    body = jsonlite::toJSON(c(params, hf_opts))
  }
  
  req = POST(url, body = body, config = config)
  response = fromJSON(content(req, "text"))
  if(is.list(response) && !is.null(response$estimated_time)){
    print(paste("Waiting", round(as.numeric(response$estimated_time)), "seconds for model to load."))
    Sys.sleep(response$estimated_time)
    hf_api(inputs, url, filename, ...)
  } else {
    response
  }
}


#' @export
hf_embeddings = function(txt, url = "https://api-inference.huggingface.co/pipeline/feature-extraction/sentence-transformers/all-MiniLM-L12-v2"){
  hf_api(inputs = txt, url = url) |> 
    as.tibble()
}

#' @export
hf_zeroshot = function(txt, labels, url = "https://api-inference.huggingface.co/models/facebook/bart-large-mnli"){
  hf_api(inputs = txt, url = url, parameters = list(candidate_labels = labels)) |> 
    as_tibble()
}


#' @export
hf_image_classification = function(filename, url = "https://api-inference.huggingface.co/models/google/vit-base-patch16-224"){
  hf_api(filename = filename, url = url) %>%
    as_tibble()
}
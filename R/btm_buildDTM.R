#' Build DTM
#'
#' @param data List of data from `btm_annotate()` function
#'
#' @return DocumentTermMatrix object for use with topicmodels package
#' @export
#'
#' @examples
#' \dontrun{
#' dtm <- btm_buildDTM(NAV_reviews)
#' }
btm_buildDTM <- function(data){
  x <- data$model_traindata
  x <- stats::aggregate(x$lemma, by = list(x$doc_id), paste, collapse = " ")
  names(x) <- c("doc_id", "lemma")

  ## DTM for coherence calculation ##
  corpus <- quanteda::corpus(x$lemma)
  DFM <- quanteda::dfm(quanteda::tokens(corpus, remove_punct = TRUE)) # edit parameters of tokens() to your needs, e. g. removal of separators and punctuation
  DTM <- quanteda::convert(DFM, to = "topicmodels")
}

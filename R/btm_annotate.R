#' Annotate text
#'
#' @description Using the [udpipe](https://cran.r-project.org/web/packages/udpipe/index.html) pacakge to annotate text
#'
#' @param data Data frame; the column to be annotated must be named `text`
#' @param modeltype Type of BTM model that we want to run (noun, adjective, verb; noun, proper noun; adjective only)
#'
#' @return A list with original data, annotated text, biterm co-occurrence counts by document, and a subset of annotated data based upon model type
#' @export
#'
#' @examples
#' \dontrun{
#' btm_annotate(text_data, "NAV")
#' }
btm_annotate <- function(data, modeltype = c('NAV', "NPN", "ADJ")) {
  if (!"text" %in% tolower(colnames(data))) stop("No 'text' column")
  .datatable.aware=TRUE

  dataset <- data |>
    janitor::clean_names() |>
    dplyr::mutate(doc_id = paste0("doc_", dplyr::row_number())) |>
    dplyr::ungroup()

  # clean up text and keep only text and doc_id -----------------------------
  txt <- dataset |>
    dplyr::select(doc_id, text) |>
    dplyr::mutate(text = iconv(text, "latin1", "ASCII", sub=""),
           text = tolower(text),
           text = qdapRegex::rm_url(text),
           text = gsub('@\\S+', '', text), ## Remove Mentions
           text = trimws(gsub('[[:punct:]]', "", text))) |>
    dplyr::ungroup()

  # load udpipe model -------------------------------------------------------
  ud_model <- udpipe::udpipe_download_model(language = "english", overwrite = FALSE)
  ud_model <- udpipe::udpipe_load_model(ud_model$file_model)

  # annotate with udpipe ----------------------------------------------------
  anno <- udpipe::udpipe(txt, "english", trace = FALSE, parallel.cores = 4)
  biterms <- data.table::as.data.table(anno)

  # model choice  -------------------------------------

  if (modeltype == "NAV") {
    # set up noun,  adjective,  verb data
    model_biterms <- biterms[, udpipe::cooccurrence(lemma, relevant = (upos %in% c("NOUN","ADJ", "VERB") & nchar(lemma) > 2 & !lemma %in% stopwords::stopwords("en")), skipgram = 3), by = list(doc_id)]
    #
    model_traindata <- subset(anno, upos %in% c("NOUN", "ADJ", "VERB") & !lemma %in% stopwords::stopwords("en") & nchar(lemma) > 2)
    #
  } else if (modeltype == "NPN") {
    # set up noun,  proper noun
    model_biterms <- biterms[, udpipe::cooccurrence(lemma, relevant = (upos %in% c("NOUN","PROPN") & nchar(lemma) > 2 & !lemma %in% stopwords::stopwords("en")), skipgram = 3), by = list(doc_id)]
    #
    model_traindata <- subset(anno, upos %in% c("NOUN","PROPN") & !lemma %in% stopwords::stopwords("en") & nchar(lemma) > 2)
  } else {
    # adjective (wiffels says he'll use it for survey responses)
    model_biterms <- biterms[, udpipe::cooccurrence(lemma, relevant = (upos %in% c("ADJ") & nchar(lemma) > 2 & !lemma %in% stopwords::stopwords("en")), skipgram = 3), by = list(doc_id)]
    #
    model_traindata <- subset(anno, upos %in% c("ADJ") & !lemma %in% stopwords::stopwords("en") & nchar(lemma) > 2)
  }

  model_traindata <- model_traindata[, c("doc_id", "lemma")]

  out <- list(dat = dataset, anno = anno, model_biterms = model_biterms,
              model_traindata = model_traindata)
  return(out)
}

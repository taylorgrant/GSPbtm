#' Get PCA Coordinates
#'
#' @description Using some code from the LDAvis package to find the x,y coordinates for visualization
#'
#' @param model Topic models
#' @param data Annotated data
#'
#' @return list with x,y coordinates and term data for each topic
#' @export
#'
#' @examples
#' \dontrun{
#' pca_coords <- purrr::map(testing_models, btm_pca_coords, data = NAV_reviews)
#' }
btm_pca_coords <- function(model, data) {
  # calculated and extract parameters
  phi <- t(model$phi)[2:model$K,]
  docsize <- table(data$model_traindata$doc_id)
  scores  <- stats::predict(model, data$model_traindata)
  scores  <- scores[names(docsize), ]
  theta <- scores[,2:model$K]
  doc.length <- as.integer(docsize)
  # calclulate topic proportion
  topic.frequency <- colSums(theta * doc.length)
  vocab <- model$vocabulary$token
  topic.proportion <- topic.frequency/sum(topic.frequency)
  # get the topic coordinates using
  x_phi <- LDAvis::jsPCA(phi)
  x_phi <- x_phi |> dplyr::mutate(id = dplyr::row_number())
  # add topic proportion for sizing of each
  x_phi <- x_phi |>
    dplyr::mutate(frac = topic.proportion,
                  Topic = paste0("Topic ", id))
  #
  # gathering top terms
  # token counts for each term-topic combination (widths of red bars)
  term.topic.frequency <- phi * topic.frequency
  term.frequency <- colSums(term.topic.frequency)
  # stopifnot(all(term.frequency > 0))
  # marginal distribution over terms (width of blue bars)
  term.proportion <- term.frequency/sum(term.frequency)
  phi <- t(phi)
  topic.given.term <- phi/rowSums(phi)  # (W x K)
  kernel <- topic.given.term * log(sweep(topic.given.term, MARGIN=2,
                                         topic.proportion, `/`))
  distinctiveness <- rowSums(kernel)
  saliency <- term.proportion * distinctiveness
  # Order the terms for the "default" view by decreasing saliency:
  R <- 12 # number of terms to show in graph/table
  K <- model$K-1
  Rs <- rev(seq_len(R))
  topic_seq <- rep(seq_len(K), each = R)
  category <- paste0("Topic ", topic_seq)
  lift <- phi/term.proportion
  find_relevance <- function(i) {
    relevance <- i*log(phi) + (1 - i)*log(lift)
    idx <- apply(relevance, 2,
                 function(x) order(x, decreasing = TRUE)[seq_len(R)])
    # for matrices, we pick out elements by their row/column index
    indices <- cbind(c(idx), topic_seq)
    data.frame(Term = vocab[idx], Topic = category,
               logprob = round(log(phi[indices]), 4),
               loglift = round(log(lift[indices]), 4),
               stringsAsFactors = FALSE)
  }
  lambda.step <- .1
  lambda.seq <- seq(0, 1, by=lambda.step)
  # tinfo <- lapply(as.list(lambda.seq), find_relevance) # use this if stepping it out
  tinfo <- lapply(0.6, find_relevance) # note that we're hard coding this
  tinfo <- unique(do.call("rbind", tinfo))
  tinfo$Total <- term.frequency[match(tinfo$Term, vocab)]
  rownames(term.topic.frequency) <- paste0("Topic ", seq_len(K))
  colnames(term.topic.frequency) <- vocab
  tinfo$Freq <- term.topic.frequency[as.matrix(tinfo[c("Topic", "Term")])]
  out <- return(list(x_phi = x_phi, tinfo = tinfo))

}

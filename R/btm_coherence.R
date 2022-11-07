#' Calculate coherence scores for each topic
#'
#' @description From https://github.com/bnosac/BTM/issues/3#issuecomment-742515859; one of the issues of "coherence scores" is that coherence is dominant
#' in models with smaller number of topics that capture frequent usage of common language, hence Roberts et al in building the STM package propose using a mixture of exclusivity and coherence.
#' With the BTM option of `background = TRUE` which captures the most frequently used common terms, this is potentially less of an issue.
#' Thus far, the modelsappear to be functioning well with fairly good discrimination across topics.
#'
#' @param model topic model of choice
#' @param DTM DTM object
#' @param N Number of terms
#'
#' @return Average coherence of each topic
#' @export
#'
#' @examples
#' \dontrun{
#' coherence <- purrr::map_dfr(testing_models, btm_coherence, DTM = dtm) |>
#'     tidyr::gather(key = "Topic", value = "Value")
#' }
btm_coherence <- function(model, DTM, N = 10) {

  # Ensure matrix or Matrix-format (convert if slam)
  if (slam::is.simple_triplet_matrix(DTM)) {
    DTM <- Matrix::sparseMatrix(i=DTM$i, j=DTM$j, x=DTM$v, dims=c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
  }

  # note, dropping Topic 1 which is our common language usage topic
  K <- model$K-1

  DTMBIN <- DTM > 0

  documentFrequency <- Matrix::colSums(DTMBIN)
  names(documentFrequency) <- colnames(DTMBIN)

  # not pulling out terms from Topic 1 (common usage topic)
  topNtermsPerTopic <- stats::terms(model, top_n = N)[2:model$K]

  termcollect <- list()
  for (i in 1:K){
    termcollect[[i]] <- topNtermsPerTopic[[i]][,1]
  }

  allTopicModelTerms <- unique(as.vector(unlist(termcollect)))

  DTMBIN <- DTMBIN[, allTopicModelTerms]
  DTMBINCooc <- Matrix::t(DTMBIN) %*% DTMBIN
  DTMBINCooc <- Matrix::t((DTMBINCooc + 1) / Matrix::colSums(DTMBIN))
  DTMBINCooc <- log(DTMBINCooc)
  DTMBINCooc <- as.matrix(DTMBINCooc)

  coherence <- rep(0, K)
  for (topicIdx in 1:K) {
    topWordsOfTopic <- topNtermsPerTopic[[topicIdx]][,1]

    coherence[topicIdx] <- 0
    for (m in 2:length(topWordsOfTopic)) {
      for (l in 1:(m-1)) {
        mTerm <- as.character(topWordsOfTopic[m])
        lTerm <- as.character(topWordsOfTopic[l])
        coherence[topicIdx] <- coherence[topicIdx] + DTMBINCooc[mTerm, lTerm]
      }
    }
  }

  mean_coherence <- mean(coherence)
  return(mean_coherence)
}

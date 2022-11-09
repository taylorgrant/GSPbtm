#' Complete BTM (topic modeling)
#'
#' @param data Original data.frame/tibble with one column named "text"
#' @param min_topics Minimum number of topics to model
#' @param max_topics Maximum number of topics to model
#'
#' @return list of two data frames - original dataset with topic numbers appended; data of top n terms with topic numbers and PCA coordinates
#' @export
#'
#' @examples
#' \dontrun{
#' complete_btm(data, min_topics, max_topics)
#' }
complete_btm <- function(data, min_topics, max_topics, modeltype) {

  # 1. POS tagging by model type
  cat("Annotating the texts provided...\n")
  Reviews <- btm_annotate(data, modeltype)
  # 2. model across n topics
  cat("Estimating topic models...\n")
  n_topics <- as.integer(min_topics):as.integer(max_topics)
  testing_models <- n_topics |>
    purrr::map(btm_topicmodel, data = Reviews) |>
    purrr::set_names(nm = paste0("topicsize_", n_topics))
  # 3a. convert data to DTM for coherence scoring
  dtm <- btm_buildDTM(Reviews)
  # 3b. coherence scoring
  cat("Determining coherence scores...\n")
  coherence <- purrr::map_dfr(testing_models, btm_coherence, DTM = dtm) |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "Topic", values_to = "Value")
  # 4. pca coordinates and top terms
  cat("Extracting topic coordinates and top terms per topic...\n")
  pca_coords <- purrr::map(testing_models, btm_pca_coords, data = Reviews)
  # 5. extract the topic with the best coherence
  best_topic <- btm_best_topic(pca_coords, coherence)
  # 6. Probabilistic mapping of topics back to reviews
  modeled_topic <- btm_mapping(testing_models, Reviews, coherence, best_topic)

  cat("Putting everything together...\n")
  topic_names <- modeled_topic$topic_names |>
    dplyr::group_by(model_topic) |>
    dplyr::summarise(topic_nouns = paste(lemma, collapse = ", "))

  df <- NAV_reviews$dat %>%
    dplyr::left_join(modeled_topic$modeled_topic) %>%
    dplyr::left_join(topic_names)

  # fulldata is already reordered properly
  fulldata <- best_topic$fulldata %>%
    dplyr::left_join(topic_names, by = c("newid" = "model_topic"))

  out <- list(df = df, fulldata = fulldata)
}


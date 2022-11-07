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
complete_btm <- function(data, min_topics, max_topics) {

  # 1. POS tagging by model type
  NAV_reviews <- btm_annotate(data, "NAV")
  # 2. model across n topics
  n_topics <- as.integer(min_topics):as.integer(max_topics)
  testing_models <- n_topics |>
    purrr::map(btm_topicmodel, data = NAV_reviews) |>
    purrr::set_names(nm = paste0("topicsize_", n_topics))
  # 3a. convert data to DTM for coherence scoring
  dtm <- btm_buildDTM(NAV_reviews)
  # 3b. coherence scoring
  coherence <- purrr::map_dfr(testing_models, btm_coherence, DTM = dtm) |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "Topic", values_to = "Value")
  # 4. pca coordinates and top terms
  pca_coords <- purrr::map(testing_models, btm_pca_coords, data = NAV_reviews)
  # 5. extract the topic with the best coherence
  best_topic <- btm_best_topic(pca_coords, coherence)
  # 6. Probabilistic mapping of topics back to reviews
  modeled_topic <- btm_mapping(testing_models, NAV_reviews, coherence, best_topic)

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


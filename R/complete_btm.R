#' Complete BTM (topic modeling)
#'
#' @param data Original data.frame/tibble with one column named "text"
#' @param min_topics Minimum number of topics to model
#' @param max_topics Maximum number of topics to model
#' @param model NAV (noun, adjective, verb); NPN (noun, proper noun); ADJ (adjective)
#'
#' @return list - $df original dataframe with topic numbers joined per row; $fulldata - data of top n terms with topic numbers and PCA coordinates; $range - the min and max for axes when plotting
#' @export
#'
#' @examples
#' \dontrun{
#' complete_btm(data, min_topics, max_topics, "NAV")
#' }
complete_btm <- function(data, min_topics, max_topics, model) {

  # 1. POS tagging by model type
  cat("Annotating the texts provided...\n")
  anno_data <- btm_annotate(data, model)
  # 2. model across n topics
  cat("Estimating topic models...\n")
  n_topics <- as.integer(min_topics):as.integer(max_topics)
  testing_models <- n_topics |>
    purrr::map(btm_topicmodel, data = anno_data) |>
    purrr::set_names(nm = paste0("topicsize_", n_topics))
  # 3a. convert data to DTM for coherence scoring
  dtm <- btm_buildDTM(anno_data)
  # 3b. coherence scoring
  coherence <- purrr::map_dfr(testing_models, btm_coherence, DTM = dtm) |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "Topic", values_to = "Value")
  # 4. pca coordinates and top terms
  pca_coords <- purrr::map(testing_models, btm_pca_coords, data = anno_data)
  # 5. extract the topic with the best coherence
  best_topic <- btm_best_topic(pca_coords, coherence)
  cat(sprintf('\nBased on coherence scoring the best model contains %s topics...\n\n', nrow(best_topic$fulldata)))
  # 6. Probabilistic mapping of topics back to anno_data
  modeled_topic <- btm_mapping(testing_models, anno_data, coherence, best_topic)

  cat("Putting everything together...\n")
  topic_names <- modeled_topic$topic_names |>
    dplyr::group_by(model_topic) |>
    dplyr::summarise(topic_nouns = paste(lemma, collapse = ", "))

  df <- anno_data$dat %>%
    dplyr::left_join(modeled_topic$modeled_topic, by = c("doc_id" = "doc_id")) %>%
    dplyr::left_join(topic_names, by = c("model_topic" = "model_topic"))

  # fulldata is already reordered properly
  fulldata <- best_topic$fulldata %>%
    dplyr::left_join(topic_names, by = c("newid" = "model_topic"))

  out <- list(df = df, fulldata = fulldata,
              range = c(best_topic$min_range, best_topic$max_range))
}


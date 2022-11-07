#' Topic Mapping
#'
#' @description Identify the topic for each piece of text in the original dataframe
#'
#' @param models List of all potential models run
#' @param data The original dataframe, annotated via udpipe
#' @param coherence_df Coherence scores for each model
#' @param best_topic The selected best topic based upon coherence scores
#'
#' @return
#' @export
#'
#' @examples
btm_mapping <- function(models, data, coherence_df, best_topic) {
  # we have to access the model by list location, not name for predictions
  best_model <- which(names(models) == as.character(coherence_df[which.max(coherence_df$Value),1]))
  # predict
  model_scores <- stats::predict(models[[best_model]], newdata = data.frame(data$anno))
  # function to find the most likely (not using 1st topic which is catch all)
  colMax <- function(tbl) apply(tbl[2:models[[best_model]]$K], 1, which.max)

  # calculate
  modeled_topic <- model_scores |>
    data.frame() %>%
    dplyr::mutate(model_topic = colMax(.)) |>
    tibble::rownames_to_column(var = "doc_id") |>
    dplyr::select(doc_id, model_topic) |>
    dplyr::left_join(dplyr::select(best_topic$fulldata, c(id, newid)),
                     by = c("model_topic" = "id")) %>%
    dplyr::select(-model_topic) %>%
    dplyr::rename(model_topic = newid)

  topic_names <- data$anno |>
    dplyr::left_join(modeled_topic) |>
    dplyr::group_by(model_topic, upos) |>
    dplyr::count(lemma) |>
    dplyr::filter(upos %in% c("NOUN")) |>
    dplyr::group_by(model_topic, upos) |>
    dplyr::slice_max(n, n = 5, with_ties = FALSE) |>
    dplyr::filter(!is.na(model_topic))
  modeled_topic <- list(modeled_topic = modeled_topic, topic_names = topic_names)
  return(modeled_topic)
}

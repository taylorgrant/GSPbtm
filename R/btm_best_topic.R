btm_best_topic <- function(pca, coherence_df){

  # extract the xphi and term info from the topic with the best coherence score
  best_topic <- pca[names(pca) %in% as.character(coherence_df[which.max(coherence_df$Value),1])] |>
    purrr::flatten()

  # wide table format for DT::datatable
  topic_terms <- best_topic$tinfo |>
    dplyr::mutate(Score = paste0("(", round(Total, 2),")")) |>
    tidyr::unite(term_score, c(Term, Score), sep = " ") |>
    dplyr::group_by(Topic) |>
    dplyr::arrange(dplyr::desc(Total), .by_group = TRUE) |>
    dplyr::select(Topic, term_score) |>
    dplyr::group_by(Topic) |>
    dplyr::mutate(termid = paste0("t_", dplyr::row_number())) |>
    tidyr::pivot_wider(names_from = termid,
                values_from = c(term_score))

  # data for tooltip and highcharter::hc plotting
  fulldata <- best_topic$x_phi |>
    dplyr::left_join(topic_terms) |>
    dplyr::arrange(dplyr::desc(frac)) |>
    dplyr::mutate(newid = dplyr::row_number(),
           Topic = paste('Topic', newid),
           pct = scales::percent(frac, accuracy = .1))

  # set axis ranges so have square plot
  pca_range <- c(range(fulldata$x), range(fulldata$y))
  max_range <- max(abs(pca_range))*1.1
  min_range <- max_range*-1
  out <- list(fulldata = fulldata, max_range = max_range, min_range = min_range)
}

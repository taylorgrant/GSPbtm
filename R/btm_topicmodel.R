#' Topic Model with BTM
#'
#' @description Run the BTM topic model function over the data and biterms
#'
#' @param n_topics Number of topics to run
#' @param data List output from `btm_annotate()` function
#'
#' @return BTM object
#' @export
#'
#' @examples
#' \dontrun{
#' n_topics <- 5:15
#' testing_models <- n_topics %>%
#'   map(btm_topicmodel, data = NAV_reviews) %>%
#'   set_names(nm = paste0("topicsize_", n_topics))
#' }
btm_topicmodel <- function(n_topics, data) {
  # seed for replicability
  set.seed(2367)
  cat("Estimating topic ", n_topics, "...\n")
  # note the 1st topic is common terms (background = TRUE), which we will not plot/use
  nav_model <- BTM::BTM(data$model_traindata, biterms = data$model_biterms,
                   k = n_topics,
                   iter = 1000,
                   background = TRUE,
                   trace = FALSE,
                   detailed = TRUE)
}

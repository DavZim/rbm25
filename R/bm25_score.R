#' Score a text corpus based on the Okapi BM25 algorithm
#'
#' A simple wrapper around the [BM25] class.
#'
#' @param data text data, a vector of strings. Note any preprocessing steps
#' (tolower, removing stopwords etc) need to have taken place before this!
#' @param query the term to search for, note all preprocessing that was
#' applied to the text corpus initially needs to be already performed on
#' the term, e.g., tolower, removing stopwords etc
#' @param lang language of the data, see self$available_languages(),
#' can also be "detect" to automatically detect the language,  default is "detect"
#' @param k1 k1 parameter of BM25, default is 1.2
#' @param b b parameter of BM25, default is 0.75
#'
#' @seealso [BM25]
#' @return a numeric vector of the BM25 scores, note higher values are showing
#' a higher relevance of the text to the query
#' @export
#'
#' @examples
#' corpus <- c(
#'  "The rabbit munched the orange carrot.",
#'  "The snake hugged the green lizard.",
#'  "The hedgehog impaled the orange orange.",
#'  "The squirrel buried the brown nut."
#' )
#' scores <- bm25_score(data = corpus, query = "orange")
#' data.frame(text = corpus, scores_orange = scores)
bm25_score <- function(data, query, lang = NULL, k1 = 1.2, b = 0.75) {
  bm <- BM25$new(data = data, k1 = k1, b = b, lang = lang)
  res <- bm$query(query = query, max_n = length(data))

  res <- res[order(res$id), ]
  res$score
}

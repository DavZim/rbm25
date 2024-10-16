#' @title BM25 Object
#'
#' @description
#' Class to construct the BM25 search object
#' @export
#' @importFrom R6 R6Class
#' @examples
#' corpus <- c(
#'   "The rabbit munched the orange carrot.",
#'   "The snake hugged the green lizard.",
#'   "The hedgehog impaled the orange orange.",
#'   "The squirrel buried the brown nut."
#' )
#' bm25 <- BM25$new(data = corpus, lang = "en",
#'                  metadata = data.frame(src = paste("file", 1:4)))
#' bm25$query("orange", max_n = 2)
#' bm25$query("orange")
BM25 <- R6::R6Class(
  "BM25",
  public = list(
    #' @description Creates a new instance of a BM25 class
    #'
    #' @param data text data, a vector of strings. Note any preprocessing steps
    #' (tolower, removing stopwords etc) need to have taken place before this!
    #' @param lang language of the data, see self$available_languages(),
    #' can also be "detect" to automatically detect the language,  default is "detect"
    #' @param k1 k1 parameter of BM25, default is 1.2
    #' @param b b parameter of BM25, default is 0.75
    #' @param metadata a data.frame with metadata for each document, default is NULL
    #' must be a data.frame with the same number of rows containing arbitrary
    #' metadata for each document, e.g. a file path or a URL
    #'
    #' @return BM25 object
    #' @export
    #'
    #' @examples
    #' corpus <- c(
    #'  "The rabbit munched the orange carrot.",
    #'  "The snake hugged the green lizard.",
    #'  "The hedgehog impaled the orange orange.",
    #'  "The squirrel buried the brown nut."
    #' )
    #' bm25 <- BM25$new(data = corpus, lang = "en",
    #'                  metadata = data.frame(src = paste("file", 1:4)))
    #' bm25
    #' bm25$get_data()
    #'
    #' bm25$query("orange", max_n = 2)
    #' bm25$query("orange", max_n = 3)
    #' bm25$query("orange") # return all, same as max_n = Inf or NULL
    initialize = function(data = NULL, lang = "detect", k1 = 1.2, b = 0.75,
                          metadata = NULL) {
      private$k1 <- k1
      private$b <- b

      if (!is.null(lang)) private$set_lang(lang)
      if (is.null(lang) && is.null(private$lang))
        private$set_lang("detect")

      if (!is.null(data)) self$add_data(data, metadata)
    },
    #' @description Returns the available languages
    #'
    #' @return a named character vector with language codes and their full names
    #' @export
    #'
    #' @examples
    #' BM25$new()$available_languages()
    available_languages = function() {
      c(
        ar = "arabic", da = "danish", nl = "dutch", en = "english",
        fr = "french", de = "german", el = "greek", hu = "hungarian",
        it = "italian", no = "norwegian", pt = "portuguese", ro = "romanian",
        ru = "russian", es = "spanish", sv = "swedish", ta = "tamil",
        tr = "turkish", auto = "detect"
      )
    },
    #' @description Returns the data
    #'
    #' @param add_metadata whether to add metadata to the data, default is TRUE
    #'
    #' @return a data.frame with the data and metadata if available and selected
    #' @export
    #'
    #' @examples
    #' BM25$new(data = letters, metadata = LETTERS)$get_data()
    get_data = function(add_metadata = TRUE) {
      r <- data.frame(text = private$data)
      if (add_metadata && !is.null(private$metadata))
        r <- cbind(r, private$metadata)
      r
    },
    #' @description Returns the language used
    #'
    #' @return a character string with the language code
    #' @export
    #'
    #' @examples
    #' BM25$new()$get_lang()
    #' BM25$new(lang = "en")$get_lang()
    #' BM25$new(lang = "detect")$get_lang()
    get_lang = function() {
      private$lang
    },
    #' @description Prints a BM25 object
    #'
    #' @param n number of data to print, default is 5
    #' @param nchar number of characters to print for each text, default is 20
    #'
    #' @return the object invisible
    #' @export
    #'
    #' @examples
    #' BM25$new(data = letters, metadata = LETTERS)
    print = function(n = 5, nchar = 20) {
      cat(sprintf("<BM25 (k1: %.2f, b: %.2f)> with %s documents (language: '%s')\n",
                  private$k1, private$b, length(private$data), private$lang))

      if (!is.null(private$data)) {
        d <- data.frame(text = head(private$data, n = n))
        title <- "  - Data"
        if (!is.null(private$metadata)) {
          m <- head(private$metadata, n = n)
          names(m) <- paste0("metadata.", names(m))
          d <- cbind(d, m)
          title <- paste(title, "& Metadata")
        }

        cat(title, "\n")
        print(d)
        if (length(private$data) > nchar)
          cat(sprintf("... ommited %s entries (total %s)\n",
                      length(private$data) - nchar, length(private$data)))
      }
      return(invisible(self))
    },
    #' @description Adds data to the BM25 object
    #'
    #' This can be useful to add more data later on, note this will rebuild the engine.
    #'
    #' @param data a vector of strings
    #' @param metadata a data.frame with metadata for each document, default is NULL
    #'
    #' @return NULL
    #' @export
    #'
    #' @examples
    #' bm25 <- BM25$new()
    #' bm25$add_data(letters, metadata = LETTERS)
    #' bm25
    add_data = function(data, metadata = NULL) {
      if (!is.vector(data) || !is.character(data))
        stop("Data must be a vector of strings")

      if (!is.null(metadata)) private$add_metadata(data, metadata)
      private$data <- c(private$data, data)
      private$engine <- build_engine(corpus = private$data,
                                     language = private$lang,
                                     k1 = private$k1, b = private$b)
    },
    #' @description Query the BM25 object for the N best matches
    #'
    #' @param query the term to search for, note all preprocessing that was
    #' applied to the text corpus initially needs to be already performed on
    #' the term, e.g., tolower, removing stopwords etc
    #' @param max_n the maximum number of results to return, default is all
    #' @param return_text whether to return the text, default is TRUE
    #' @param return_metadata whether to return metadata, default is TRUE
    #'
    #' @return a data.frame with the results
    #' @export
    #'
    #' @examples
    #' corpus <- c(
    #'  "The rabbit munched the orange carrot.",
    #'  "The snake hugged the green lizard.",
    #'  "The hedgehog impaled the orange orange.",
    #'  "The squirrel buried the brown nut."
    #' )
    #' bm25 <- BM25$new(data = corpus, lang = "en",
    #'                  metadata = data.frame(src = paste("file", 1:4)))
    #'
    #' bm25$query("orange", max_n = 2)
    #' bm25$query("orange", max_n = 3)
    #' bm25$query("orange", return_text = FALSE, return_metadata = FALSE)
    #' bm25$query("orange", max_n = 3)
    query = function(query, max_n = NULL, return_text = TRUE, return_metadata = TRUE) {
      if (is.null(private$engine))
        stop("No data available, use self$add_data() to add data or initialize with data")
      if (is.null(max_n) || is.infinite(max_n)) max_n <- length(private$data)

      max_n <- min(max_n, length(private$data))
      res <- search(private$engine, query, max_n)

      res <- as.data.frame(res)

      if (nrow(res) < max_n)
        res <- rbind(res, data.frame(id = setdiff(seq(max_n), res$id), score = 0))

      res$rank <- rank(-res$score, ties.method = "min")
      res <- res[order(res$id), ]

      if (return_text) res$text <- private$data[res$id]

      if (return_metadata && !is.null(private$metadata))
        res <- cbind(res, private$metadata[res$id, , drop = FALSE])

      res <- res[seq(max_n), ]
      res[order(res$rank), ]
    }
  ),
  private = list(
    k1 = 1.2, # BM25 parameter
    b = 0.75, # BM25 parameter
    lang = NULL, # language for the BM25 engine used
    data = NULL, # a vector of the text corpus
    metadata = NULL, # a data.frame (same length as data) or NULL
    engine = NULL, # the rust reference pointer to the BM25 engine

    # sets a language
    set_lang = function(lang) {
      lang <- tolower(lang)
      lmap <- self$available_languages()
      if (lang %in% names(lmap)) lang <- lmap[[lang]]
      if (!(lang %in% lmap))
        stop(sprintf("Language '%s' not supported, see self$available_languages()", lang))
      private$lang <- fupper(lang)
    },
    # adds metadata to the object
    add_metadata = function(data, metadata) {
      if (!is.data.frame(metadata) && is.vector(metadata))
        metadata <- data.frame(metadata = metadata)
      if (!is.data.frame(metadata)) stop("Metadata must be a data.frame")
      if (length(data) != nrow(metadata))
        stop("Length of metadata must be the same as the length of the data")

      forbidden_vars <- "text"
      if (any(forbidden_vars %in% names(metadata)))
        stop(sprintf("Metadata cannot contain the following variables: '%s'",
                     paste(forbidden_vars, collapse = "', '")))

      if (is.null(private$metadata)) {
        private$metadata <- metadata
      } else {
        private$metadata <- cbind(private$metadata, metadata)
      }
    }
  )
)

# small helper function to capitalize the first letter of a string
fupper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))

#' Create a BM25 Search Engine
#'
#' @description
#' Class to construct a BM25 search engine with support for document IDs.
#'
#' @details
#'
#' By default, IDs are assigned using an auto-incrementing counter.
#' Alternatively, a vector of IDs can be provided as a character vector—
#' for example a vector of GUIDs or ULIDs.
#'
#' @export
#' @importFrom R6 R6Class
#' @examples
#' corpus <- c(
#'   "The rabbit munched the orange carrot.",
#'   "The snake hugged the green lizard.",
#'   "The hedgehog impaled the orange orange.",
#'   "The squirrel buried the brown nut."
#' )
#'
#' engine <- SearchEngine$new(data = corpus, lang = "en")
#' engine$search("orange", max_n = 2)
#'
#' ids <- c("doc1", "doc2", "doc3", "doc4")
#' engine <- SearchEngine$new(data = corpus, lang = "en", ids = ids)
#' engine$search("orange", max_n = 2)
SearchEngine <- R6::R6Class(
  "SearchEngine",
  public = list(
    #' @description Creates a new instance of a SearchEngine class
    #'
    #' @param data a character vector
    #' @param lang language of the data, see self$available_languages()
    #' @param k1 k1 parameter of BM25, default is 1.2
    #' @param b b parameter of BM25, default is 0.75
    #' @param ids default NULL. Optional character vector of IDs.
    #' @param metadata a data.frame with metadata for each document, default is NULL
    #'
    #' @return SearchEngine object
    #' @export
    initialize = function(
      data = NULL,
      lang = "detect",
      k1 = 1.2,
      b = 0.75,
      ids = NULL,
      metadata = NULL
    ) {
      private$k1 <- k1
      private$b <- b

      if (!is.null(lang)) {
        private$set_lang(lang)
      }
      if (is.null(lang) && is.null(private$lang)) {
        private$set_lang("detect")
      }

      if (!is.null(data)) {
        if (!is.character(data)) {
          stop("Data must be a character vector")
        }

        if (!is.null(ids)) {
          if (length(ids) != length(data)) {
            stop("Length of ids must match length of data")
          }
          ids <- as.character(ids)
          private$uses_ids <- TRUE
        } else {
          private$uses_ids <- FALSE
        }

        if (!is.null(metadata)) {
          if (!inherits(metadata, "data.frame")) {
            stop("Metadata must be a data.frame")
          }
          if (nrow(metadata) != length(data)) {
            stop("Number of rows in metadata must match length of data")
          }
          private$metadata <- metadata
        }

        private$n_docs <- length(data)
        private$engine <- Engine$new(
          corpus = data,
          language = private$lang,
          k1 = k1,
          b = b,
          ids = ids
        )
      }
    },

    #' @description Returns the available languages
    #'
    #' @return a named character vector with language codes and their full names
    #' @export
    available_languages = function() {
      c(
        ar = "arabic",
        da = "danish",
        nl = "dutch",
        en = "english",
        fr = "french",
        de = "german",
        el = "greek",
        hu = "hungarian",
        it = "italian",
        no = "norwegian",
        pt = "portuguese",
        ro = "romanian",
        ru = "russian",
        es = "spanish",
        sv = "swedish",
        ta = "tamil",
        tr = "turkish",
        auto = "detect"
      )
    },

    #' @description Returns the language used
    #'
    #' @return a character string with the language code
    #' @export
    get_lang = function() {
      private$lang
    },

    #' @description Prints a SearchEngine object
    #'
    #' @return the object invisible
    #' @export
    print = function() {
      id_type <- if (private$uses_ids) "custom IDs" else "auto-increment"
      cat(sprintf(
        "<SearchEngine (k1: %.2f, b: %.2f)> with %s documents (%s, language: '%s')\n",
        private$k1,
        private$b,
        private$n_docs,
        id_type,
        private$lang
      ))
      return(invisible(self))
    },

    #' @description Search for documents matching a query
    #'
    #' @param query the search query
    #' @param max_n maximum number of results to return
    #' @param return_metadata whether to return metadata, default is TRUE
    #'
    #' @return a data.frame with search results
    #' @export
    search = function(query, max_n = private$n_docs, return_metadata = TRUE) {
      private$check_engine()
      if (is.null(max_n) || is.infinite(max_n)) {
        max_n <- private$n_docs
      }
      max_n <- min(max_n, private$n_docs)

      res <- private$engine$search(query, as.integer(max_n))

      res$rank <- rank(-res$score, ties.method = "min")

      res <- res[, c("id", "score", "rank", "text")]

      if (return_metadata && !is.null(private$metadata)) {
        if (private$uses_ids) {
          res <- cbind(
            res,
            private$metadata[
              match(res$id, rownames(private$metadata)),
              ,
              drop = FALSE
            ]
          )
        } else {
          res <- cbind(res, private$metadata[res$id, , drop = FALSE])
        }
      }

      res
    },

    #' @description Search for documents matching a query with control over returned columns
    #'
    #' @inheritParams search
    #' @param return_text whether to return the text column, default is TRUE
    #'
    #' @return a data.frame with search results
    #' @export
    query = function(
      query,
      max_n = private$n_docs,
      return_text = TRUE,
      return_metadata = TRUE
    ) {
      res <- self$search(
        query,
        max_n = max_n,
        return_metadata = return_metadata
      )

      # if return_text = FALSE we drop the text column which
      # is always returns by the `SearchEngine<T>::search()` method
      if (!return_text) {
        res$text <- NULL
      }
    },

    #' @description Upsert a document
    #'
    #' @param id document ID (integer for auto-increment, character for custom IDs)
    #' @param text document text
    #'
    #' @return invisible self
    #' @export
    upsert = function(text, id = NULL) {
      private$check_engine()
      id <- private$validate_id(id)
      if (private$uses_ids) {
        private$engine$upsert(as.character(id), text)
      } else {
        private$engine$upsert(as.integer(id), text)
      }

      invisible(self)
    },

    #' @description Remove a document
    #'
    #' @param id document ID to remove
    #'
    #' @return invisible self
    #' @export
    remove = function(id = NULL) {
      private$check_engine()
      id <- private$validate_id(id)

      if (private$uses_ids) {
        private$engine$remove(as.character(id))
      } else {
        private$engine$remove(as.integer(id))
      }

      invisible(self)
    },

    #' @description Get a document by ID
    #'
    #' @param id document ID
    #'
    #' @return document text or NULL if not found
    #' @export
    get = function(id = NULL) {
      private$check_engine()
      private$validate_id(id)

      if (private$uses_ids) {
        private$engine$get(as.character(id))
      } else {
        private$engine$get(as.integer(id))
      }
    }
  ),
  private = list(
    k1 = 1.2,
    b = 0.75,
    lang = NULL,
    n_docs = 0,
    uses_ids = FALSE,
    metadata = NULL,
    engine = NULL,

    set_lang = function(lang) {
      lang <- tolower(lang)
      lmap <- self$available_languages()
      if (lang %in% names(lmap)) {
        lang <- lmap[[lang]]
      }
      if (!(lang %in% lmap)) {
        stop(sprintf(
          "Language '%s' not supported, see self$available_languages()",
          lang
        ))
      }
      private$lang <- fupper(lang)
    },

    check_engine = function() {
      if (is.null(private$engine)) {
        stop(
          "No engine available, initialize with `SearchEngine$new()` first",
          call. = FALSE
        )
      }
    },

    validate_id = function(id = NULL) {
      # NULL with custom IDs requires an ID to be provided
      if (is.null(id) && private$uses_ids) {
        stop("`id` must be provided when using custom IDs", call. = FALSE)
      }

      # NULL with auto-increment generates next ID
      if (is.null(id) && !private$uses_ids) {
        return(private$engine$n_docs() + 1L)
      }

      # Non-NULL with custom IDs must be character
      if (!is.null(id) && private$uses_ids && !is.character(id)) {
        stop("`id` must be a character", call. = FALSE)
      }

      # Non-NULL with auto-increment must be numeric/integer
      if (!is.null(id) && !private$uses_ids && !is.numeric(id) && !is.integer(id)) {
        stop("`id` must be numeric or integer", call. = FALSE)
      }

      return(id)
    }
  )
)

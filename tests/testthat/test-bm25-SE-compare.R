corpus_original <- c(
  "The rabbit munched the orange carrot.",
  "The snake hugged the green lizard.",
  "The hedgehog impaled the orange orange.",
  "The squirrel buried the brown nut."
)

stopwords <- c("the", "a", "an", "and")
corpus <- corpus_original |>
  tolower() |>
  gsub(pattern = "[[:punct:]]", replacement = "") |>
  gsub(
    pattern = paste0("\\b(", paste(stopwords, collapse = "|"), ") *\\b"),
    replacement = ""
  ) |>
  trimws()

metadata <- data.frame(
  text_original = corpus_original,
  source = c("book1", "book2", "book3", "book4")
)

test_that("BM25 and SearchEngine produce identical search results", {
  bm25 <- BM25$new(data = corpus, lang = "detect", k1 = 1.2, b = 0.75)
  engine <- SearchEngine$new(data = corpus, lang = "detect", k1 = 1.2, b = 0.75)

  bm25_res <- bm25$query(
    "orange",
    max_n = 4,
    return_text = FALSE,
    return_metadata = FALSE
  )

  # we're filtering out padd results here
  bm25_res <- bm25_res[bm25_res$score > 0, ]

  engine_res <- engine$search("orange", max_n = 4, return_metadata = FALSE)

  expect_equal(bm25_res$id, engine_res$id)
  expect_equal(bm25_res$score, engine_res$score, tolerance = 1e-6)
})

test_that("BM25 and SearchEngine handle different queries identically", {
  bm25 <- BM25$new(data = corpus)
  engine <- SearchEngine$new(data = corpus)

  queries <- c("orange", "squirrel", "green", "brown nut")

  for (q in queries) {
    bm25_res <- bm25$query(
      q,
      max_n = 4,
      return_text = FALSE,
      return_metadata = FALSE
    )

    bm25_res <- bm25_res[bm25_res$score > 0, ]

    engine_res <- engine$search(q, max_n = 4, return_metadata = FALSE)

    expect_equal(
      bm25_res$id,
      engine_res$id,
      info = paste("Query:", q)
    )
    expect_equal(
      bm25_res$score,
      engine_res$score,
      tolerance = 1e-6,
      info = paste("Query:", q)
    )
  }
})

test_that("BM25 and SearchEngine handle metadata identically", {
  bm25 <- BM25$new(data = corpus, metadata = metadata)
  engine <- SearchEngine$new(data = corpus, metadata = metadata)

  bm25_res <- bm25$query(
    "orange",
    max_n = 2,
    return_text = FALSE,
    return_metadata = TRUE
  )
  engine_res <- engine$search("orange", max_n = 2, return_metadata = TRUE)

  expect_equal(bm25_res$text_original, engine_res$text_original)
  expect_equal(bm25_res$source, engine_res$source)
})

test_that("BM25 and SearchEngine respect language parameter identically", {
  bm25_en <- BM25$new(data = corpus, lang = "en")
  engine_en <- SearchEngine$new(data = corpus, lang = "en")

  expect_equal(bm25_en$get_lang(), engine_en$get_lang())

  bm25_res <- bm25_en$query(
    "orange",
    max_n = 4,
    return_text = FALSE,
    return_metadata = FALSE
  )

  bm25_res <- bm25_res[bm25_res$score > 0, ]

  engine_res <- engine_en$search("orange", max_n = 4, return_metadata = FALSE)

  expect_equal(bm25_res$score, engine_res$score, tolerance = 1e-6)
})

test_that("BM25 and SearchEngine respect k1 and b parameters identically", {
  k1_val <- 1.5
  b_val <- 0.8

  bm25 <- BM25$new(data = corpus, k1 = k1_val, b = b_val)
  engine <- SearchEngine$new(data = corpus, k1 = k1_val, b = b_val)

  bm25_res <- bm25$query(
    "orange",
    max_n = 4,
    return_text = FALSE,
    return_metadata = FALSE
  )

  bm25_res <- bm25_res[bm25_res$score > 0, ]

  engine_res <- engine$search("orange", max_n = 4, return_metadata = FALSE)

  expect_equal(bm25_res$score, engine_res$score, tolerance = 1e-6)
})

test_that("BM25 and SearchEngine handle max_n parameter identically", {
  bm25 <- BM25$new(data = corpus)
  engine <- SearchEngine$new(data = corpus)

  for (n in c(1, 2)) {
    bm25_res <- bm25$query(
      "orange",
      max_n = n,
      return_text = FALSE,
      return_metadata = FALSE
    )

    bm25_res <- bm25_res[bm25_res$score > 0, ]

    engine_res <- engine$search("orange", max_n = n, return_metadata = FALSE)

    expect_equal(nrow(bm25_res), nrow(engine_res), info = paste("max_n:", n))
    expect_equal(bm25_res$id, engine_res$id, info = paste("max_n:", n))
  }
})

test_that("BM25 and SearchEngine both return correct column structure", {
  bm25 <- BM25$new(data = corpus, metadata = metadata)
  engine <- SearchEngine$new(data = corpus, metadata = metadata)

  bm25_res <- bm25$query(
    "orange",
    max_n = 2,
    return_text = TRUE,
    return_metadata = TRUE
  )
  engine_res <- engine$search("orange", max_n = 2, return_metadata = TRUE)

  expect_equal(
    names(bm25_res),
    c("id", "score", "rank", "text", "text_original", "source")
  )

  expect_equal(
    names(engine_res),
    c("id", "score", "rank", "text", "text_original", "source")
  )

  expect_equal(
    names(bm25_res),
    names(engine_res)
  )
  expect_equal(bm25_res, engine_res, ignore_attr = TRUE)
})

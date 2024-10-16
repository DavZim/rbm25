corpus_original <- c(
  "The rabbit munched the orange carrot.",
  "The snake hugged the green lizard.",
  "The hedgehog impaled the orange orange.",
  "The squirrel buried the brown nut."
)

# text preprocessing: tolower, remove punctuation, remove stopwords
# note this is just an example and not the best way for larger amounts of text
stopwords <- c("the", "a", "an", "and")
corpus <- corpus_original |>
  tolower() |>
  gsub(pattern = "[[:punct:]]", replacement = "") |>
  gsub(pattern = paste0("\\b(", paste(stopwords, collapse = "|"), ") *\\b"),
       replacement = "") |>
  trimws()

# define some metadata for the text corpus, e.g., the original text and the source
metadata <- data.frame(
  text_original = corpus_original,
  source = c("book1", "book2", "book3", "book4")
)

test_that("BM25 works", {
  bm <- BM25$new(data = corpus, metadata = metadata)

  expect_equal(class(bm), c("BM25", "R6"))
  expect_equal(bm$get_lang(), "Detect")
  expected_data <- data.frame(
    text = corpus,
    text_original = corpus_original,
    source = c("book1", "book2", "book3", "book4")
  )
  expect_equal(bm$get_data(), expected_data)
  expected_languages <- c(
    ar = "arabic", da = "danish", nl = "dutch", en = "english",
    fr = "french", de = "german", el = "greek", hu = "hungarian",
    it = "italian", no = "norwegian", pt = "portuguese", ro = "romanian",
    ru = "russian", es = "spanish", sv = "swedish", ta = "tamil",
    tr = "turkish", auto = "detect"
    )
  expect_equal(bm$available_languages(), expected_languages)

  res <- bm$query(query = "orange", max_n = 2)

  expected <- data.frame(
    id = c(3, 1),
    score = c(0.49042809, 0.35667497),
    rank = c(1, 2),
    text = corpus[c(3, 1)],
    text_original = corpus_original[c(3, 1)],
    source = c("book3", "book1")
  )

  expect_equal(res, expected)
})

test_that("bm25_score works", {
  scores <- bm25_score(data = corpus, query = "orange")

  expected <- c(0.35667497, 0.0, 0.49042809, 0.0)
  expect_equal(scores, expected)
})

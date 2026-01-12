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

test_that("SearchEngine works with auto-increment IDs", {
  engine <- SearchEngine$new(data = corpus)

  expect_equal(class(engine), c("SearchEngine", "R6"))
  expect_equal(engine$get_lang(), "Detect")

  expected_languages <- c(
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
  expect_equal(engine$available_languages(), expected_languages)

  res <- engine$search(query = "orange", max_n = 2, return_metadata = FALSE)

  expected <- data.frame(
    id = c(3L, 1L),
    score = c(0.49042809, 0.35667497),
    rank = c(1, 2),
    text = corpus[c(3, 1)],
    row.names = c(1L, 2L)
  )

  expect_equal(res, expected, tolerance = 1e-6)
})

test_that("SearchEngine works with custom IDs", {
  ids <- c("doc1", "doc2", "doc3", "doc4")
  engine <- SearchEngine$new(data = corpus, ids = ids)

  expect_equal(class(engine), c("SearchEngine", "R6"))

  res <- engine$search(query = "orange", max_n = 2, return_metadata = FALSE)

  expected <- data.frame(
    id = c("doc3", "doc1"),
    score = c(0.49042809, 0.35667497),
    rank = c(1, 2),
    text = corpus[c(3, 1)],
    row.names = c(1L, 2L),
    stringsAsFactors = FALSE
  )

  expect_equal(res, expected, tolerance = 1e-6)
})

test_that("SearchEngine metadata works", {
  engine <- SearchEngine$new(data = corpus, metadata = metadata)

  res <- engine$search(
    query = "orange",
    max_n = 2,
    return_metadata = TRUE
  )

  expected <- data.frame(
    id = c(3L, 1L),
    score = c(0.49042809, 0.35667497),
    rank = c(1, 2),
    text = corpus[c(3, 1)],
    text_original = corpus_original[c(3, 1)],
    source = c("book3", "book1")
  )

  expect_equal(
    res,
    expected,
    tolerance = 1e-6,
    ignore_attr = TRUE
  )

  res_no_metadata <- engine$search(
    query = "orange",
    max_n = 2,
    return_metadata = FALSE
  )

  expect_equal(
    names(res_no_metadata),
    c("id", "score", "rank", "text")
  )
})

test_that("SearchEngine upsert works", {
  ids <- c("doc1", "doc2", "doc3", "doc4")
  engine <- SearchEngine$new(data = corpus, ids = ids)

  engine$upsert("doc1", "banana fruit yellow")

  doc <- engine$get("doc1")
  expect_equal(doc, "banana fruit yellow")

  res <- engine$search("squirrel")
  expect_equal(res$id, "doc4")
})

test_that("SearchEngine remove works", {
  ids <- c("doc1", "doc2", "doc3", "doc4")
  engine <- SearchEngine$new(data = corpus, ids = ids)

  engine$remove("doc1")

  doc <- engine$get("doc1")
  expect_null(doc)
})

test_that("SearchEngine get works", {
  ids <- c("doc1", "doc2", "doc3", "doc4")
  engine <- SearchEngine$new(data = corpus, ids = ids)

  doc <- engine$get("doc1")
  expect_equal(doc, corpus[1])

  doc <- engine$get("doc_nonexistent")
  expect_null(doc)
})

test_that("SearchEngine validates inputs", {
  expect_error(
    SearchEngine$new(data = 123),
    "Data must be a character vector"
  )

  expect_error(
    SearchEngine$new(data = corpus, ids = c("a", "b")),
    "Length of ids must match length of data"
  )

  expect_error(
    SearchEngine$new(data = corpus, metadata = "not a dataframe"),
    "Metadata must be a data.frame"
  )

  expect_error(
    SearchEngine$new(data = corpus, metadata = data.frame(x = 1:2)),
    "Number of rows in metadata must match length of data"
  )
})

test_that("SearchEngine auto-increment IDs work with upsert/remove/get", {
  engine <- SearchEngine$new(data = corpus)

  engine$upsert(1L, "new text for doc 1")
  doc <- engine$get(1L)
  expect_equal(doc, "new text for doc 1")

  engine$remove(2L)
  doc <- engine$get(2L)
  expect_null(doc)
})

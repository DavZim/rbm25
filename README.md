
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbm25

<!-- badges: start -->

[![R-CMD-check](https://github.com/DavZim/rbm25/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DavZim/rbm25/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/rbm25)](https://CRAN.R-project.org/package=rbm25)
<!-- badges: end -->

`{rbm25}` is a light wrapper around the rust
[`bm25`](https://crates.io/crates/bm25) crate. It provides a simple
interface to the [Okapi BM25
algorithm](https://en.wikipedia.org/wiki/Okapi_BM25) for text search.

Note the package does not provide any text preprocessing, this needs to
be done before using the package.

## Installation

You can install the development version of rbm25 like so:

``` r
# Development Version
# devtools::install_github("DavZim/rbm25")

# CRAN release
install.packages("rbm25")
```

## Example

The package exposes an R6 class `BM25` that can be used to query a text
corpus. For simplicity, there is also a `bm25_score()` function that
wraps the `BM25` class.

``` r
library(rbm25)
# create a text corpus, where we want to find the closest matches for a query
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
```

**Using the BM25 Class**

``` r
bm <- BM25$new(data = corpus, metadata = metadata)
bm
#> <BM25 (k1: 1.20, b: 0.75)> with 4 documents (language: 'Detect')
#>   - Data & Metadata 
#>                             text                  metadata.text_original
#> 1   rabbit munched orange carrot   The rabbit munched the orange carrot.
#> 2      snake hugged green lizard      The snake hugged the green lizard.
#> 3 hedgehog impaled orange orange The hedgehog impaled the orange orange.
#> 4      squirrel buried brown nut      The squirrel buried the brown nut.
#>   metadata.source
#> 1           book1
#> 2           book2
#> 3           book3
#> 4           book4

# note that query returns the values sorted by rank
bm$query(query = "orange", max_n = 2)
#>   id     score rank                           text
#> 1  3 0.4904281    1 hedgehog impaled orange orange
#> 2  1 0.3566750    2   rabbit munched orange carrot
#>                             text_original source
#> 1 The hedgehog impaled the orange orange.  book3
#> 2   The rabbit munched the orange carrot.  book1
```

**Using the `bm25_score()` function**

``` r
# note that bm25_score returns the score in the order of the input data
scores <- bm25_score(data = corpus, query = "orange")
data.frame(text = corpus, scores_orange = scores)
#>                             text scores_orange
#> 1   rabbit munched orange carrot     0.3566750
#> 2      snake hugged green lizard     0.0000000
#> 3 hedgehog impaled orange orange     0.4904281
#> 4      squirrel buried brown nut     0.0000000
```

# Score a text corpus based on the Okapi BM25 algorithm

A simple wrapper around the
[BM25](https://davzim.github.io/rbm25/reference/BM25.md) class.

## Usage

``` r
bm25_score(data, query, lang = NULL, k1 = 1.2, b = 0.75)
```

## Arguments

- data:

  text data, a vector of strings. Note any preprocessing steps (tolower,
  removing stopwords etc) need to have taken place before this!

- query:

  the term to search for, note all preprocessing that was applied to the
  text corpus initially needs to be already performed on the term, e.g.,
  tolower, removing stopwords etc

- lang:

  language of the data, see self\$available_languages(), can also be
  "detect" to automatically detect the language, default is "detect"

- k1:

  k1 parameter of BM25, default is 1.2

- b:

  b parameter of BM25, default is 0.75

## Value

a numeric vector of the BM25 scores, note higher values are showing a
higher relevance of the text to the query

## See also

[BM25](https://davzim.github.io/rbm25/reference/BM25.md)

## Examples

``` r
corpus <- c(
 "The rabbit munched the orange carrot.",
 "The snake hugged the green lizard.",
 "The hedgehog impaled the orange orange.",
 "The squirrel buried the brown nut."
)
scores <- bm25_score(data = corpus, query = "orange")
data.frame(text = corpus, scores_orange = scores)
#>                                      text scores_orange
#> 1   The rabbit munched the orange carrot.     0.8025914
#> 2      The snake hugged the green lizard.     0.0000000
#> 3 The hedgehog impaled the orange orange.     1.0516716
#> 4      The squirrel buried the brown nut.     0.0000000
```

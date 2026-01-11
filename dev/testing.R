devtools::load_all()

corpus_original <- c(
  "The rabbit munched the orange carrot.",
  "The snake hugged the green lizard.",
  "The hedgehog impaled the orange orange.",
  "The squirrel buried the brown nut."
)

engine <- Engine$new(
  corpus_original,
  "Detect",
  k1 = 1.2,
  b = 0.75,
  NULL
)

engine$get(1)
engine$upsert(1, "The rabbit munched the orange carrot.")
engine$upsert(1, "The rabbit munched the orange carrot!")
engine$get(1)

engine$search("carrot", 5)

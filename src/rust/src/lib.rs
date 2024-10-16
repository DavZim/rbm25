use extendr_api::prelude::*;
use bm25::{
  Language,
  LanguageMode,
  SearchEngineBuilder,
  SearchEngine
};

// Build a search engine from a corpus.
// default values: k1 = 1.2, b = 0.75
#[extendr]
fn build_engine(corpus: Vec<String>, language: &str, k1: f32, b: f32) -> ExternalPtr<SearchEngine<u32>> {
  let search_engine = if language == "Detect"{
    SearchEngineBuilder::<u32>::with_corpus(LanguageMode::Detect, corpus)
  } else {
    let lang = match language {
      "Arabic" => Language::Arabic,
      "Danish" => Language::Danish,
      "Dutch" => Language::Dutch,
      "English" => Language::English,
      "French" => Language::French,
      "German" => Language::German,
      "Greek" => Language::Greek,
      "Hungarian" => Language::Hungarian,
      "Italian" => Language::Italian,
      "Norwegian" => Language::Norwegian,
      "Portuguese" => Language::Portuguese,
      "Romanian" => Language::Romanian,
      "Russian" => Language::Russian,
      "Spanish" => Language::Spanish,
      "Swedish" => Language::Swedish,
      "Tamil" => Language::Tamil,
      "Turkish" => Language::Turkish,
      _ => panic!("Language '{}' not supported", language),
    };

  SearchEngineBuilder::<u32>::with_corpus(lang, corpus)
  };

  let res = search_engine.k1(k1).b(b).build();

  ExternalPtr::new(res)
}

// Return string `"Hello world!"` to R.
#[extendr]
fn search(engine: ExternalPtr<SearchEngine<u32>>, query: &str, max_n: usize) -> List{
  let engine: ExternalPtr<SearchEngine<u32>> = engine.try_into().unwrap();
  let res = (*engine).search(query, max_n);

  let mut ids = Vec::with_capacity(res.len());
  let mut scores = Vec::with_capacity(res.len());

  for r in res {
    ids.push(r.document.id + 1);
    scores.push(r.score);
  }

  let ll: List = list!(id = ids, score = scores);
  ll
}


// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod rbm25;
    fn build_engine;
    fn search;
}

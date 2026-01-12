use bm25::{Language, LanguageMode, SearchEngine, SearchEngineBuilder};
use extendr_api::prelude::*;

// Build a search engine from a corpus.
// default values: k1 = 1.2, b = 0.75
#[extendr]
fn build_engine(
    corpus: Vec<String>,
    language: &str,
    k1: f32,
    b: f32,
) -> ExternalPtr<SearchEngine<u32>> {
    let lang_mode = parse_language(language);

    let search_engine = SearchEngineBuilder::<u32>::with_corpus(lang_mode, corpus);

    let res = search_engine.k1(k1).b(b).build();

    ExternalPtr::new(res)
}

// Return string `"Hello world!"` to R.
#[extendr]
fn search(engine: ExternalPtr<SearchEngine<u32>>, query: &str, max_n: usize) -> List {
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

#[extendr]
pub enum Engine {
    AutoIncrement(SearchEngine<u32>),
    ProvidedIds(SearchEngine<String>),
}

#[extendr]
impl Engine {
    fn new(corpus: Strings, language: &str, k1: f32, b: f32, ids: Nullable<Strings>) -> Self {
        let lang_mode = parse_language(language);

        match ids {
            Nullable::NotNull(id_strings) => {
                if corpus.len() != id_strings.len() {
                    throw_r_error("Length of corpus and ids must match".to_string());
                }

                let documents = corpus
                    .iter()
                    .zip(id_strings.iter())
                    .map(|(text, id)| bm25::Document::new(id.to_string(), text.to_string()))
                    .collect::<Vec<_>>();

                let engine = SearchEngineBuilder::<String>::with_documents(lang_mode, documents)
                    .k1(k1)
                    .b(b)
                    .build();

                Engine::ProvidedIds(engine)
            }
            Nullable::Null => {
                let corpus_vec = corpus
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<String>>();

                let engine = SearchEngineBuilder::<u32>::with_corpus(lang_mode, corpus_vec)
                    .k1(k1)
                    .b(b)
                    .build();

                Engine::AutoIncrement(engine)
            }
        }
    }

    fn upsert(&mut self, id: Either<String, i32>, text: &str) {
        match (self, id) {
            (Engine::AutoIncrement(engine), Either::Right(id_int)) => {
                let id_u32 = (id_int - 1) as u32;
                let doc = bm25::Document::new(id_u32, text.to_string());
                engine.upsert(doc);
            }
            (Engine::ProvidedIds(engine), Either::Left(id_str)) => {
                let doc = bm25::Document::new(id_str, text.to_string());
                engine.upsert(doc);
            }
            _ => throw_r_error("ID type does not match engine type".to_string()),
        }
    }

    fn remove(&mut self, id: Either<String, i32>) {
        match (self, id) {
            (Engine::AutoIncrement(engine), Either::Right(id_int)) => {
                let id_u32 = (id_int - 1) as u32;
                engine.remove(&id_u32);
            }
            (Engine::ProvidedIds(engine), Either::Left(id_str)) => {
                engine.remove(&id_str);
            }
            _ => throw_r_error("ID type does not match engine type".to_string()),
        }
    }

    fn get(&self, id: Either<String, i32>) -> Robj {
        match (self, id) {
            (Engine::AutoIncrement(engine), Either::Right(id_int)) => {
                let id_u32 = (id_int - 1) as u32;
                match engine.get(&id_u32) {
                    Some(doc) => doc.contents.into(),
                    None => NULL.into(),
                }
            }
            (Engine::ProvidedIds(engine), Either::Left(id_str)) => match engine.get(&id_str) {
                Some(doc) => doc.contents.into(),
                None => NULL.into(),
            },
            _ => throw_r_error("ID type does not match engine type".to_string()),
        }
    }

    fn search(&self, query: &str, max_n: i32) -> Robj {
        match self {
            Engine::AutoIncrement(engine) => {
                let res = engine.search(query, max_n as usize);
                let ids = res
                    .iter()
                    .map(|r| Rint::from((r.document.id + 1) as i32))
                    .collect::<Integers>();
                let contents = res
                    .iter()
                    .map(|r| Rstr::from(r.document.contents.clone()))
                    .collect::<Strings>();
                let scores = res.iter().map(|r| r.score).collect::<Vec<_>>();
                data_frame!(id = ids, text = contents, score = scores)
            }
            Engine::ProvidedIds(engine) => {
                let res = engine.search(query, max_n as usize);
                let ids = res
                    .iter()
                    .map(|r| Rstr::from(r.document.id.clone()))
                    .collect::<Strings>();

                let contents = res
                    .iter()
                    .map(|r| Rstr::from(r.document.contents.clone()))
                    .collect::<Strings>();
                let scores = res.iter().map(|r| r.score).collect::<Vec<_>>();
                data_frame!(id = ids, text = contents, score = scores)
            }
        }
    }
}

// Helper to parse language string
fn parse_language(language: &str) -> LanguageMode {
    if language == "Detect" {
        return LanguageMode::Detect;
    }

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
        _ => {
            throw_r_error(format!("Language '{}' not supported", language));
        }
    };

    lang.into()
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod rbm25;
    fn build_engine;
    fn search;
    impl Engine;
}

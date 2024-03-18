# Funkce vezme vektor textů (např. vyhledávacích dotazů) a vrátí k nim text
# ebeddings. Embeddings k jednotlivým textům hledá nejprve v keši, a teprve když
# je tam nenajde, zeptá se OpenAI API. Embeddings nových textů pak do keše
# přidá. Tím šetří platby za API.

library(openai)

get_embeddings <- function(texts, cache_path, model = "text-embedding-3-small") {
  # Načtu keš
  if (file.exists(cache_path)) {
    cache <- readRDS(cache_path)
  } else {
    cache <- list()
  }

  # Vytvořím seznam textů, které chybí v keši
  missing_texts <- unique(texts[!texts %in% names(cache)])

  # Pokud nějaké texty v keši chybí, získám embeddings z OpenAI API
  if (length(missing_texts) > 0) {
    embeddings <- openai::create_embedding(
      model = model,
      input = missing_texts
    ) |>
      purrr::pluck("data", "embedding")
    names(embeddings) <- missing_texts
    cache <- c(cache, embeddings)

    # Uložím aktualizovanou keš
    saveRDS(cache, cache_path)
  }

  # Vrátím embeddings
  return(unname(cache[texts]))
}

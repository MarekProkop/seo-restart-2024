#' Ke každému dotazu v queries přidá nejlépe odpovídající segment
#'
#' @param queries Data frame dotazů. Musí obsahovat alespoň sloupce query a
#'   embedding.
#' @param segments Data frame segmentů. Musí obsahovat alespoň sloupce name a
#'   embedding.
#' @param threshold Práh pro segment Ostatní. Pokud je cosinová podobnost
#'   největší shody menší, dotaz se zařadí do segmentu Ostatní.
#' @param other_name Název pro segment Ostatní.
#'
#' @return Data frame queries doplněný o sloupec segment.
segment_queries <- function(queries, segments, threshold, other_name) {
  new_cols <- map2(
    segments$name, segments$embedding,
    \(x, y) {
      queries |>
        mutate(
          !!x := map_dbl(embedding, cosine_similarity, y),
          .keep = "none"
        )
    }
  ) |>
    bind_cols()

  queries |> bind_cols(
    new_cols
  ) |>
    select(!embedding) |>
    pivot_longer(
      cols = !query:position,
      names_to = "segment"
    ) |>
    group_by(query) |>
    slice_max(order_by = value) |>
    ungroup() |>
    mutate(
      segment = if_else(value > threshold, segment, other_name),
      segment = factor(segment, levels = c(segments$name, other_name))
    )
}

#' Spočítá cosinovou podobnost dvou seznamů s embeddings
#'
#' @param x Seznam embeddings více textů.
#' @param y Seznam ebedding jednoho textu.
#'
#' @return Vektor cosinových podobností.
cosine_similarity <- function(x, y) {
  sum(x * y)
}

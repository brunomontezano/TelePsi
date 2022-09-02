#' Criar histograma de escores da PROMIS no baseline e após 1 mês do tratamento
#'
#' @param dados Tibble ou dataframe com as variáveis a serem plotadas.
#' @param dominio Domínio a ser plotado. Pode assumir os valores `dep` (depressão), `anx` (ansiedade), `irr` (irritabilidade) ou `sat` (satisfação com a vida).
#'
#' @return Retorna um objeto do ggplot2.
#' @export
#'
plotar_hist_promis <- function(dados, dominio) {
    rotulo <- dplyr::case_when(
        dominio == "dep" ~ "Depress\u00E3o",
        dominio == "anx" ~ "Ansiedade",
        dominio == "irr" ~ "Irritabilidade",
        dominio == "sat" ~ "Satisfa\u00E7\u00E3o"
    )

    grafico <- dados |>
        dplyr::filter(!is.na(.data[[paste0("g_promis_", dominio)]])) |>
        ggplot2::ggplot() +
        ggplot2::geom_histogram(
            ggplot2::aes(x = .data[[paste0("b_promis_", dominio)]],
                         fill = "Baseline"),
            alpha = 0.5,
            position = "identity"
        ) +
        ggplot2::geom_histogram(ggplot2::aes(x = .data[[paste0("g_promis_", dominio)]],
                                             fill = "1 m\u00EAs"), alpha = 0.5) +
        ggplot2::scale_fill_manual(values = c(Baseline = "darkblue",
                                              "1 m\u00EAs" = "orange")) +
        ggplot2::theme_minimal(base_size = 20) +
        ggplot2::theme(legend.position = "top",
                       legend.title = ggplot2::element_blank()) +
        ggplot2::labs(
            x = paste0("Escore no dom\u00EDnio de ", rotulo, " da PROMIS"),
            y = "Frequ\u00EAncia",
            title = "Histograma dos escores de depress\u00E3o ap\u00F3s tratamento",
            subtitle = "Avalia\u00E7\u00E3o no baseline e 1 m\u00EAs ap\u00F3s o tratamento"
        )

    return(grafico)

}

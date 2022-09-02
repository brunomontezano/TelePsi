#' Criar gráfico de violino com boxplot de escores da PROMIS no baseline e após 1 mês do tratamento
#'
#' @param dados Tibble ou dataframe com as variáveis a serem plotadas.
#' @param dominio Domínio a ser plotado. Pode assumir os valores `dep` (depressão), `anx` (ansiedade), `irr` (irritabilidade) ou `sat` (satisfação com a vida).
#'
#' @return Retorna um objeto do ggplot2.
#' @export
#'
plotar_violin_promis <- function(dados, dominio) {
    rotulo <- dplyr::case_when(
        dominio == "dep" ~ "Depress\u00E3o",
        dominio == "anx" ~ "Ansiedade",
        dominio == "irr" ~ "Irritabilidade",
        dominio == "sat" ~ "Satisfa\u00E7\u00E3o"
    )

    grafico <- dados |>
        dplyr::filter(!is.na(.data[[paste0("g_promis_", dominio)]])) |>
        tidyr::pivot_longer(
            cols = c(
                paste0("b_promis_", dominio),
                paste0("g_promis_", dominio)
            ),
            names_to = paste0("promis_", dominio),
            values_to = paste0("valores_promis_", dominio)
        ) |>
        ggplot2::ggplot() +
        ggplot2::geom_violin(ggplot2::aes(
            x = as.factor(.data[[paste0("promis_", dominio)]]),
            y = .data[[paste0("valores_promis_", dominio)]],
            fill = .data[[paste0("promis_", dominio)]]
        )) +
        ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(.data[[paste0("promis_", dominio)]]),
                                           y = .data[[paste0("valores_promis_", dominio)]])) +
        ggplot2::scale_x_discrete(labels = c(
            #paste0("b_promis_", dominio) = "Baseline",
            #paste0("g_promis_", dominio) = "1 m\u00EAs"
            "Baseline", "1 m\u00EAs"
        )) +
        ggplot2::theme_minimal(base_size = 20) +
        ggplot2::labs(
            x = "Fase de acompanhamento",
            y = "Escores de Depress\u00E3o da PROMIS",
            title = paste0(
                "Gr\u00E1fico de violino e boxplot dos escores de ",
                rotulo,
                " ap\u00F3s tratamento"
            ),
            subtitle = "Avalia\u00E7\u00E3o no baseline e 1 m\u00EAs ap\u00F3s o tratamento"
        ) +
        ggplot2::theme(legend.position = "none")

    return(grafico)

}

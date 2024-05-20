#' Aplica unnest de forma recursiva em colunas de data frame ou lista
#'
#' Esta função aplica a função unnest de forma recursiva em colunas de um data frame ou lista,
#' expandindo as colunas que são data frames ou listas até que não haja mais aninhamentos.
#'
#' @param df O data frame ou lista a ser processado.
#' @return O data frame resultante após aplicar unnest de forma recursiva.
#' @import dplyr
#' @import tidyr
#' @importFrom purrr keep map_lgl
#' @export
#'
#' @examples
#' df <- data.frame(
#'   col1 = list(1, list(2, 3)),
#'   col2 = list(data.frame(a = 1:2, b = 3:4), data.frame(c = 5:6))
#' )
#' unnest_recursivo(df)
#'
unnest_recursivo <- function(df) {
  # Função para verificar se uma coluna é um data frame ou uma lista
  is_df_or_list <- function(col) {
    is.data.frame(col) || is.list(col)
  }

  # Loop até que não haja mais colunas que sejam data frames ou listas
  while(any(purrr::map_lgl(df, is_df_or_list))) {
    # Seleciona as colunas que são data frames ou listas
    columns <- df %>%
      purrr::keep(is_df_or_list) %>%
      names()

    for (list_column in columns) {

    df <- df %>%
          tidyr::unnest(
            dplyr::all_of(list_column),
            keep_empty = TRUE,
            names_sep = "_",
            names_repair = "universal"
          )
    }
    # Aplica unnest() nas colunas selecionadas

  }

  return(df)
}

#' Função unnest_all
#'
#' Esta função expande todas as colunas que contêm listas ou data frames em um data frame.
#'
#' @param df O data frame a ser expandido.
#' @param recursively Um valor lógico indicando se a função deve ser aplicada recursivamente.
#' @param ... Outros argumentos a serem passados para a função `unnest()`.
#' @return O data frame expandido.
#' @importFrom dplyr %>%
#' @importFrom tidyr unnest
unnest_all <- function(df, recursively = TRUE, ...) {

  if (recursively) {

    while (df %>% dplyr::select(dplyr::where(~is.data.frame(.) || is.list(.))) %>% ncol() != 0) {
      columns <- df %>% dplyr::select(dplyr::where(~is.data.frame(.) || is.list(.))) %>% names()

      if (length(columns) == 0) {
        return(df)
      }

      for (list_column in columns) {
        df <- df %>%
          tidyr::unnest(dplyr::all_of(list_column), ...)
      }
    }

  } else {
    columns <- df %>% dplyr::select(dplyr::where(~is.list(.) || is.data.frame(.))) %>% names()

    if (length(columns) == 0) {
      return(df)
    }

    for (list_column in columns) {
      df <- df %>%
        tidyr::unnest(dplyr::all_of(list_column), names_sep = "_")
    }
  }

  return(df)
}


#' Trata Processos
#'
#' Esta função realiza o tratamento de dados de processos.
#'
#' @param initial_path O caminho para o arquivo inicial.
#' @param dest_folder O diretório de destino para salvar o arquivo tratado.
#' @return Nenhum valor é retornado explicitamente, o arquivo tratado é salvo no diretório de destino.
#' @importFrom readr read_rds
#' @importFrom tidyr unnest
#' @importFrom stringr str_extract_all
#' @importFrom glue glue
#' @importFrom purrr map_chr
#' @importFrom tidyr unnest
#' @export
trata_bdc_processes <- function(initial_path, dest_folder) {
  # Tratamento dos dados
  tratado <- data %>%
    MarsCraft:::unnest_all(recursively = FALSE) %>%
    MarsCraft:::unnest_all(recursively = FALSE) %>%
    dplyr::mutate(MatchKeys = str_extract_all(MatchKeys, "\\d+") %>% purrr::map_chr(~paste(.x, collapse = "")))

  # Verifica e filtra colunas
  if ("Lawsuits_Lawsuits_Parties" %in% colnames(tratado)) {
    tratado <- tratado %>%
      tidyr::unnest(Lawsuits_Lawsuits_Parties) %>%
      dplyr::filter(Doc == MatchKeys, Polarity == 'PASSIVE')
  }

  # Seleciona e escreve os dados tratados
  tratado %>%
    dplyr::select(where(~!is.list(.) & !is.data.frame(.))) %>%
    readr::write_rds(glue::glue(dest_folder, file))
}



#' Trata Processo de Sócios
#'
#' Esta função realiza o tratamento de dados de processos de sócios.
#'
#' @param initial_path O caminho para o arquivo inicial.
#' @param dest_folder O diretório de destino para salvar o arquivo tratado.
#' @return Nenhum valor é retornado explicitamente, o arquivo tratado é salvo no diretório de destino.
#' @importFrom readr read_rds
#' @importFrom dplyr %>%
#' @importFrom tidyr unnest
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract_all
#' @importFrom purrr map
#' @importFrom glue glue
#' @export
trata_bdc_owners_lawsuits <- function(initial_path, dest_folder) {
  data <- readr::read_rds(initial_path)

  file <- basename(initial_path)

  tratado <- data %>%
    MarsCraft:::unnest_all(recursively = FALSE, keep_empty = TRUE)


  if (
    nrow(tratado) == 1 && tratado$OwnersLawsuits_TotalLawsuits != 0 ||
    all(tratado$OwnersLawsuits_TotalLawsuits) != 0
  ) {
    tratado <- tratado %>%
      tidyr::unnest(OwnersLawsuits_Lawsuits) %>%
      tidyr::pivot_longer(matches("\\d$")) %>%
      tidyr::unnest(value, keep_empty = TRUE, names_sep = "_") %>%
      dplyr::mutate(
        value_Lawsuits = purrr::map(value_Lawsuits, ~.x %>% purrr::pluck() %>% as.data.frame())
      ) %>%
      tidyr::unnest(value_Lawsuits, keep_empty = TRUE) %>%
      dplyr::mutate(
        cpf_socio = stringr::str_extract_all(name, "\\d+")
      )

    if ("Parties" %in% colnames(tratado)) {
      tratado <- tratado %>%
        tidyr::unnest(Parties, keep_empty = TRUE, names_sep = "_") %>%
        dplyr::filter(cpf_socio == Parties_Doc, Parties_Polarity == 'PASSIVE') %>%
        dplyr::select(-MatchKeys) %>%
        dplyr::distinct(Number, .keep_all = TRUE)
    }
  } else {
    tratado <-
      tratado %>%
      dplyr::distinct(MatchKeys, .keep_all = TRUE)
  }

  tratado %>%
    dplyr::select(dplyr::where(~!is.list(.) & !is.data.frame(.))) %>%
    readr::write_rds(glue::glue(dest_folder, file))
}


#' Trata Processo de Sócios
#'
#' Esta função realiza o tratamento de dados de processos de sócios.
#'
#' @param initial_path O caminho para o arquivo inicial.
#' @return Nenhum valor é retornado explicitamente, o arquivo tratado é salvo no diretório de destino.
#' @importFrom readr read_rds
#' @importFrom dplyr %>%
#' @importFrom tidyr unnest
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract_all
#' @importFrom purrr map
#' @importFrom glue glue
#' @export
trata_bdc <- function(initial_path, type) {
  # Lê o arquivo inicial
  data <- readr::read_rds(initial_path)

  # Obtém o nome do arquivo
  file <- basename(initial_path)

  if(type == 'owners_lawsuits'){
    trata_bdc_owners_lawsuits(initial_path)
  }else if(type == 'processes'){
    trata_bdc_processes(initial_path = initial_path)
  }

}



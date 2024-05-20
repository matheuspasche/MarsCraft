#' Conserta documentos
#'
#' Esta função recebe um documento ou vetor de documentos e conserta seu formato,
#' completando com zeros à esquerda se necessário.
#'
#' @param x O documento ou vetor de documentos a serem consertados.
#' @return O documento ou vetor de documentos corrigidos.
#' @export
consertar_documentos <- function(x) {
  # Aplica a função para consertar em cada documento do vetor
  docs_consertados <- purrr::map_chr(x, function(doc) {
    # Verifica o tamanho do documento
    nchar_doc <- nchar(doc)
    if (nchar_doc <= 11) {
      # Completa com zeros à esquerda até 11 caracteres
      doc <- paste0(strrep("0", 11 - nchar_doc), doc)
    } else if (nchar_doc <= 14) {
      # Completa com zeros à esquerda até 14 caracteres
      doc <- paste0(strrep("0", 14 - nchar_doc), doc)
    } else {
      doc <- NA
    }
    return(doc)
  })
  return(docs_consertados)
}


#' Remove colunas com todos os valores vazios ou repetidos
#'
#' Esta função remove as colunas de um data frame que possuem todos os valores vazios
#' ou contêm o mesmo valor repetido em todas as linhas.
#'
#' @param df O data frame a ser processado.
#' @return Um novo data frame sem as colunas mencionadas.
#' @export
remover_colunas_vazias <- function(df) {
  # Verifica se todos os valores em um vetor são iguais
  all_equal <- function(x) length(unique(x[!is.na(x)])) == 1

  # Dropa as colunas com todos os valores vazios ou todos os valores iguais
  df %>%
    purrr::keep(~!all(is.na(.)) & !all(. == .[[1]] | is.na(.)))
}

#' @title value_counts
#' @description Cuenta valores distintos en una columna de data.frame
#'
#' @param column Columna de data.frame.
#' @param sort TRUE si queremos que lo devuelva ordenado por frecuencia.
#'
#' @return data.table data.frame
#' @export

value_counts <- function(column, sort = T){
  if (sort){
    dt <- table(column) %>% as.data.table() %>%
      setnames(., colnames(.), c("Value", "Counts")) %>%
      mutate("Freq %" = Counts / sum(Counts) * 100) %>% arrange(-Counts)
  } else {
    dt <- table(column) %>% as.data.table() %>%
      setnames(., colnames(.), c("Value", "Counts")) %>%
      mutate("Freq %" = Counts / sum(Counts) * 100)
  }
  return(dt)
}

#' @title types
#' @description Función que devuelve el tipo de dato en cada columna del data.frame
#'
#' @param dt Data frame del que queremos saber los tipo de datos.
#' @param sort TRUE si queremos que lo devuelva ordenado por frecuencia.
#'
#' @return data.table data.frame
#' @export

types <- function(dt, sort = T){
  t <- data.frame(
    "var"     = names(unlist(sapply(dt, class))),
    "types"   = unlist(sapply(dt, class)),
    row.names = NULL
  )

  if (sort){
    t %<>% arrange(var)
  }

  return(t)
}

#' @title grepdt
#' @description Esta funcion busca todas las columnas cuyo nombre coincida con el str dado.
#'
#' @param dt Data frame
#' @param str Cadena de caracteres que queremos encontrar o regex.
#'
#' @return vector de caracteres
#' @export

grepdt <- function(dt, str){
  return(colnames(dt)[grepl(str, colnames(dt))])
}

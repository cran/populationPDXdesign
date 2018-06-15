#' @title Function to return the mode of a vector of values
#' @description This is an internal function. Please use cautiously if calling directly. Returns the mode from numeric vector. Example usage: \code{getMode(c(0,1,1))}
#' 
#' @param v vector of numeric values
#'
#' @return a numeric value 
#'
#' @author Maria Luisa Guerriero, \email{maria.guerriero@@astrazeneca.com}
#' @author Natasha A. Karp, \email{natasha.karp@@astrazeneca.com} 
#'
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

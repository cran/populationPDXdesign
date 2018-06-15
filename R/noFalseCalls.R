#' @title Function to calculate the number of false calls for a design for a go-no go threshold 
#' 
#' @description This is an internal function. Please use cautiously if calling directly. Returns the number of false calls from a simulation study exploring the impact of varying PDXn and PDXr for an underlying Biol_RR for a particularly go-no go threshold.
#' A false call can only arise in the situation where the underlying Biol_RR is below the go-no go threshold.
#' Example usage: \code{noFalseCalls(ImpactVarying_PDXn_PDXr_BRR, GoNoGoThreshold=30)}
#'
#' @param dataset dataset obtained as output from the 'varying_PDXn_PDXr' function
#' @param GoNoGoThreshold go-no go threshold
#' 
#' @return vector with three elements:
#' @return - numeric value indicating the number of experiments simulated
#' @return - numeric value indicating the number of experiments which were above the go-no go threshold
#' @return - numeric value indicating the FPR
#' 
#' @author Maria Luisa Guerriero, \email{maria.guerriero@@astrazeneca.com}
#' @author Natasha A. Karp, \email{natasha.karp@@astrazeneca.com}
#'
noFalseCalls <- function(dataset, GoNoGoThreshold){
  n1 <- sum(is.finite(dataset[ , "Results_Percentlines_R"]))
  n_Hits <- sum(dataset[ , "Results_Percentlines_R"]>=GoNoGoThreshold)
  outcome <- c(n1, n_Hits, (n_Hits/n1)*100)
  names(outcome) <- c("numberExperiment", "number_Hit" , "FalsePositiverate")
  return(outcome)
}

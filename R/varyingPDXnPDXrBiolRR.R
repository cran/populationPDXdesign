#' @title Function to run simulations to mimic population PDX studies for variety of experimental and biological scenarios
#' 
#' @description This is an internal function. Please use cautiously if calling directly.
#' Simulations are used to mimic population PDX studies by inputing a variety of experimental factors (PDXn and PDXr) and biological factors (Biol_RR and C_Acc).
#' Example usage: \code{varyingPDXnPDXrBiolRR(PDXn_range=c(8,10,12), PDXr_range=c(1,3,5), Biol_RR_range=c(30,40,50), C_Acc=0.95, iterations=500)}
#'
#' @param PDXn_range a vector of PDXn values to study
#' @param PDXr_range a vector of PDXr values to study
#' @param Biol_RR_range a vector of values between 0 and 100 to indicate the Biol_RR to study
#' @param C_Acc the classification accuracy (numeric value between 0 and 1)
#' @param iterations iterations
#'
#' @return a dataframe where each row represents the results from a simulation mimicking an individual experiment for a particular design with meta data returned to describe the experimental design
#'
#' @author Maria Luisa Guerriero, \email{maria.guerriero@@astrazeneca.com}
#' @author Natasha A. Karp, \email{natasha.karp@@astrazeneca.com} 
#'
varyingPDXnPDXrBiolRR <- function(PDXn_range, PDXr_range, Biol_RR_range, C_Acc, iterations) {

  results <- data.frame(matrix(nrow=iterations*length(Biol_RR_range)*length(PDXr_range)*length(PDXn_range), ncol=9))
  colnames(results) <- c("Iteration",  
                         "Nolines_R", "Percent_lines_R",  
                         "Results_lines_R", "Results_Percentlines_R", 
                         "PDXn", "PDXr", "C_Acc", "Biol_RR")
  i <- 0
  for(Biol_RR in Biol_RR_range) {
    for(PDXr in PDXr_range) {
      for (PDXn in PDXn_range) {
        firstConditionResults <- outcomeMultipleExperiments(PDXn=PDXn, PDXr=PDXr, C_Acc, Biol_RR=Biol_RR, iterations)
        
        results[((iterations*i)+1):(iterations*(i+1)),] <- firstConditionResults
        i <- i+1
      }
    }
  }
  return(results)
}

#' @title Function to run simulations to mimic population PDX studies for a defined scenario
#' 
#' @description This is an internal function. Please use cautiously if calling directly. Simulations are used to mimic population PDX studies for specified values of PDXn, PDXr, Biol_RR and C_Acc.
#' Example usage: \code{outcomeMultipleExperiments(PDXn=8, PDXr=3, C_Acc=0.95, Biol_RR=30, iterations=500)}
#' 
#' @param PDXn PDXn
#' @param PDXr PDXr
#' @param C_Acc the classification accuracy (numeric value between 0 and 1)
#' @param Biol_RR Biol_RR
#' @param iterations no of experiments to simulated
#' 
#' @return a dataframe where each row represents the results from a simulation mimicking an individual experiment for a particular design with meta data returned to describe the experimental design
#' 
#' @author Maria Luisa Guerriero, \email{maria.guerriero@@astrazeneca.com}
#' @author Natasha A. Karp, \email{natasha.karp@@astrazeneca.com}
#'
outcomeMultipleExperiments <- function(PDXn, PDXr, C_Acc, Biol_RR, iterations){
  results <- vector('list', iterations)
  for (i in seq_len(iterations)) {
    iteration_name  <- paste("Iteration", i)
    iteration_frame <- data.frame(Iteration=iteration_name, stringsAsFactors = F)
    
    df_outcomeExp <- callsInSingleExperiment(PDXn, PDXr, C_Acc, Biol_RR)
    summaryOutcome <- outcomeInSingleExperiment(df=df_outcomeExp, PDXn, PDXr, C_Acc, Biol_RR)
    
    result_frame <- do.call('data.frame', as.list(summaryOutcome)) # convert vector to dataframe row
    result_frame <- cbind(iteration_frame, result_frame) # bind on the iteration number
    results[[i]] <- result_frame # add to list
  }
  results <- do.call('rbind', results)
  return(results)
}

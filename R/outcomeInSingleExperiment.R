#' @title Function to summarise the results for a single simulation representing one experiment
#' 
#' @description This is an internal function. Please use cautiously if calling directly. From a simulation of a single experiment, the estimated response rate is determined and captured with the meta data (e.g. PDXn, PDXr) for that experiment.
#' Example usage: \code{outcomeInSingleExperiment(df=outcomeInSingleExperiment_1, PDXn=8, PDXr=3, C_Acc=0.95, Biol_RR=30)}
#' 
#' @param df data frame from callsInSingleExperiment
#' @param PDXn PDXn
#' @param PDXr PDXr
#' @param C_Acc the classification accuracy (numeric value between 0 and 1)
#' @param Biol_RR Biol_RR
#' 
#' @return a vector with 8 values that captures the input design and the estimated response rate for that design from a single simulation
#' 
#' @author Maria Luisa Guerriero, \email{maria.guerriero@@astrazeneca.com}
#' @author Natasha A. Karp, \email{natasha.karp@@astrazeneca.com}
#'
outcomeInSingleExperiment <- function(df, PDXn, PDXr, C_Acc, Biol_RR){
  NoLinesResponders <- sum(df[ , "PDXclassification"]==1)
  PerLinesResponders <- (NoLinesResponders/PDXn)*100
  Results_NoLinesResponders <- sum(df[ , "StudyResult"]==1)
  Results_PerLinesResponders <- (Results_NoLinesResponders/PDXn)*100
  
  out <- c(NoLinesResponders, PerLinesResponders, Results_NoLinesResponders, Results_PerLinesResponders, 
           PDXn, PDXr, C_Acc, Biol_RR )
  names(out) <- c("Nolines_R","Percent_lines_R", "Results_lines_R", "Results_Percentlines_R",
                  "PDXn", "PDXr", "C_Acc", "Biol_RR")
  return(out)
}

#' @title Simulation of a single population PDX experiment
#' 
#' @description This is an internal function. Please use cautiously if calling directly.
#' Samples some animals and classify as responders or non-responders based on number of models studied (PDXn), number of mice measured per model (PDXr), the classification accuracy (C_Acc) and the underlying biological response rate (Biol_RR).
#' Example usage: \code{callsInSingleExperiment(PDXn=8, PDXr=3, C_Acc=0.95, Biol_RR=30)}
#'
#' @param PDXn number of PDX models studied
#' @param PDXr number of mice measured per PDX model
#' @param Biol_RR underlying biological response rate for this treatment 
#' @param C_Acc classification accuracy
#' 
#' @return dataframe with three columns:
#' @return - PDXModel is a string that indicates the model id
#' @return - PDXclassification is a numeric value that indicates the true biological classification of that PDX - 0 equal non-responder and 1 equal responder
#' @return - StudyResult is a numeric value that indicates the classification of the PDX model after sampling - 0 equal non-responder and 1 equal responder
#'
#' @author Maria Luisa Guerriero, \email{maria.guerriero@@astrazeneca.com}
#' @author Natasha A. Karp, \email{natasha.karp@@astrazeneca.com}
#'
callsInSingleExperiment <- function(PDXn, PDXr, C_Acc, Biol_RR) {
  out=c()
  
  PDXclassification <- rbinom(n=PDXn, size=1, prob=Biol_RR/100) # For the underlying biological response rate - sample some models and classify as responding or not
  
  linesTested <- c(1:length(PDXclassification))
  
  for (l in linesTested) {
    
    lineClassification <- PDXclassification[l]  #0 = non responder, 1 = responder
    if (lineClassification==1) {
      lineOutcome <- getMode(rbinom(n=PDXr, size=1, prob=C_Acc)) # return the most frequent classification of the line accounting for sampling depth and classificaiton accuracy
    } else {
      lineOutcome <- getMode(1-rbinom(n=PDXr, size=1, prob=C_Acc)) # The 1- element is to switch the sampling to represent that the line classification is of a non responder
    }
    
    out <- c(out, lineOutcome)
    PDXModel <- c(paste(rep("PDXmodel", length(PDXclassification)), c(1:length(PDXclassification))))
  }
  
  result <- data.frame(PDXModel, PDXclassification, StudyResult=out)
  return(result)
}

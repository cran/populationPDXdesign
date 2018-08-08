#' @title A function to visualise the sensitivity as a function of PDXn and PDXr
#' 
#' @description This is an internal function. Please use cautiously if calling directly.
#' A visualisation of the sensitivity from the simulations 
#' 
#' @param data data frame with four columns which indicate the PDXn, PDXr, Biol_RR and the MissedCalls for a specified go-no go threshold
#'
#' @return a graphic visualisation
#'
#' @author Maria Luisa Guerriero, \email{maria.guerriero@@astrazeneca.com}
#' @author Natasha A. Karp, \email{natasha.karp@@astrazeneca.com} 
#'
plotSensitivity <- function(data) {
  ggplot(data=data,  mapping=aes_string(x="PDXn", y="MissedCalls")) +
  geom_point(size=4, mapping=aes(col=paste(data$Biol_RR, "%"))) +
  geom_smooth(method="loess", se=FALSE,mapping=aes(col=paste(data$Biol_RR, "%"))) +
  xlab("Number of PDX models") +
  ylab("Missed calls (%)") +
  scale_x_continuous(breaks = data$PDXn,
                     labels = data$PDXn) +
  guides(colour=guide_legend("Biological\nresponse rate")) +
  #ylim(0,100) +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold"), legend.title=element_text(size=16,face="bold"),
        legend.title.align=0.5, strip.text = element_text(size=16, face="bold"), strip.text.y = element_text(angle = 0)) +
  facet_grid(PDXr~., labeller = as_labeller(c(`1` = "Number of animals\nper PDX model\n\n\n1",`3` = "Number of animals\nper PDX model\n\n\n3",`5` = "Number of animals\nper PDX model\n\n\n5",`7` = "Number of animals\nper PDX model\n\n\n7")))
}

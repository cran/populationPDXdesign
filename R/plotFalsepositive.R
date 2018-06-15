#' @title A function to visualise the false postive rate as a function of PDXn and PDXr
#' 
#' @description This is an internal function. Please use cautiously if calling directly.
#' A visualisation of the false positive rate behaviour from the simulations 
#' 
#' @param data data frame with four columns which indicate the PDXn, PDXr, Biol_RR and the FPR for a specified go-no go threshold
#'
#' @return a graphic visualisation
#'
#' @author Maria Luisa Guerriero, \email{maria.guerriero@@astrazeneca.com}
#' @author Natasha A. Karp, \email{natasha.karp@@astrazeneca.com} 
#'
plotFalsepositive <- function(data) {
  ggplot(data=data,  mapping=aes(x=data$PDXn, y=data$FalsePositiverate, col=data$PDXr)) +
  geom_point(size=4) +
  geom_line() +
  xlab("Number of PDX models") +
  ylab("False calls (%)") +
  scale_x_continuous(breaks = data$PDXn,
                     labels = data$PDXn) +
  guides(colour=guide_legend("Number of animals\nper PDX model")) +
  #ylim(0,100) +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold"), legend.title=element_text(size=16,face="bold"),
        legend.title.align=0.5, strip.text = element_text(size=16, face="bold"), strip.text.y = element_text(angle = 0)) +
  facet_grid(Biol_RR~., labeller = as_labeller(c(`0` = "Biological\nresponse rate\n\n\n0%",`5` = "Biological\nresponse rate\n\n\n5%",`10` = "Biological\nresponse rate\n\n\n10%",`15` = "Biological\nresponse rate\n\n\n15%",`20` = "Biological\n response rate\n\n\n20%",`25` = "Biological\n response rate\n\n\n25%",`30` = "Biological\n response rate\n\n\n30%",`35` = "Biological\n response rate\n\n\n35%",`40` = "Biological\n response rate\n\n\n40%",`45` = "Biological\n response rate\n\n\n45%",`50` = "Biological\n response rate\n\n\n50%",`55` = "Biological\n response rate\n\n\n55%",`60` = "Biological\n response rate\n\n\n60%",`65` = "Biological\n response rate\n\n\n65%",`70` = "Biological\n response rate\n\n\n70%",`75` = "Biological\n response rate\n\n\n75%",`80` = "Biological\n response rate\n\n\n80%",`85` = "Biological\n response rate\n\n\n85%",`90` = "Biological\n response rate\n\n\n90%",`95` = "Biological\n response rate\n\n\n95%",`100` = "Biological\n response rate\n\n\n100%")))
}

#' @title 'shiny' app server function
#' @description This is an internal function. Please use cautiously if calling directly
#' 
#' @param input input
#' @param output output
#' @param session session
#'
#' @author Maria Luisa Guerriero, \email{maria.guerriero@@astrazeneca.com}
#' @author Natasha A. Karp, \email{natasha.karp@@astrazeneca.com} 
#' 
server <- function(input, output, session) {

  plotheight1 <- reactive({
   Biol_RR_range <- seq(max(0,input$GoNoGoThreshold-20), input$GoNoGoThreshold-10,5)
   plotheight1 <<- (((length(Biol_RR_range)-1) %/% 3 + 1) * 800)
   return(plotheight1)
  })
  
  plotheight2 <- reactive({
   PDXr_range <- seq(input$PDXr[1], input$PDXr[2],2)
   plotheight2 <<- (((length(PDXr_range)-1) %/% 1 + 1) * 250)
   return(plotheight2)
  })
  
  runFalsepositive <- reactive({
    withProgress(message = paste('Running'), detail = paste('Please wait...'), value = NULL, {

      PDXn_range       <- seq(input$PDXn[1], input$PDXn[2],as.numeric(input$PDXn_step))
      PDXr_range       <- seq(input$PDXr[1], input$PDXr[2],2)
      Biol_RR_range    <- seq(max(0,input$GoNoGoThreshold-20), input$GoNoGoThreshold-10,5)
      C_Acc            <- input$C_Acc/100
      iterations       <- as.numeric(input$iterations)
      
      runFalsepositive <- varyingPDXnPDXrBiolRR(PDXn_range=PDXn_range, PDXr_range=PDXr_range, C_Acc=C_Acc, Biol_RR_range=Biol_RR_range, iterations=iterations)
      return(runFalsepositive)
    })
  })
  
  runSensitivity <- reactive({
    withProgress(message = paste('Running'), detail = paste('Please wait...'), value = NULL, {
      
      PDXn_range       <- seq(input$PDXn[1], input$PDXn[2],as.numeric(input$PDXn_step))
      PDXr_range       <- seq(input$PDXr[1], input$PDXr[2],2)
      Biol_RR_range    <- seq(input$GoNoGoThreshold+10, min(100,input$GoNoGoThreshold+40),10)
      C_Acc            <- input$C_Acc/100
      iterations       <- as.numeric(input$iterations)
      
      runSensitivity <- varyingPDXnPDXrBiolRR(PDXn_range=PDXn_range, PDXr_range=PDXr_range, C_Acc=C_Acc, Biol_RR_range=Biol_RR_range, iterations=iterations)
      return(runSensitivity)
    })
  })

  output$plotFalsepositive <- renderPlot({
    # Assessing the rate of false calls as a function of PDXn and PDXr

    GoNoGoThreshold <- input$GoNoGoThreshold
    
    ImpactVarying_PDXn_PDXr_Biol_RR <- runFalsepositive()

    summaryByBiol_RR=ddply(.data=ImpactVarying_PDXn_PDXr_Biol_RR, .variables=c("PDXn", "PDXr", "Biol_RR"), .fun=noFalseCalls, GoNoGoThreshold=GoNoGoThreshold)
    summaryByBiol_RR$PDXr=as.factor(summaryByBiol_RR$PDXr)
    summaryByBiol_RR$Biol_RR=as.factor(summaryByBiol_RR$Biol_RR)
    
    plotFalsepositive(summaryByBiol_RR)
    
  }, height = plotheight1)
  
  output$plotSensitivity <- renderPlot({
    #Assessing sensitivity  as a function of PDXn and PDXr

    GoNoGoThreshold <- input$GoNoGoThreshold
    
    ImpactVarying_PDXn_PDXr_Biol_RR <- runSensitivity()
    
    summary_sensitivity=ddply(.data=ImpactVarying_PDXn_PDXr_Biol_RR, .variables=c("PDXn", "PDXr", "Biol_RR"), .fun=noMissedCalls, GoNoGoThreshold=GoNoGoThreshold)
    summary_sensitivity$PDXr=as.factor(summary_sensitivity$PDXr)
    summary_sensitivity$Biol_RR=as.factor(summary_sensitivity$Biol_RR)
    
    plotSensitivity(summary_sensitivity)

  }, height = plotheight2)
  
  session$onSessionEnded(stopApp)
}

options(spinner.color.background="white")

#' @title 'shiny' app user interface function
#' @description This is an internal function. Please use cautiously if calling directly.
#'
#' @author Maria Luisa Guerriero, \email{maria.guerriero@@astrazeneca.com}
#' @author Natasha A. Karp, \email{natasha.karp@@astrazeneca.com}
#'
ui <- function(){
  fluidPage(
  sidebarLayout(
    sidebarPanel(
      #If multiple animals are assessed per line the majority lead to the line classification e.g. if 2/3  R then line R i.e. as a proportion value
      #Numeric value  between 0 and 1 e.g. 0.33
      sliderInput("GoNoGoThreshold","Go-No go threshold (%)", min=20, max=80, value=30, step = 10),
      
      #PDXn
      #= number of PDX models assessed  which could vary between 8 and 40 
      # numeric value - only integers allowed
      fluidRow(
        column(9, sliderInput("PDXn","Number of PDX models", min=8, max=40, value=c(8,16), step = 1)),
        column(3, radioButtons("PDXn_step","Increment", choices=c(1,2,5), selected = 2, inline=FALSE))
       ),
      
      #PDXr
      #= number of replicate animals per PDX model  Question what is the impact of n=1 versus n=3? 
      # numeric value - only odd integer values allowed
      sliderInput("PDXr","Number of animals per PDX model", min=1, max=7, value=c(1,3), step = 2),
      
      #Biol_RR
      #= Biological Response Rate= proportion of lines which will respond varies between 20 and 60% 
      #Biol_RR is numeric value between 0 and 1 e.g. 0.6
      #sliderInput("Biol_RR","Biological response rate", min=0, max=1, value=c(0.1,0.25), step = 0.05),
      
      #C_Acc
      #= Classification Accuracy = 70 to 95%  as a proportion  (Gao et al 2015 Nature Medicine suggest it is 95%)
      #numeric value between between 0 and 1 e.g. 0.95
      sliderInput("C_Acc","Classification accuracy (%)", min=70, max=100, value=95, step = 1),
      
      radioButtons("iterations","Iterations", choices=c(300,500), selected = 300, inline=TRUE),

      submitButton("Go"),
      
      width = 4
    ),
    mainPanel(
     tabsetPanel(
      tabPanel("DESCRIPTION",
               verticalLayout(
                 h1("Designing Population PDX studies"),
                 h3("Natasha A. Karp and Maria Luisa Guerriero"),
                 h3("Quantitative Biology, Discovery Sciences, IMED Biotech Unit, AstraZeneca"),
                 br(),
                 strong("Objective:"),
                 p("Simulations to assess the impact of various designs features and the underlying biological behaviour on the outcome of a Patient Derived Xenograft (PDX) population studies.
                    In these studies, the response to the treatment is classed as a responder or non-responder based on the RECIST (Response evaluation criteria in solid tumours) criteria [1]"),
                 strong("Variables:"),
                 tags$ul(
                   tags$li("Go-No go threshold"),
                           tags$ul(tags$li("The minimum percent of the target population that respond to the treatment for research to proceed further for this treatment.")),
                   tags$li("Number of PDX models"),
                           tags$ul(tags$li("The range of PDX models considered. This simulation assumes that the PDX models have been selected as a random sample from the target population for this treatment.")),
                   tags$li("Increment"),
                           tags$ul(tags$li("The step-wise increase in number of PDX models tested.")),
                   tags$li("Number of animals per PDX models  (PDXr)"),
                           tags$ul(tags$li("The range of animals per PDX model considered. This is restricted to an odd number as the PDX model will be classified as a responder or non-responder based on the most frequently observed classification.")),
                   tags$li("Classification accuracy"),
                           tags$ul(tags$li("The percentage accuracy when comparing the individual observed classifications at an animal level compared to the classification at a model level.  The default value is 95% based on the research published by Gao", tags$em("et al."), "[2].")),
                   tags$li("Iterations"),
                           tags$ul(tags$li("The number of resampling repetitions used to simulate the behaviour.")),
                   tags$li("Biological response rate (Biol_RR)"),
                           tags$ul(tags$li("The underlying population response rate being studied. This parameter is automatically selected based on the selected Go-No go threshold."))),
                 strong("Outcome:"),
                 tags$ul(
                   tags$li("Assessment of the sensitivity"),
                           tags$ul(tags$li("For scenarios where the underlying biological response rate exceeds the Go-No go threshold, the percentage missed calls is assessed.")),
                   tags$li("Assessment of the false positive rate"),
                           tags$ul(tags$li("For scenarios where the underlying biological response rate is below the Go-No go threshold, the percentage of experiments where the threshold is exceeded is assessed."))),
                 strong("Usage:"),
                 p("To drive the analysis, once you have selected the values of interest, select the tab", strong("(False positive rate"), "or", strong("Sensitivity)"), " of interest. Press the", strong("GO"), "button after having amended the values as required to apply the changes."),
                 strong("To assess the impact of these bespoke experimental settings, large number of simulations are run prior to the data being summarised with measures of sensitivity and false positive rate.  Consequently, the graphs take time to render. This time can be shortened by minimising the range or increasing the size of the step you select in your exploration of the factors."),
                 br(),
                 strong("References:"),
                   p("1: Therasse, Patrick", tags$em("et al."), "New guidelines to evaluate the response to treatment in solid tumors.\" Journal of the National Cancer Institute 92.3 (2000): 205-216."),
                   p("2: Gao, Hui", tags$em("et al."), "High-throughput screening using patient-derived tumor xenografts to predict clinical trial drug response.\" Nature medicine 21.11 (2015): 1318.")
               )
      ),
      tabPanel("False positive rate",
               withSpinner(plotOutput("plotFalsepositive"), type=4)
      ),
      tabPanel("Sensitivity",
               withSpinner(plotOutput("plotSensitivity"), type=4)
      )
    ))
  )
)}

#' @title Function \code{populationPDXdesignApp}
#' @description Runs the 'shiny' app.
#' 
#' @author Maria Luisa Guerriero, \email{maria.guerriero@@astrazeneca.com}
#' @author Natasha A. Karp, \email{natasha.karp@@astrazeneca.com} 
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#' populationPDXdesignApp()
#' }
#'
populationPDXdesignApp = function(){
  shinyApp(ui, server)
}

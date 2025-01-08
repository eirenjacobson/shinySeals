#libraries
library(shiny)

ui <- fluidPage(
 
  fluidRow(
           align="center",
           h1(strong("Harbour seal population simulation")),
           
           hr()),
           
  fluidRow(
    column(9,
           plotOutput("SealPopModel"),
           p(span("Lines",style="color:grey")," = Simulated total population trajectory;",strong ("Black diamonds")," = Simulated counts of seals from aerial surveys;",span("Red dots",style="color:red")," = Real count data from Orkney 1989 - 2019 (optional)" ),
           hr()),
           
    column(3,
           br(),

           radioButtons(inputId = "plotObs", h4(div("Plot real counts?"),style="color:red"),
                        choices = list("Yes"= TRUE,"No"= FALSE), selected = FALSE)
      
    )
    
    
  ),
  
  fluidRow(
    column(3,
           h4(div("Simulation settings",style = "color:blue")),
           numericInput(inputId="nyears",
                        label = "Number of years (default = 35)", 
                        min = 10, max = 50, value = 35),
           numericInput(inputId="seals",
                        label = "Number of seals (default = 4900)", 
                        min = 100, max = 5000, value = 4900),
           numericInput(inputId="nreps",
                        label = "Number of simulations (default = 100)", 
                        min = 10, max = 900, value = 100)
           
    ),
    column(3, 
           h4(div("Demographic parameter values",style = "color:blue")),
           sliderInput(inputId = "f", 
                       label = "1. FECUNDITY RATE (default 0.88)", 
                       min=0, max=1, value= c(0.88)),
           sliderInput(inputId = "S0", 
                       label = "2. PUP SURVIVAL (default 0.35)", 
                       min=0, max=1, value= c(0.35))
           
    ),
    
    column(3,
           br(),
           br(),
           sliderInput(inputId = "S1", 
                       label = "3. JUVENILE SURVIVAL (default 0.8) ", 
                       min=0, max=1, value= c(0.8)),
           sliderInput(inputId = "S2F", 
                       label = "4. ADULT FEMALE SURVIVAL (default 0.94)", 
                       min=0, max=1, value= c(0.94))
           ),
    
    column(3,
           radioButtons(inputId = "changetype",h4(div("Change by proportion or give exact value", style = "color:blue")), 
                        choices = list("None" = "none", "Proportion" = "percentage", "Exact" = "exact")),
           radioButtons(inputId = "changepar",h4(div("Which parameter(s) to change",style = "color:blue")),
                       choices = list("None" = "none","Fecundity"= "f","Pup survival"="S0","Juvenile survival"="S1","Adult female survival"="S2F", "Adult male survival" = "S2M")),
           sliderInput(inputId = "S0_change", 
                       label = "Change in pup survival",  
                       min=0, max=1, value= c(1)),
           sliderInput(inputId = "S1_change", 
                       label = "Change in juvenile survival",  
                       min=0, max=1, value= c(1)),
           sliderInput(inputId = "S2M_change", 
                       label = "Change in adult male survival",  
                       min=0, max=1, value= c(1)),
           sliderInput(inputId = "S2F_change", 
                       label = "Change in adult female survival",  
                       min=0, max=1, value= c(1)),
           sliderInput(inputId = "f_change", 
                       label = "Change in fecundity",  
                       min=0, max=1, value= c(1))
    ) # end column 4
  )
)


# server.R ----
server <- function(input, output) {
  
  source("simSealPop.R")
  
  output$SealPopModel <- renderPlot({
    out1 <- simSeals(nyears=input$nyears, seals=input$seals, 
                     f=input$f, S0=input$S0, S1=input$S1, S2F=input$S2F,
                     nreps=input$nreps,changepar=input$changepar, changeyear=11, changetype = input$changetype, 
                     changevec = c(input$S0_change, input$S1_change, input$S2M_change, input$S2F_change, input$f_change))
    source("plotSimSealPop.R")    
    plotSim(out=out1, nreps=input$nreps, plotObs=input$plotObs)
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)


library(shiny)
library(hyperSpec)
library(baseline)
#install.packages("birk")
library(birk)
library(reshape2)
library(ggplot2)

# Define UI ----
ui <- fluidPage(
  titlePanel("Backsheet FTIR Spectrum Analysis"),

  sidebarLayout(
   sidebarPanel(
           fileInput("file", label = ("Input FTIR raw file"), multiple = TRUE),
              
           ##read in FTIR spc file
           sliderInput("slider", label = "Baseline Correction Degree",
                       min = 0, max = 20, value = 17),
   ##baseline correction: method = medpolyfit
   
   checkboxInput("checkbox", label = "Default Normalization",
                 value = TRUE),
   ##normalization: for each material type, there is one particular peak used as
   ##normalization peak:
   ##PA: 1454 cm-1 (C-H2 scissor)
   ##PET: 1410 cm-1 (C-H)
   ##PVF: 1089 cm-1(C-F)
   ##PVDF: 1184 cm-1 (CF2)
   
   numericInput("num", label = "Custom normalziation peak cm^(-1)",
                value = 1000),
   
   checkboxGroupInput("var",
                      label = "Material Type",
                      choices = c("Polyamide" ,
                                  "Poly(ethylene terephthlatate)",
                                  "Polyvinyl fluoride",
                                  "Polyvinylidene fluoride"),
                      selected = "Polyamide")),
   ##select material type for default mormalization

          
  mainPanel(
    plotOutput("plotftir")
    #tableOutput("peakratio")
    #textOutput("selected_var")
  )
 )
)


# Define server logic ----
server <- function(input, output){

  output$plotftir <- renderPlot({
    req(input$file)
    
    if (length(input$file$datapath) < 2) {
    df <- read.spc(input$file$datapath[1])
    }
    else {df <- read.spc(input$file$datapath[1])
      for (i in 2:length(input$file$datapath)) {
      curfile <- read.spc(input$file$datapath[i])
      df <- rbind(df,curfile)
    }
    }
    
    #plotspc(df)

    for (i in 1:length(input$file[,1])) {
      bl <- baseline(df[i][[]], method = "modpolyfit", degree = input$slider)
      df[i][[]] <- getCorrected(bl) 
      
    } 
    
    #plotspc(df)
    
    dff <- as.t.df(df)
    
    if (input$checkbox) {
      if (input$var == "Polyamide") {
        de = which.closest(dff[,1], 1454)
      }else if (input$var  == "Poly(ethylene terephthlatate)") {
        de = which.closest(dff[,1], 1410)
      }else if (input$var  == "Polyvinyl fluoride") {
        de = which.closest(dff[,1], 1089)
      }else if (input$var  == "Polyvinylidene fluoride") {
        de = which.closest(dff[,1], 1184)
      }
    }
    
    localMaxima <- function(x) { ##find local maximum to find the peak near the
      ##normalization peak
      # Use -Inf instead if x is numeric (non-integer)
      y <- diff(c(-.Machine$integer.max, x)) > 0L
      rle(y)$lengths
      y <- cumsum(rle(y)$lengths)
      y <- y[seq.int(1L, length(y), 2L)]
      if (x[[1]] == x[[2]]) {
        y <- y[-1]
      }
      y
    }
    
    pe = localMaxima(dff[,2][(de - 10):(de + 10)])
    
    for (i in 2:ncol(dff)) {
      dff[,i] = dff[,i]/dff[,i][de + pe - 11]
    }
    
    fdf = melt(dff, id = colnames(dff)[1])
    
    ggplot(data = fdf, aes( x = fdf[,1],  y = value, color = variable)) +
      geom_line() + xlab("Wavenumber (cm-1)") + ylab("Intensity")
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

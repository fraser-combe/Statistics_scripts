library(shiny)
library(ape)
library(tidyverse)
library(ggtree)
library(phylocanvas)
library(plotly)
# Define server logic required to draw a histogram
  server<-shinyServer(function(input, output) {
    nwkfile<-read.tree(file="IBVtreenewick.newick")
    treeFile<- makeNodeLabel(nwkfile)
    treeFile<- as(treeFile,"phylo4")
    phycanv <- phylocanvas(treeFile,treetype = "rectangular", alignlabels = T)
    
    # output$pTree <- renderPlot({ggtree(nwkfile)+ geom_tiplab() + xlim(0,5) + geom_point()})
    
    output$phyloTree <- renderPhylocanvas({#env = parent.frame(1), quoted = T
      phycanv
    })
  
    
    # output$click_info <- renderPrint({
    #     nearPoints(ggtree(nwkfile), input$pTree_click, addDist = TRUE)
    # })
    # download
    output$ExportPlot <- downloadHandler(
      # file name
      filename <- 'treeoutput.png',
      # content
      content = function(file){
        # create plot
        export(p = thePlot(), file = 'tempPlot.png')
        # hand over the file
        file.copy('tempPlot.png',file)
    
  })

    
ui<-shinyUI(fluidPage(
  plotlyOutput("plot"),
  # Application title
  headerPanel("FastPAS tree visualization tool"),
  
  # Show a plot of the generated distribution
  mainPanel(
    # plotOutput("pTree"),
    phylocanvasOutput("phyloTree"),
    downloadButton('ExportPlot', 'Export as png')
  )
  
)
)

shinyApp(ui = ui, server = server)

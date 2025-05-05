#==============================================================================
# This is the ui code to display tree app
#
# Fraser Combe V1
#==============================================================================

shinyUI(
  tagList(
    useShinyjs(),
    navbarPage(
      title = span("Fastpas Phylogenetic Tree and Annotations", style = "background-color: #DEEBF7; color: black"),
      tabPanel(
        title = "Explore phylogenetic Tree",
        fluidRow(
          class = "inputs",
          column(
            6, 
            selectInput(
              inputId = "file_type",
              label = "Select Tree File Type:",
              choices = c(
                "Newick" = "tree",  
                #"Beast" = "beast",  
                #"MrBayes" = "mrbayes", 
                "phylip" = "phylip", 
                "RAxML" = "raxml"
              ),
              selected = "tree"
            )
          ),
          column(
            6,
            fileInput(
              inputId = "upload_tree",
              label = "Select Tree File:"
            )
          )
        ),
        uiOutput("select_node_render"),
        fluidRow(
          uiOutput(
            "subtree_render"
          )
        )
      )
    ) 
  )
)

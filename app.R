
#######
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(parcats)
library(easyalluvial)

## EID2 reduced to Lao bush market and CITES Thailand
eid <- read.csv("./data/eid_merge.csv") |>
  dplyr::mutate('Parasite group' = Parasite,
                'Parasite species' = Cargo,
                'Host species' = Carrier,
                'Host group' = Group)


# ui object
ui <- fluidPage(
  theme = shinytheme("flatly"),
  # tags$head(includeScript("google-analytics.js")),
  tags$title("Parasites shared among hosts"),
  imageOutput("home_img", width = "100%",height = "auto"),
   column(width = 12,
         h1("Risk of parasite transmission associated wih wildlife trade (prototype app)"),
         HTML(
           "<div class='alert alert-info'>",
           "Parasites and microbes shared using EID2 database",
           "</div>"
         )
  ),
     sidebarPanel(
      # Input: Choose dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("cites_Thailand", "bushmarket_Laos"), multiple=F),
      br(),
      selectInput(inputId="host", 
                  label ="Choose Host Group", 
                  choices=unique(eid$Host),
                  selected = dplyr::filter(eid, Host == "Aves")),
      selectInput("species",  label ="Choose Species", 
                  # choices=unique(eid$Species),
                  choices =NULL,
                  selected = dplyr::filter(eid, Species == "Accipiter cooperii")),
      # actionButton("update", "Update range"),
      br(),
      #imageOutput("photo"),
      hr(),
      h4("Project"),
      helpText(HTML("Guidelines health risk wildlife trade")),
      hr(),
      h4("Data"),
      helpText(HTML("Wardeh et al, 2021
                    Scientific Data
                    DOI:10.1038/sdata.2015.49")),
      hr(),
      h4("Conception"),
      helpText(HTML("HealthDEEP")),
      br(), 
      width=2
    )
    ,
    mainPanel(
      h3(),
      hr(),
       #hr(),
      fluidRow(
        column(width = 10,
               h2(em(textOutput("text1"))),
               parcatsOutput("alluvial_plot", width = "1000", height = "1500", inline = FALSE)
               )
        )
        )
    )



server = function(input, output,session) {
  
  # Update subcategory choices based on selected category
  
  observeEvent(input$dataset, {
    host_sel <- unique(eid$Host[eid$dataSet == input$dataset])
    species_sel <- unique(eid$Species[eid$dataSet == input$dataset])
    updateSelectInput(session, "host", choices = host_sel)
    updateSelectInput(session, "species", choices = species_sel)
  },ignoreNULL = FALSE)
  
  observeEvent(input$host, {
    species_sel <- unique(eid$Species[eid$Host == input$host])
    updateSelectInput(session, "species", choices = species_sel)
  },ignoreNULL = FALSE)
  
  
  # species selected 
  output$text1 <- renderText({ 
    file <- input$species
    if (is.null(file)) {
      return("No species selected.")
    }
    file
  })
  
  output$alluvial_plot <- render_parcats({
    
    eid_chosen <- eid |>
      dplyr::filter(Host == input$host)|>
      dplyr::filter(Species == input$species) |>
      dplyr::select('Parasite group','Parasite species', 'Host species', 'Host group')
    
    if (nrow(eid_chosen) == 0) {
      return(NULL)  # Or return a message like "No data available for this selection."
    }
    
    p <- alluvial_wide(eid_chosen, max_variables = 4,
                       fill_by = 'first_variable',
                       stratum_label_size = 0.5)
    parcats(p, marginal_histograms = TRUE, data_input = eid_chosen)
    
   })
  
  
  # image logo selected  
  output$home_img <- renderImage({
    
    list(src = "./data/HD5.jpg",
         width = "30%",
         height = "auto")
    
  }, deleteFile = F)
  
}


# Run the application 
shinyApp(ui = ui, server = server)

#' Shiny interface module for geneset selection upd
#' 
#' @param id A unique namespace identifier
#' @return Shiny ui elements
#'
#' @importFrom shiny NS tagList uiOutput selectizeInput actionButton
#' 
#' @export
genesets_UI <- function(id) {
    genesets <- readRDS(file.path(system.file("extdata", package="hypeR"), "genesets.rds"))
    
    ns <- NS(id)
    tagList(
        selectizeInput(ns("db"), label="Database", choices=names(genesets)),
        uiOutput(ns("species")),
        uiOutput(ns("genesets")),
        actionButton(ns("fetch"), label="Fetch Genesets"),
        uiOutput(ns("status"), inline=TRUE)
    )
}

#' Shiny server module for geneset selection
#' 
#' @param id A unique namespace identifier matching to interface
#' @param clean Use TRUE to clean geneset names
#' @return A reactive genesets list
#'
#' @importFrom shiny moduleServer renderUI validate need icon selectizeInput eventReactive
#' 
#' @export
genesets_Server <- function(id, clean = FALSE) {
  # Load the genesets metadata (pre-generated list of options)
  genesets <- readRDS(file.path(system.file("extdata", package = "hypeR"), "genesets.rds"))
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # Build species selection dropdown
      output$species <- renderUI({
        ns <- session$ns
        validate(need(input$db, message = FALSE))
        choices <- names(genesets[[input$db]])
        selectizeInput(ns("species"), label = "Species", choices = choices)
      })
      
      # Build genesets selection dropdown
      output$genesets <- renderUI({
        ns <- session$ns
        validate(need(input$db, message = FALSE))
        validate(need(input$species, message = FALSE))
        choices <- names(genesets[[input$db]][[input$species]])
        selectizeInput(ns("genesets"), label = "Genesets", choices = choices)
      })
      
      # Download selected geneset and put into a reactive expression
      reactive.genesets <- eventReactive(input$fetch, {
        validate(need(input$db, message = FALSE))
        validate(need(input$species, message = FALSE))
        validate(need(input$genesets, message = FALSE))
        
        gs <- NULL  # default value
        
        if (input$db == "msigdb") {
          metadata <- genesets[[input$db]][[input$species]][[input$genesets]]
          
          # Correct argument names for msigdb_download
          collection <- metadata$collection %||% metadata$category
          subcollection <- metadata$subcollection %||% metadata$subcategory
          if (is.character(subcollection) && nchar(subcollection) == 0) {
            subcollection <- NULL
          }
          
          species <- input$species
          
          # Correct usage
          gs <- msigdb_download(
            species = species,
            collection = collection,
            subcollection = subcollection
          )
          
        } else if (input$db == "enrichr") {
          metadata <- genesets[[input$db]][[input$species]][[input$genesets]]
          gs <- do.call(enrichr_download, metadata)
        }
        
        # Optional cleaning
        if (!is.null(gs) && clean) {
          names(gs) <- clean_genesets(names(gs))
        }
        
        return(gs)
      })
      
      # Check status of loaded genesets and show an icon
      output$status <- renderUI({
        if (is.null(reactive.genesets())) {
          icon("times-circle", lib = "font-awesome", class = "fa-lg", style = "color: red;")
        } else {
          icon("check-circle", lib = "font-awesome", class = "fa-lg", style = "color: green;")
        }
      })
      
      return(reactive.genesets)
    }
  )
}

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
#' @param clean Use true to clean geneset names
#' @return Shiny server code
#'
#' @importFrom shiny moduleServer renderUI validate need icon selectizeInput eventReactive
#' 
#' @export
genesets_Server <- function(id, clean=FALSE) {
    genesets <- readRDS(file.path(system.file("extdata", package="hypeR"), "genesets.rds"))
    
    moduleServer(
        id,
        function(input, output, session) {
            
            # Build species selection
            output$species <- renderUI({
                ns <- session$ns
                validate(need(input$db, message=FALSE))
                choices <- names(genesets[[input$db]])
                selectizeInput(ns("species"), label="Species", choices=choices)
            })
            
            # Build genesets selection
            output$genesets <- renderUI({
                ns <- session$ns
                validate(need(input$db, message=FALSE))
                validate(need(input$species, message=FALSE))
                choices <- names(genesets[[input$db]][[input$species]])
                selectizeInput(ns("genesets"), label="Genesets", choices=choices)
            })
            
            # Download selected geneset and put into a reactive expression
            reactive.genesets <- eventReactive(input$fetch, {
                validate(need(input$db, message=FALSE))
                validate(need(input$species, message=FALSE))
                validate(need(input$genesets, message=FALSE))
                if (input$db == "msigdb") {
                  metadata <- genesets[[input$db]][[input$species]][[input$genesets]]
                  
                  # Assuming metadata includes: category, subcategory (optional), species, version
                  category <- metadata$category
                  subcategory <- metadata$subcategory
                  species <- input$species
                  version <- metadata$version %||% "latest"  # use rlang::`%||%` or define fallback
                  
                  # msigdb_download likely now needs at least: category, subcategory, species
                  gs <- msigdb_download(
                    category = category,
                    subcategory = subcategory,
                    species = species,
                    version = version
                  )
                }   
                else if (input$db == "enrichr") {
                    kwargs <- genesets[[input$db]][[input$species]][[input$genesets]]
                    gs <- do.call(enrichr_download, kwargs)
                }
                else {
                    gs <- NULL
                }
                if (!is.null(gs) & clean) {
                    names(gs) <- clean_genesets(names(gs))
                }
                return(gs)
            })
            
            # Check status of loaded genesets
            output$status <- renderUI({
              if (is.null(reactive.genesets())) {
                icon("times-circle", lib = "font-awesome", class = "fa-lg")
              } else {
                icon("check-circle", lib = "font-awesome", class = "fa-lg")
              }
            })
            
            
            return(reactive.genesets)
        }
    )
}

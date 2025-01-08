#' km UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_km_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' km Server Functions
#'
#' @noRd 
mod_km_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_km_ui("km_1")
    
## To be copied in the server
# mod_km_server("km_1")

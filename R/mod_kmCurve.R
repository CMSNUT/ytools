#' kmCurve UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_kmCurve_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' kmCurve Server Functions
#'
#' @noRd 
mod_kmCurve_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_kmCurve_ui("kmCurve_1")
    
## To be copied in the server
# mod_kmCurve_server("kmCurve_1")

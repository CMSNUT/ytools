#' dataProcess UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dataProcess_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' dataProcess Server Functions
#'
#' @noRd 
mod_dataProcess_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_dataProcess_ui("dataProcess_1")
    
## To be copied in the server
# mod_dataProcess_server("dataProcess_1")

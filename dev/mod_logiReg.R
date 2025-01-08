#' logiReg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_logiReg_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' logiReg Server Functions
#'
#' @noRd 
mod_logiReg_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_logiReg_ui("logiReg_1")
    
## To be copied in the server
# mod_logiReg_server("logiReg_1")

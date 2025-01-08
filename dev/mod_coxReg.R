#' coxReg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_coxReg_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' coxReg Server Functions
#'
#' @noRd 
mod_coxReg_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_coxReg_ui("coxReg_1")
    
## To be copied in the server
# mod_coxReg_server("coxReg_1")

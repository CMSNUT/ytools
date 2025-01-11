#' regModel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regModel_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' regModel Server Functions
#'
#' @noRd
mod_regModel_server <- function(id,df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_regModel_ui("regModel_1")

## To be copied in the server
# mod_regModel_server("regModel_1")

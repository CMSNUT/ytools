#' dataProcess UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard box
#'
mod_dataProcess_ui <- function(id,data) {
  ns <- NS(id)
  tagList(
    # 设置 ----
    column(
      width = 3,
      h4(strong("KM曲线变量设置")),
      uiOutput(
        outputId = ns("kmVars")
      ),
      box(
        width = 12,
        title = "",
        status = "primary",

      )
    ),

    # 绘图 ----
    column(
      width = 9,
    )
  )
}

#' dataProcess Server Functions
#'
#' @noRd
mod_dataProcess_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    dataFile <- paste0("./temp/",id,"_data_import.RData")
    if (file.exists(dataFile)) {
      load(dataFile)
    } else {
      data <- NULL
    }




  })
}

## To be copied in the UI
# mod_dataProcess_ui("dataProcess_1")

## To be copied in the server
# mod_dataProcess_server("dataProcess_1")

#' survAnal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_survAnal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(
      strong("生存分析",style = "color:blue;font-size:28px"),
      ## 生成曲线数据导入 ----
      tabPanel(
        title = "数据导入",
        mod_dataImport_ui(id)
      ),
      ## 生存曲线数据处理 ----
      tabPanel(
        title = "数据处理",
        mod_dataProcess_ui(id)
      ),
      ## 生存曲线绘制 ----
      tabPanel(
        title = "KM曲线",
        mod_km_ui(id)
      )
    )
  )
}

#' survAnal Server Functions
#'
#' @noRd
mod_survAnal_server <- function(id,df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_survAnal_ui("survAnal_1")

## To be copied in the server
# mod_survAnal_server("survAnal_1")

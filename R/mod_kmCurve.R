#' kmCurve UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList renderUI uiOutput insertUI
#' @importFrom purrr is_empty
#' @importFrom magrittr %>%
#' @importFrom DT DTOutput renderDT
#' @importFrom shinyWidgets pickerInput
#' @importFrom stringr str_which
#' @import survival

mod_kmCurve_ui <- function(id) {
  ns <- NS(id)
  tagList(

    column(
      width = 3,
      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = strong("KM曲线变量设置",style="font-size:20px;"),


      ),

      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        title = strong("KM曲线图设置",style="font-size:20px;"),
      ),

      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        title = strong("坐标轴和标签设置",style="font-size:20px;"),
      ),

      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        title = strong("风险表设置",style="font-size:20px;"),
      ),

      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        title = strong("KM曲线图下载设置",style="font-size:20px;"),
      )

    ),
    column(
      width = 9,
      box(
        width = 12,
        solidHeader = TRUE,
        status = "success",
        title = strong("KM曲线图",style="font-size:20px;"),

        plotOutput(ns("plot"))
      ),
      box(
        width = 3,
        solidHeader = TRUE,
        status = "warning",
        title = strong("中位生存时间计算方法",style="font-size:20px;"),

        plotOutput(ns("plot"))
      ),

      box(
        width = 9,
        solidHeader = TRUE,
        status = "warning",
        title = strong("中位生存时间表",style="font-size:20px;"),

        plotOutput(ns("plot"))
      )
    )

  )
}

#' kmCurve Server Functions
#'
#' @noRd
mod_kmCurve_server <- function(id,df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns



  })
}

## To be copied in the UI
# mod_kmCurve_ui("kmCurve_1")

## To be copied in the server
# mod_kmCurve_server("kmCurve_1")

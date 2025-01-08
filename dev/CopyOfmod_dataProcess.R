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
#' @importFrom DT DTOutput
#'
mod_dataProcess_ui <- function(id) {
  ns <- NS(id)
  tagList(
    #  设置 ----
    box(
      width = 6,
      title = strong("数据设置", style = "font-size:20px"),
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,

      tabsetPanel(
        id = "tabs",
        tabPanel(title = "分类变量设置"),
        tabPanel(title = "定量变量转为分类变量"),
        tabPanel(title = "衍生新变量"),
        tabPanel(title = "分类变量设置"),
        tabPanel(title = "分析集设置")
      ),
      tags$hr(),
      radioButtons(
        ns("processDataFormat"),
        "选择数据保存格式",
        c("xlsx", "csv", "RData"),
        inline = TRUE
      ),
      actionButton(ns("processDataSave"), "保存数据", icon = icon("save"))
    ),

    box(
      width = 6,
      title = strong("数据信息", style = "font-size:20px"),
      status = "success",
      solidHeader = TRUE,
      collapsible = FALSE,

      DTOutput(ns("processDataTab")),
      tags$hr(),
      verbatimTextOutput(ns("processDataSummary"))
    )
  )
}

#' dataProcess Server Functions
#'
#' @noRd
mod_dataProcess_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- reactiveVal()

    dataFile <- paste0("./temp/", id, "_data_import.RData")

    if (file.exists(dataFile)) {
      df(load(dataFile))
    } else {
      data <- data.frame()
      df("data")
    }



    # 数据表展示 ----
    output$processDataTab = renderDT(
      # iris, options = list(lengthChange = FALSE)
      get(df()),
      rownames = FALSE,
      escape = FALSE,
      options = list(
        lengthChange = FALSE,
        dom = 'tipr',
        scrollX = TRUE
      )
    )

    # 数据集变量概览 ----
    output$processDataSummary <- renderPrint({
      if (!is_empty(get(df()))) {
        str(get(df()))
      }
    })
  })
}

## To be copied in the UI
# mod_dataProcess_ui("dataProcess_1")

## To be copied in the server
# mod_dataProcess_server("dataProcess_1")

#' dataImport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList column
#' @importFrom shinydashboard box
mod_dataImport_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = strong("导入数据", style = "font-size:20px"),
      status = "primary",
      solidHeader = TRUE,
      width = 3,
      collapsible = FALSE,

      div(
        style = "inline-block",
        actionButton(
          inputId = ns("dataImportModal"),
          label = "打开本地数据",
          class = "btn-success",
          icon = icon("arrow-up-from-bracket")
        ),
        actionButton(
          inputId = ns("dataFormatModal"),
          label = "数据格式要求",
          class = "btn-danger"
        )
      ),

      tags$br(),
      p("导入数据前，请仔细阅读数据格式要求",style="color:red;"),

      tags$hr(),
      selectInput(
        inputId = ns("dataExample"),
        label = strong("选择示例数据", style = "color:blue;font-size:20px"),
        choices = c("lung", "pbc"),
        selected = 0,
        selectize = FALSE,
        size = 5
      )
    ),

    box(
      title = strong("数据信息", style = "font-size:20px"),
      status = "success",
      solidHeader = TRUE,
      width = 9,
      collapsible = FALSE,
      DT::DTOutput(outputId = ns("dataTab")),
      tags$hr(),
      strong("数据概览", style = "color:blue;font-size:16px"),
      verbatimTextOutput(ns("dataSummary"))
    )
  )
}

#' dataImport Server Functions
#'
#' @noRd
mod_dataImport_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # 定义 df ----
    df <- reactiveVal()

    # 数据格式要求对话框 ----
    observeEvent(input$dataFormatModal, {
      showModal(
        modalDialog(
          title = h4(strong("数据格式要求")),
          easy_close = TRUE,
          footer = tagList(modalButton("确定")),
          p("1.数据导入格式为csv、xlsx、xls格式，数据集大小不超过10M"),
          p("2.首行是变量名，不要有 '.、/、*、% 、$'等符号"),
          p("3.从第二行开始，每一行都代表着一个研究对象的所有变量信息"),
          p("4.缺失值默认处理，一般是空白或NA")
        )
      )
    })

    # 本地数据导入对话框 ----
    observeEvent(input$dataImportModal, {
      showModal(
        modalDialog(
          title = strong("数据导入",style="color:blue;font-size:28px"),
          easy_close = TRUE,
          size = "l",
          footer = tagList(
            actionButton(
              inputId = ns("cancelImportBtn"),
              label = "取消导入数据",
              class = "btn-danger",
              width = "30%"
            ),
            actionButton(
              inputId = ns("dataImportBtn"),
              label = "确定导入数据",
              class = "btn-success",
              width = "30%"
            )
          ),

          box(
            title = strong("选择数据文件", style = "font-size:20px"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,

            fileInput(
              inputId = ns("dataFile"),
              label = "",
              accept = c(".csv", ".xlsx", ".xls"),
              width = "100%",
              buttonLabel = "浏览..."
            ),

            checkboxInput(
              inputId = ns("hasTitle"),
              label = "数据包含标题",
              value = TRUE,
              width = "100%"
            ),

            checkboxInput(
              inputId = ns("chrAsFactor"),
              label = "字符串变量转为因子变量",
              value = TRUE,
              width = "100%"
            ),

            numericInput(
              inputId = ns("titleRowNum"),
              label = "去掉前N行数据",
              value = 0,
              min = 0,
              step = 1,
              width = "100%"
            ),


            textInput(
              inputId = ns("missValue"),
              label = "缺失值字符",
              value = ",NA",
              width = "100%"
            ),

            "若数据有多种缺失值符号(如NA,na,NAN,nan等)，用逗号(',')分隔,如(,NA,nan)",

          ),
          DT::DTOutput(ns("dataTabPreview"))
        )
      )
    })

    # 返回 df ----
    return(df)

  })
}

## To be copied in the UI
# mod_dataImport_ui("dataImport_1")

## To be copied in the server
# mod_dataImport_server("dataImport_1")

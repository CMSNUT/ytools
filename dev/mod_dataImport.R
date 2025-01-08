#' dataImport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList verbatimTextOutput
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput renderDT
#' @importFrom readxl read_xlsx read_xls
#'
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
          label = "数据集格式介绍",
          class = "btn-warning"
        )
      ),

      tags$br(),
      p("导入数据前，请仔细阅读数据集格式的介绍",style="color:red;"),

      tags$hr(),
      selectInput(
        inputId = ns("dataExample"),
        label = strong("选择示例数据", style = "color:blue;font-size:20px"),
        choices = c("lung", "pbc"),
        selected = 0,
        selectize = FALSE,
        size = 5
      ),
    ),

    box(
      title = strong("数据表", style = "font-size:20px"),
      status = "success",
      solidHeader = TRUE,
      width = 9,
      collapsible = FALSE,

      DT::DTOutput(outputId = ns("dataTab")),

      strong("数据概览", style = "font-size:20px"),
      verbatimTextOutput(ns("dataSummary"))
    )
  )
}

#' dataImport Server Functions
#'
#' @noRd
mod_dataImport_server <- function(id,df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    df1 <- reactiveVal()

    # 数据集格式介绍对话框 ----
    observeEvent(input$dataFormatModal, {
      showModal(
        modalDialog(
          title = h4(strong("数据集格式介绍")),
          easy_close = TRUE,
          footer = tagList(modalButton("确定")),
          p("1.数据导入格式为csv、excel格式，数据集大小不超过10M"),
          p("2.首行是变量名，一般是英文或者拼音，不建议用汉字，更不要有 '.、/、*、% '等符号。"),
          p("3.从第二行开始，每一行都代表着一个研究对象的所有变量信息。"),
          p("4.每一列都代表所有人一个变量的变量值。"),
          p(
            "5.建议，CSV和excel中的变量值，建议用数字表示(非必需)，比如男性=1，女性=2；血型（1、2、3、4）"
          ),
          p("6.缺失值默认处理，一般是空白代替，不要写NA"),
          p("7.除非特殊字符串（比如姓名），数据库变量值不要出现汉字或者英文字符串，会导致后续分析被卡。")
        )
      )
    })


    # 数据表展示 ----
    output$table = renderDT(# iris, options = list(lengthChange = FALSE)
      df(),
      rownames = FALSE,
      escape = FALSE,
      options = list(
        lengthChange = FALSE,
        dom = 'tipr',
        scrollX = TRUE
      )
    )

    # 数据变量概览 ----
    output$dataSummary <- renderPrint({
      req(df())
      str(df())
    })

  })
}

## To be copied in the UI
# mod_dataImport_ui("dataImport_1")

## To be copied in the server
# mod_dataImport_server("dataImport_1")

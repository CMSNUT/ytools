#' dataImport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput renderDT
mod_dataImport_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 3,
      box(
        width = 12,
        fileInput(
          inputId = ns("dataSelect"),
          label = "选择数据",
          accept = c(".csv",".xlsx",".xls"),
          width = "100%",
          buttonLabel = "浏览..."
        ),

        actionButton(
          inputId = ns("dataImport"),
          label = "导入数据",
          class = "btn-success"
        ),

        h4(strong("特别提醒：")),
        p("1.数据集变量名不要含有空格或符号如括号、※等"),
        p("2.支持10 M 以内的的各种格式数据集"),
        p("3.数据导入前，请仔细阅读数据集格式的介绍"),
        actionButton(
          inputId = ns("dataFormatModal"),
          label = "数据集格式介绍"
        )
      ),

      box(
        width = 12,
        h4(strong("示例数据")),

        selectInput(
          inputId = ns("dataExample"),
          label = "",
          choices = c("lung","pbc"),
          selected = 0,
          selectize = FALSE,
          size = 5
        ),
      ),

      box(
        width = 12,
        h3(strong("数据集变量概览")),
        # input_task_button()
        verbatimTextOutput(ns("dataSummary"))
      )
    ),
    column(
      width = 9,
      h3(strong("导入的数据")),
      DT::DTOutput(
        outputId = ns("dataTab")
      )
    )
  )
}

#' dataImport Server Functions
#'
#' @noRd
mod_dataImport_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    df <- reactiveVal()

    observeEvent(input$dataImport,{
      file <- input$dataSelect
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext %in% c("csv","xlsx","xls"), "请导入csv 或 xlsx 或 xls格式文件"))
      if (ext=="csv") {
        data <- read.csv(file$datapath)
      } else {
        data <- read_xlsx(file$datapath)
      }
      df(data)
    })

    observeEvent(input$dataExample, {
      data <- get(input$dataExample)
      df(data)
    })



    # # 数据导入对话框 ----
    # observe({
    #   showModal(
    #     modalDialog(
    #       title = "Somewhat important message",
    #       easy_close = TRUE,
    #       # "This is your important message."
    #
    #     )
    #   )
    # }) |>
    #   bindEvent(input$dataImportModal)

    # 数据集格式介绍对话框 ----
    observe({
      showModal(
        modalDialog(
          title = h4(strong("数据集格式介绍")),
          easy_close = TRUE,
          p("1.数据导入格式为csv、excel格式，数据集大小不超过10M"),
          p("2.首行是变量名，一般是英文或者拼音，不建议用汉字，更不要有 '.、/、*、% '等符号。"),
          p("3.从第二行开始，每一行都代表着一个研究对象的所有变量信息。"),
          p("4.每一列都代表所有人一个变量的变量值。"),
          p("5.建议，CSV和excel中的变量值，建议用数字表示(非必需)，比如男性=1，女性=2；血型（1、2、3、4）"),
          p("6.缺失值默认处理，一般是空白代替，不要写NA"),
          p("7.除非特殊字符串（比如姓名），数据库变量值不要出现汉字或者英文字符串，会导致后续分析被卡。")
        )
      )
    }) |>
      bindEvent(input$dataFormatModal)

    # 数据集变量概览 ----
    output$summary <- renderPrint({
      summary(sample())
    })

    # 数据表展示 ----
    output$dataTab = renderDT(
      # iris, options = list(lengthChange = FALSE)
      df()
    )
  })
}

## To be copied in the UI
# mod_dataImport_ui("dataImport_1")

## To be copied in the server
# mod_dataImport_server("dataImport_1")

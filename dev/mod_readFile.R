#' readFile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard box
#' @importFrom readxl read_xlsx read_xls
mod_readFile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("导入数据前，请仔细阅读数据格式的介绍",style="color:red;"),
    actionButton(
      inputId = ns("dataFormatModal"),
      label = "数据格式介绍",
      class = "btn-warning"
    ),
    tags$hr(),

    strong("数据导入设置",style="color:blue;font-size:20px;"),
    checkboxInput(
      inputId = ns("hasTitle"),
      label = "数据表包含标题",
      value = TRUE,
      width = "100%"
    ),
    textInput(
      inputId = ns("missValue"),
      label = "缺失值字符",
      value = ",NA",
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
    "若数据有多种缺失值符号(如NA,na,NAN,nan等)，用逗号(',')分隔,如(,NA,nan)",

    tags$hr(),

    fileInput(
      ns("dataFile"),
      strong("导入本地数据",style="color:blue;font-size:20px;"),
      buttonLabel = "浏览...",
      placeholder = "选择CSV、xlsx、xls文件"
    ),



    tags$hr(),
    selectInput(
      inputId = ns("dataExample"),
      label = strong("选择示例数据", style = "color:blue;font-size:20px"),
      choices = c("lung", "pbc"),
      selected = 0,
      selectize = FALSE,
      size = 5
    )
  )
}

#' readFile Server Functions
#'
#' @noRd
mod_readFile_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

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

    df1 <- reactiveVal()

    # 导入本地数据 ----
    df <- reactive({
      file <- input$dataFile
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext %in% c("csv", "xlsx", "xls"), "请导入csv 或 xlsx 或 xls格式文件"))
      if (ext == "csv") {
        data <- read.csv(
          file$datapath,
          header = input$hasTitle,
          skip = input$titleRowNum,
          na.strings = strsplit(input$missValue, ",")[[1]],
        )
      } else if (ext == "xlsx") {
        data <- read_xlsx(
          file$datapath,
          col_names = input$hasTitle,
          na = strsplit(input$missValue, ",")[[1]],
          skip = input$titleRowNum,
        )
      } else {
        data <- read_xls(
          file$datapath,
          col_names = input$hasTitle,
          na = strsplit(input$missValue, ",")[[1]],
          skip = input$titleRowNum
        )
      }
    })

    observe({
      df1(df())
    })

    # 导入示例数据 ----
    observeEvent(input$dataExample, {
      expr <- parse(text = paste0('survival::', input$dataExample))
      data <- eval(expr)
      df1(data)
    })


    return(df1)
  })
}

## To be copied in the UI
# mod_readFile_ui("readFile_1")

## To be copied in the server
# mod_readFile_server("readFile_1")

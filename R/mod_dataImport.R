#' dataImport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput renderDT
#' @importFrom readxl read_xlsx read_xls
#' @import survival
#' @import stringr
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
          label = "打开我的数据",
          class = "btn-success",
          icon = icon("arrow-up-from-bracket")
        ),
        actionButton(
          inputId = ns("dataFormatModal"),
          label = "数据集格式介绍",
          class = "btn-danger"
        )
      ),

      h4(strong("注意事项：")),
      p("1.数据集变量名不要含有空格或符号如括号、*等"),
      p("2.支持10 M 以内的的各种格式数据集"),
      p("3.数据导入前，请仔细阅读数据集格式的介绍"),

      strong("示例数据", style = "color:blue;font-size:20px"),
      selectInput(
        inputId = ns("dataExample"),
        label = p("选择示例数据", style = "color:red"),
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
      width = 6,
      collapsible = FALSE,
      DT::DTOutput(outputId = ns("dataTab"))
    ),

    box(
      title = strong("数据集变量概览", style = "font-size:20px"),
      status = "primary",
      solidHeader = TRUE,
      width = 3,
      collapsible = FALSE,
      # input_task_button()
      verbatimTextOutput(ns("dataSummary"))
    )
  )
}

#' dataImport Server Functions
#'
#' @noRd
mod_dataImport_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df1 <- reactive({
      file <- input$dataSelect
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
          skip = input$titleRowNum,
          n_max = 6
        )
      }
      # df(data)
      #
      # if (!dir.exists("./temp")) {
      #   dir.create("./temp")
      # }
      # save(data, file = paste0("./temp/", id, "_data_import.RData"))
    })

    df2 <- reactiveVal()
    df <- reactiveVal()

    # observeEvent(input$dataImport, {
    #   file <- input$dataSelect
    #   ext <- tools::file_ext(file$datapath)
    #   req(file)
    #   validate(need(ext %in% c("csv", "xlsx", "xls"), "请导入csv 或 xlsx 或 xls格式文件"))
    #   if (ext == "csv") {
    #     data <- read.csv(file$datapath)
    #   } else if (ext == "xlsx") {
    #     data <- read_xlsx(file$datapath)
    #   } else {
    #     data <- read_xls(file$datapath)
    #   }
    #   df(data)
    #
    #   if (!dir.exists("./temp")) {
    #     dir.create("./temp")
    #   }
    #   save(data, file = paste0("./temp/", id, "_data_import.RData"))
    # })

    # 示例数据 ----
    observeEvent(input$dataExample, {
      expr <- parse(text = paste0('survival::', input$dataExample))
      data <- eval(expr)
      df(data)

      if (!dir.exists("./temp")) {
        dir.create("./temp")
      }
      save(data, file = paste0("./temp/", id, "_data_import.RData"))
    })


    # 数据导入对话框 ----
    observeEvent(input$dataImportModal, {
      showModal(modalDialog(
        # title = h4(strong("数据导入")),
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

        tabsetPanel(
          id = "dm_tabs",
          tabPanel(
            title = "加载数据",
            fileInput(
              inputId = ns("dataSelect"),
              label = strong("选择数据文件", style = "font-size:20px"),
              accept = c(".csv", ".xlsx", ".xls"),
              width = "100%",
              buttonLabel = "浏览..."
            ),

            box(
              title = strong("数据表格式设置", style = "font-size:20px"),
              status = "success",
              solidHeader = TRUE,
              width = 12,
              collapsible = FALSE,

              column(
                width = 6,
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
              ),
              column(
                width = 6,
                numericInput(
                  inputId = ns("titleRowNum"),
                  label = "标题所在行",
                  value = 0,
                  min = 0,
                  step = 1,
                  width = "100%"
                ),
                "若数据有多种缺失值符号(如NA,na,NAN,nan等)，用逗号(',')分隔,如(,NA,nan)"
              )
            ),

            DT::DTOutput(ns("dataTabPreview"))
          ),
          tabPanel(
            title = "查看数据",
            DT::DTOutput(ns("dataTabView"))
          ),
          tabPanel(
            title = "修改数据",
            DT::DTOutput(ns("dataTabModify")),
            actionButton(
              ns("dataModifyBtn"),
              "确定修改",
              width = "100%"
            )
          )
        )

      ))
    })

    observeEvent(input$cancelImportBtn, {
      removeModal()
    })

    observeEvent(input$dataImportBtn, {
      # Check that data object exists and is data frame.
      # if (!is.null(input$dataset) && nzchar(input$dataset) &&
      #     exists(input$dataset) && is.data.frame(get(input$dataset))) {
      #   vals$data <- get(input$dataset)
      #   removeModal()
      # } else {
      #   showModal(dataModal(failed = TRUE))
      # }
      removeModal()
    })

    # 数据集格式介绍对话框 ----
    observeEvent(input$dataFormatModal, {
      showModal(modalDialog(
        title = h4(strong("数据集格式介绍")),
        easy_close = TRUE,
        footer = tagList(
          modalButton("确定")
        ),
        p("1.数据导入格式为csv、excel格式，数据集大小不超过10M"),
        p("2.首行是变量名，一般是英文或者拼音，不建议用汉字，更不要有 '.、/、*、% '等符号。"),
        p("3.从第二行开始，每一行都代表着一个研究对象的所有变量信息。"),
        p("4.每一列都代表所有人一个变量的变量值。"),
        p(
          "5.建议，CSV和excel中的变量值，建议用数字表示(非必需)，比如男性=1，女性=2；血型（1、2、3、4）"
        ),
        p("6.缺失值默认处理，一般是空白代替，不要写NA"),
        p("7.除非特殊字符串（比如姓名），数据库变量值不要出现汉字或者英文字符串，会导致后续分析被卡。")
      ))
    })

    # 数据集变量概览 ----
    output$dataSummary <- renderPrint({
      req(df())
      str(df())
    })

    # 数据表预览 ----
    output$dataTabPreview = renderDT(
      head(df1()),
      rownames = FALSE,
      escape = FALSE,
      options = list(
        lengthChange = FALSE,
        dom = 't',
        scrollX = TRUE
      )
      # df1(), options = list(scrollX = TRUE)
    )

    # 数据表查看 ----
    output$dataTabView = renderDT(
      df1(),
      rownames = FALSE,
      escape = FALSE,
      options = list(
        lengthChange = FALSE,
        pageLength=20,
        dom = 'tipr',
        scrollX = TRUE,
        scrollY = TRUE,
        language = list(
          info = '显示第_START_ 至 _END_ 项结果，共 _TOTAL_ 项',
          search = '搜索:',
          paginate = list(previous = '上页', `next` = '下页'),
          lengthMenu = '显示 _MENU_ 项结果')
        )
      # df1(), options = list(scrollX = TRUE)
    )

    # 数据修改 ----
    output$dataTabModify = renderDT(
      data.frame(
        列名 = colnames(d),
        新列名 = rep("输入新列名", each = length(d)),

      ),
      rownames = FALSE,
      escape = FALSE,
      colnames = c("列名"),
      options = list(

        lengthChange = FALSE,
        pageLength=20,
        dom = 'tipr',
        scrollX = TRUE,
        scrollY = TRUE,
        language = list(
          info = '显示第_START_ 至 _END_ 项结果，共 _TOTAL_ 项',
          search = '搜索:',
          paginate = list(previous = '上页', `next` = '下页'),
          lengthMenu = '显示 _MENU_ 项结果')
      )
      # df1(), options = list(scrollX = TRUE)
    )

    # 数据表展示 ----
    # output$dataTab = renderDT(# iris, options = list(lengthChange = FALSE)
    #   df(), options = list(scrollX = TRUE)
    # )
  })
}

## To be copied in the UI
# mod_dataImport_ui("dataImport_1")

## To be copied in the server
# mod_dataImport_server("dataImport_1")

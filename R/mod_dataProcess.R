#' dataProcess UI Function
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
#'
mod_dataProcess_ui <- function(id) {
  ns <- NS(id)
  tagList(column(width = 4, uiOutput(ns("process_tabs"))), column(
    width = 8,
    box(
      title = strong("数据信息", style = "font-size:20px"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,

      DT::DTOutput(outputId = ns("processDataTab")),
    ),

    box(
      title = strong("数据概览", style = "font-size:20px"),
      status = "primary",
      solidHeader = TRUE,
      width = 6,
      collapsible = FALSE,
      verbatimTextOutput(ns("processDataSummary"))
    ),
    box(
      title = strong("变量概览", style = "font-size:20px"),
      status = "primary",
      solidHeader = TRUE,
      width = 6,
      collapsible = FALSE,
      DT::DTOutput(outputId = ns("processVarsTab")),
    )
  ))
}

#' dataProcess Server Functions
#'
#' @noRd
mod_dataProcess_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df1 <- reactiveVal()
    allVars <- reactiveVal()
    fvars <- reactiveVal()
    nfvars <- reactiveVal()

    df2 <- reactive(df())

    observe({
      df1(df2())
      # allVars(colnames(df2()))
      # fv <- sapply(df2(),is.factor) %>% as.data.frame
      # if (!is_empty(fv)) {
      #   fvars(colnames(df2())[fv[,1]])
      #   nfvars(setdiff(colnames(df2()),fvars()))
      # }
    })

    observe({
      dt <- df1()
      allVars(colnames(dt))
      fv <- sapply(dt, is.factor) %>% as.data.frame
      if (!is_empty(fv)) {
        fvars(colnames(dt)[fv[, 1]])
        nfvars(setdiff(colnames(dt), fvars()))
      }
    })


    # ui ----
    output$process_tabs = renderUI({
      tabsetPanel(
        id = "process_tabs",
        # 变量(列名)重命名 ----
        tabPanel(
          "变量(列名)重命名",
          tags$br(),
          selectInput(ns("modifyVarName"), "选择重命名的变量", choices = allVars()),
          textInput(ns("newVarName"), "设置变量的新名称", value = ""),
          p("变量名中不要含特殊字符", style = "color:red"),

          actionButton(ns("modifyVarNameBtn"), "确定修改", class = "btn-success")
        ),

        # 变量类型重置 ----
        tabPanel(
          "变量类型重置",
          tags$br(),

          pickerInput(
            ns("modfiyVarsClass"),
            "选择变量",
            choices = allVars(),
            # selected = fvars(),
            multiple = TRUE,
            options = pickerOptions(container = "body", actionsBox = TRUE),
            width = "100%"
          ),

          pickerInput(
            ns("targetClass"),
            "选择目标数据类型",
            choices = c("charater", "factor", "numeric", "integer"),
            multiple = FALSE,
            options = pickerOptions(container = "body", actionsBox = TRUE),
            width = "100%"
          ),

          actionButton(ns("modfiyVarsClassBtn"), "确定修改", class = "btn-success")
        ),

        # 连续变量转分类变量 ----
        tabPanel("连续变量转分类变量"),

        # 衍生新变量 ----
        tabPanel("衍生新变量"),

        # 分类变量标签重置 ----
        tabPanel(
          "分类变量标签重置",
          tags$br(),
          pickerInput(
            ns("setFactorVarsLabel"),
            strong("选择因子变量",style="color:blue;font-size=20px"),
            choices = fvars(),
            multiple = FALSE,
            options = pickerOptions(container = "body", title = "Nothing Selected"),
            width = "100%"
          ),

          radioGroupButtons(
            ns("modifyMethod"),
            strong("因子变量标签修改方式",style="color:blue;font-size=20px"),
            choices = c("直接在原列上修改"="notNew","生成新变量(列)" = "addNew"),
            selected = "notNew",
            checkIcon = list(
              yes = icon("square-check"),
              no = icon("square")
            )
          ),
          actionButton(ns("setFactorVarsLabelBtn"), "确定修改", class = "btn-success")
        ),

        # 设置分析数据集 ----
        tabPanel("设置分析数据集"),
        # 数据保存 ----
        tabPanel("数据保存")
      )
    })

    # 变量重命名 ----
    observeEvent(input$modifyVarNameBtn, {
      dm <- df1()
      if (nchar(input$modifyVarName) > 0 &&
          nchar(trimws(input$newVarName)) > 0 &&
          input$modifyVarName != trimws(input$newVarName)) {
        cnames <- allVars()
        cnames[which(cnames == input$modifyVarName)] = trimws(input$newVarName)
        allVars(cnames)
        colnames(dm) <- allVars()
        df1(dm)
      }
    })

    # 变量类型重置 ----
    observeEvent(input$modfiyVarsClassBtn, {
      if (length(input$modfiyVarsClass) > 1) {
        dm <- df1()
        if (input$targetClass == "character") {
          dm[, input$modfiyVarsClass] <- lapply(dm[, input$modfiyVarsClass], as.character)
        } else if (input$targetClass == "factor") {
          dm[, input$modfiyVarsClass] <- lapply(dm[, input$modfiyVarsClass], as.factor)
        } else if (input$targetClass == "numeric") {
          dm[, input$modfiyVarsClass] <- lapply(dm[, input$modfiyVarsClass], as.numeric)
        } else if (input$targetClass == "integer") {
          dm[, input$modfiyVarsClass] <- lapply(dm[, input$modfiyVarsClass], as.integer)
        }
        df1(dm)
      } else if (length(input$modfiyVarsClass) == 1) {
        dm <- df1()
        if (input$targetClass == "character") {
          dm[, input$modfiyVarsClass] <- as.character(dm[, input$modfiyVarsClass])
        } else if (input$targetClass == "factor") {
          dm[, input$modfiyVarsClass] <- as.factor(dm[, input$modfiyVarsClass])
        } else if (input$targetClass == "numeric") {
          dm[, input$modfiyVarsClass] <- as.numeric(dm[, input$modfiyVarsClass])
        } else if (input$targetClass == "integer") {
          dm[, input$modfiyVarsClass] <- as.integer(dm[, input$modfiyVarsClass])
        }
        df1(dm)
      }
    })

    # 分类变量标签重置 ----

    observeEvent(input$setFactorVarsLabel,{
      dm2 <- df1()

      input_lst <- reactiveValuesToList(input)
      my_dataframe <- as.data.frame(t(sapply(input_lst, "[", i = 1:max(
        sapply(input_lst, length)
      ))))

      if (sum(str_which(colnames(my_dataframe), "labstxt")) >0) {
        for (i in 1:length(str_which(colnames(my_dataframe), "labstxt"))) {
          removeUI(
            selector = paste0("div:has(> #", ns("labstxt"),i)
          )
        }
      }

      if ((!is.null(input$setFactorVarsLabel)) &&
          input$setFactorVarsLabel %in% allVars()){

        lbs <- unique(dm2[, input$setFactorVarsLabel])
        for (i in 1:length(lbs)) {
          insertUI(
            selector = paste0("#", ns("modifyMethod")),
            where = "beforeBegin",
            ui = textInput(paste0(ns(
              "labstxt"
            ), i), paste0("修改变量标签 ", i," 的值"), value = lbs[i])
          )
        }
      }
    })

    observeEvent(input$setFactorVarsLabelBtn, {
      input_lst <- reactiveValuesToList(input)
      my_dataframe <- as.data.frame(t(sapply(input_lst, "[", i = 1:max(
        sapply(input_lst, length)
      ))))

      # %>% as.data.frame
      newLables <- my_dataframe[, str_which(colnames(my_dataframe), "labstxt")] %>%
        sapply(., as.data.frame) %>% as.data.frame %>% na.omit(.) %>% as.character
      # print(newLables)
      # save(newLables, file = "mydata.RData")

      dm3 <- df1()

      lbs <- unique(dm3[, input$setFactorVarsLabel]) %>% as.character

      if (input$modifyMethod == "notNew") {
        dm3[,input$setFactorVarsLabel] <- as.character(dm3[,input$setFactorVarsLabel])
        for (i in 1:length(lbs)) {
          dm3[input$setFactorVarsLabel][dm3[input$setFactorVarsLabel] == lbs[i]] <- newLables[i]
        }
        dm3[,input$setFactorVarsLabel] <- as.factor(dm3[,input$setFactorVarsLabel])
      } else if (input$modifyMethod == "addNew") {
        newVar <- paste("new",input$setFactorVarsLabel,sep = "_")

        dm3[,newVar] <- dm3[,input$setFactorVarsLabel]

        dm3[,newVar] <- as.character(dm3[,newVar])

        for (i in 1:length(lbs)) {
          dm3[newVar][dm3[newVar] == lbs[i]] <- newLables[i]
        }
        dm3[,newVar] <- as.factor(dm3[,newVar])
      }

      df1(dm3)

      # print(input$modifyMethod)
      # print(t[,str_which(colnames(t),ns("labstxt"))]%>% as.character)
    })


    # 数据表展示 ----
    output$processDataTab = renderDT(
      df1(),
      rownames = FALSE,
      escape = FALSE,
      options = list(
        lengthChange = FALSE,
        dom = 'tipr',
        scrollX = TRUE,
        language = list(
          info = '显示第 _START_ 至 _END_ 项结果，共 _TOTAL_ 项',
          search = '搜索:',
          paginate = list(previous = '上页', `next` = '下页'),
          lengthMenu = '显示 _MENU_ 项结果'
        )
      )
    )


    # 数据概览 ----
    output$processDataSummary <- renderPrint({
      str(df1())
    })

    # 变量概览 ----
    output$processVarsTab = renderDT(
      df1(),
      rownames = FALSE,
      escape = FALSE,
      options = list(
        lengthChange = FALSE,
        dom = 'tipr',
        scrollX = TRUE,
        language = list(
          info = '显示第 _START_ 至 _END_ 项结果，共 _TOTAL_ 项',
          search = '搜索:',
          paginate = list(previous = '上页', `next` = '下页'),
          lengthMenu = '显示 _MENU_ 项结果'
        )
      )
    )


    return(df1)

  })
}

## To be copied in the UI
# mod_dataProcess_ui("dataProcess_1")

## To be copied in the server
# mod_dataProcess_server("dataProcess_1")

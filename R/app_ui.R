#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import survival
#' @import shinyWidgets
#' @import stringr
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # fluidPage(
    #   golem::golem_welcome_page() # Remove this line to start building your UI
    # )
    ui = dashboardPage(
      dashboardHeader(title = "临床数据分析"),
      dashboardSidebar(
        sidebarMenu(
          # Setting id makes input$tabs give the tabName of currently-selected tab
          id = "tabs",
          menuItem("生存分析", tabName = "kmPlot", icon = icon("dashboard")),
          menuItem("Cox回归", tabName = "coxReg", icon = icon("dashboard")),
          menuItem("Logistic回归", tabName = "logiReg", icon = icon("dashboard"))
        )
      ),
      dashboardBody(
        tabItems(
          # 生存分析 ----
          tabItem(
            "kmPlot",
            navbarPage(
              strong("生存分析",style = "color:blue;font-size:28px"),
              ## 生成曲线数据导入 ----
              tabPanel(
                title = strong("数据导入",style = "font-size:16px"),
                mod_dataImport_ui("km")
              ),
              ## 生存曲线数据处理 ----
              tabPanel(
                title = strong("数据处理",style = "font-size:16px"),
                mod_dataProcess_ui("km")
              ),
              ## 生存曲线绘制 ----
              tabPanel(
                title = strong("KM曲线",style = "font-size:16px"),
                mod_kmCurve_ui("km")
              )
            )
          ),

          # cox回归 ----
          tabItem(
            "coxReg",
            navbarPage(
              strong("Cox回归分析",style = "color:blue;font-size:28px"),
              ## Cox回归数据导入 ----
              tabPanel(
                title = strong("数据导入",style = "font-size:16px"),
                mod_dataImport_ui("cox")
              ),
              ## Cox回归数据处理 ----
              tabPanel(
                title = strong("数据处理",style = "font-size:16px"),
                mod_dataProcess_ui("cox")
              ),
              ## COX比例风险模型 ----
              tabPanel(
                title = strong("COX比例风险模型",style = "font-size:16px"),
                mod_regModel_ui("cox")
              )
            )
          ),

          # Logistic回归 ----
          tabItem(
            "logiReg",
            navbarPage(
              strong("Logistic回归分析",style = "color:blue;font-size:28px"),
              ## logi回归数据导入 ----
              tabPanel(
                title = strong("数据导入",style = "font-size:16px"),
                mod_dataImport_ui("logistic")
              ),
              ## Logistic回归数据处理 ----
              tabPanel(
                title = strong("数据处理",style = "font-size:16px"),
                mod_dataProcess_ui("logistic")
              ),
              ## Logistic回归模型 ----
              tabPanel(
                title = strong("Logistic回归模型",style = "font-size:16px"),
                mod_regModel_ui("logistic")
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ytools"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

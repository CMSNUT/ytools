#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
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
          menuItem("生存分析", tabName = "survAnal", icon = icon("dashboard")),
          menuItem("Cox回归", tabName = "coxReg", icon = icon("dashboard")),
          menuItem("Logistic回归", tabName = "logiReg", icon = icon("dashboard"))
        )
      ),
      dashboardBody(
        tabItems(
          # 生存分析 ----
          tabItem(
            "survAnal",
            navbarPage(
              strong("生存分析",style = "color:blue;font-size:28px"),
              ## 生成曲线数据导入 ----
              tabPanel(
               "数据导入",
               mod_dataImport_ui("survAnal")
              ),
              ## 生存曲线数据整理 ----
              tabPanel(
               title = "数据整理",
               mod_dataProcess_ui("survAnal")
              ),
              ## 生存曲线绘制 ----
              tabPanel(
               title = "KM曲线",
               mod_km_ui("survAnal")
              )
            )
          ),

          # cox回归 ----
          tabItem(
            "coxReg",
            navbarPage(
              strong("Cox回归分析",style = "color:blue;font-size:28px"),
              ## cox回归数据导入 ----
              tabPanel(
                "数据导入",
                mod_dataImport_ui("coxReg")
              ),
              ## 生存曲线数据整理 ----
              tabPanel(
                title = "数据整理",
                mod_dataProcess_ui("coxReg")
              ),
              ## 生存曲线绘制 ----
              tabPanel(
                title = "Cox回归",
              )
            )
          ),

          # Logistic回归 ----
          tabItem(
            "logiReg",
            navbarPage(
              strong("Logistic回归分析",style = "color:blue;font-size:28px"),
              ## Logistic回归数据导入 ----
              tabPanel(
                "数据导入",
                mod_dataImport_ui("logiReg")
              ),
              ## 生存曲线数据整理 ----
              tabPanel(
                title = "数据整理",
                mod_dataProcess_ui("logiReg")
              ),
              ## 生存曲线绘制 ----
              tabPanel(
                title = "Logistic回归"
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

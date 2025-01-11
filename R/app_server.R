#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import survival
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # 生存分析 ----
  df <- mod_dataImport_server("km")
  df2 <- mod_dataProcess_server("km",df)
  mod_kmCurve_server("km",df2)


  # observe({
  #   print(str(df2()))
  # })


  # df2 <- mod_dataProcess_server("km")


  # Cox回归分析 ----
  df <- mod_dataImport_server("cox")
  df2 <- mod_dataProcess_server("cox",df)
  mod_regModel_server("cox",df2)


  # Logistic回归分析 ----

  df <- mod_dataImport_server("logistic")
  df2 <- mod_dataProcess_server("logistic",df)
  mod_regModel_server("logistic",df2)


}

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
  data<-mod_dataImport_server("survAnal")
  print(data)
  mod_dataProcess_server("survAnal")
  mod_km_server("survAnal")

  # Cox回归分析
  mod_dataImport_server("coxReg")
  mod_dataProcess_server("coxReg")

  # Logistic回归分析
  mod_dataImport_server("logiReg")
  mod_dataProcess_server("logiReg")

}

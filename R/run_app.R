run_app <- function(port = NULL)
{
if (is.null(port)) {
    shiny::shinyApp(ui = IDBacApp::app_ui(),
                    server = IDBacApp::app_server)
  } else {
    port <- try(as.integer(port))
    if (is.integer(port) && port > 0L) {
      shiny::shinyApp(ui = IDBacApp::app_ui(),
                      server = IDBacApp::app_server,
                      options = list(port = port))
    }
  }
}

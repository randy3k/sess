#' @importFrom R6 R6Class
"_PACKAGE"


#' @export
start <- function(host, port, debug = FALSE) {
    logger$debug_mode(debug)
    server <- SessServer$new(host, port)

    task_callback <- function(expr, value, ok, visible) {
        # testing now
        server$broadcast(paste0(capture.output(expr), collapse = "\n"))

        TRUE
    }

    addTaskCallback(task_callback, name = "sess")
    invisible(server)
}

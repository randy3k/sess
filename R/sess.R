#' @importFrom R6 R6Class
"_PACKAGE"


#' @export
start <- function(host, port, debug = FALSE) {
    logger$debug_mode(debug)
    server <- SessServer$new(host, port)
    invisible(server)
}

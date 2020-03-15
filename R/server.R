SessServer <- R6::R6Class(
    "SessServer",
    public = list(
        server = NULL,
        channels = NULL,

        initialize = function(host, port) {
            self$server <- httpuv::startServer(
                host = host,
                port = port,
                app = list(onWSOpen = self$on_ws_open)
            )
        },
        on_ws_open = function(ws) {
            channel <- SessChannel$new(ws)
            self$channels <- c(self$channels, channel)

            ws$onMessage(
                function(binary, message) {
                    self$on_message(channel, message)
                }
            )
            ws$onClose(
                function() {
                    self$on_close(channel)
                }
            )
        },
        on_message = function(channel, message) {
            channel$handle_raw(message)
        },
        on_close = function(channel) {
            updated_channels <- NULL
            for (c in self$channels) {
                if (!identical(channel, c)) {
                    updated_channels <- c(updated_channels, c)
                }
            }
            self$channels <- updated_channels
        },
        broadcast = function(message) {
            for (channel in self$channels) {
                channel$ws$send(message)
            }
        }
    )
)


SessChannel <- R6::R6Class(
    "SessChannel",
    private = list(
        ticket = 0
    ),
    public = list(
        ws = NULL,
        request_handlers = NULL,
        notification_handlers = NULL,
        request_callbacks = NULL,

        initialize = function(ws) {
            self$ws <- ws
            self$request_callbacks <- collections::Dict()
            self$register_handlers()
        },
        finalize = function() {
            self$request_callbacks$clear()
        },

        get_ticket = function() {
            private$ticket <- private$ticket + 1
            private$ticket
        },

        request = function(method, params) {
            Request$new(
                self$get_ticket(),
                method,
                params
            )
        },

        deliver = function(message, callback = NULL) {
            if (is.null(message)) {
                return(NULL)
            }
            logger$info("deliver: ", class(message))
            method <- message$method
            if (!is.null(method)) {
                logger$info("method: ", method)
            }
            self$ws$send(message$format())
            if (inherits(message, "Request") && !is.null(callback)) {
                id <- message$id
                self$request_callbacks$set(as.character(id), callback)
            }
        },

        handle_raw = function(data) {
            payload <- tryCatchStack(
                jsonlite::fromJSON(data, simplifyVector = FALSE),
                error = function(e) e
            )
            if (inherits(payload, "error")) {
                logger$error("error handling json: ", payload)
                return(NULL)
            }
            pl_names <- names(payload)
            logger$info("received payload.")
            if ("id" %in% pl_names && "method" %in% pl_names) {
                self$handle_request(payload)
            } else if ("method" %in% pl_names) {
                self$handle_notification(payload)
            } else if ("id" %in% pl_names) {
                self$handle_response(payload)
            } else {
                logger$info("unknown message")
            }
        },
        handle_request = function(request) {
            id <- request$id
            method <- request$method
            params <- request$params
            if (method %in% names(self$request_handlers)) {
                logger$info("handling request: ", method)
                tryCatchStack({
                    dispatch <- self$request_handlers[[method]]
                    dispatch(self, id, params)
                },
                error = function(e) {
                    logger$info("internal error:", e)
                    self$deliver(ResponseErrorMessage$new(id, "InternalError", to_string(e)))
                }
                )
            } else {
                logger$info("unknown request: ", method)
                self$deliver(ResponseErrorMessage$new(
                    id, "MethodNotFound", paste0("unknown request ", method)
                ))
            }
        },
        handle_notification = function(notification) {
            method <- notification$method
            params <- notification$params
            if (method %in% names(self$notification_handlers)) {
                logger$info("handling notification: ", method)
                tryCatchStack({
                    dispatch <- self$notification_handlers[[method]]
                    dispatch(self, params)
                },
                error = function(e) {
                    logger$info("internal error:", e)
                }
                )
            } else {
                logger$info("unknown notification: ", method)
            }
        },
        handle_response = function(response) {
            id <- response$id
            callback <- tryCatch(
                self$request_callbacks$pop(as.character(id)),
                error = function(e) NULL
            )
            if ("error" %in% names(response)) {
                logger$info("internal error:", response$error)
            } else if (!is.null(callback)) {
                logger$info("calling callback")
                tryCatchStack(
                    callback(self, response$result),
                    error = function(e) logger$info("callback error: ", e)
                )
            }
        }
    )
)


SessChannel$set("public", "register_handlers", function() {
    self$request_handlers <- list(
        initialize = on_initialize
    )

    self$notification_handlers <- list(
    )
})

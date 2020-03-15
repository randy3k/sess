tryCatchStack <- function(expr, ...) {
    expr <- substitute(expr)
    env <- parent.frame()
    capture_calls <- function(e) {
        calls <- sys.calls()
        ncalls <- length(calls)
        e$calls <- calls[-c(seq_len(frame + 7), ncalls - 1, ncalls)]
        class(e) <- c("errorWithStack", class(e))
        signalCondition(e)
    }
    frame <- sys.nframe()
    tryCatch(withCallingHandlers(eval(expr, env), error = capture_calls), ...)
}


print.errorWithStack <- function(x, ...) {
    cat("Error: ", conditionMessage(x), "\n", sep = "")

    call <- conditionCall(x)
    if (!is.null(call)) {
        cat("Call: ")
        print(call)
    }

    if (length(x$calls)) {
        cat("Stack trace:\n")
        rev_calls <- rev(x$calls)
        for (i in seq_along(rev_calls)) {
            cat(i, ": ", sep = "")
            print(rev_calls[[i]])
        }
    }
    invisible(x)
}


capture_print <- function(x) {
    paste0(utils::capture.output(print(x)), collapse = "\n")
}

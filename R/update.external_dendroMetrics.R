update.external_dendroMetrics <- function
##title<< Update a stored external_dendroMetrics call
##description<< Rebuild the call stored in a previous external_dendroMetrics result, optionally replace named arguments, and either evaluate the updated call or return it unevaluated.
(
    object, ##<< Object returned by external_dendroMetrics().
    ..., ##<< Named arguments used to replace entries in the stored call.
    evaluate = TRUE ##<< If TRUE, evaluate the updated call; otherwise return the call.
) {
    ##details<< The method checks that object inherits from "external_dendroMetrics" and that the original matched call is stored in attr(object, "call"). It then replaces any named arguments supplied in "..." inside that stored call.
    ##details<< Use evaluate = FALSE to inspect the reconstructed call before execution. This is useful when debugging filters, grouping variables, schemas, or volume-method options. The method only changes arguments supplied explicitly in "..."; all other arguments remain as stored in the original call.
    ##value<< A new external_dendroMetrics object when evaluate = TRUE; otherwise the updated call.

    if (!inherits(object, "external_dendroMetrics"))
        stop(
            "'object' must inherit from 'external_dendroMetrics'. ",
            "Call external_dendroMetrics(...) first and then use update(result, ...).",
            call. = FALSE
        )

    call0 <- attr(object, "call")

    if (is.null(call0))
        stop(
            "No stored call found in 'object'. ",
            "Recreate the result with the updatable external_dendroMetrics() definition ",
            "and then call update(result, ...).",
            call. = FALSE
        )

    extras <- as.list(substitute(list(...)))[-1L]

    if (length(extras)) {
        extra_names <- names(extras)
        has_name <- !is.na(extra_names) & nzchar(extra_names)

        if (any(!has_name))
            stop("All arguments in '...' must be named.", call. = FALSE)

        call_list <- as.list(call0)
        for (i in seq_along(extras))
            call_list[[extra_names[[i]]]] <- extras[[i]]
        call0 <- as.call(call_list)
    }

    if (isTRUE(evaluate))
        eval(call0, envir = parent.frame())
    else
        call0
}

attr(update.external_dendroMetrics, "ex") <- function() {
    x <- structure(
        data.frame(plot = "P1", d = 12, h = 8),
        class = c("external_dendroMetrics", "dendroMetrics", "data.frame")
    )

    attr(x, "call") <- quote(
        external_dendroMetrics(
            x = data.frame(plot = "P1", d = 12, h = 8),
            var = c("d", "h"),
            summ.vr = NULL
        )
    )

    update(x, var = c("d", "h", "ba"), evaluate = FALSE)
}

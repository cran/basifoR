# Dominant-height method registry and helpers

new_dominant_height_method <- structure(function
##title<< Define one dominant-height computation method
##description<< Create a compact method specification for registry-based dominant-height calculations.
(
    output = "Hd", ##<< \code{character(1)}. Name of the output column produced by the method.
    fun = NULL, ##<< Optional \code{function}. Direct function used to compute dominant height.
    fun_name = NULL, ##<< Optional \code{character(1)}. Name of a function to resolve at run time when \code{fun} is not supplied.
    diameter_fun = NULL, ##<< Optional paired function used to compute dominant diameter from \code{d} and \code{n}.
    diameter_fun_name = NULL, ##<< Optional run-time name of the paired dominant-diameter function.
    diameter_unit = "mm", ##<< \code{character(1)}. Unit reported for dominant diameter.
    diameter_equation = NULL, ##<< \code{character(1)}. Human-readable dominant-diameter equation.
    unit = "m", ##<< \code{character(1)}. Unit reported for the returned dominant-height value.
    threshold = 100, ##<< \code{numeric(1)}. Reference number of trees per hectare used to define the dominant-height subset.
    equation = NULL, ##<< \code{character(1)}. Human-readable equation used by the method.
    selection_rule = NULL, ##<< \code{character(1)}. Human-readable rule used to select trees before applying the equation.
    fallback = NULL, ##<< \code{character(1)}. Human-readable description of what the method returns when the threshold cannot be reached.
    variables = c( ##<< Named \code{character}. Meaning and expected units of the variables used by \code{equation}.
        h = "tree height in metres",
        d = "diameter at breast height in millimetres",
        n = "tree expansion factor in trees per hectare"
    ),
    required_inputs = c("h", "d", "n"), ##<< \code{character}. Standardized inputs required by the method.
    reference = NULL, ##<< Optional \code{character(1)}. Source or implementation reference.
    description = NULL ##<< Optional \code{character(1)}. Short description of the method.
) {
    if (!is.null(fun) && !is.function(fun))
        stop("'fun' must be a function or NULL.", call. = FALSE)

    if (!is.null(diameter_fun) && !is.function(diameter_fun))
        stop("'diameter_fun' must be a function or NULL.", call. = FALSE)

    if (is.null(fun) && is.null(fun_name))
        stop("Supply either 'fun' or 'fun_name'.", call. = FALSE)

    method <- list(
        output = output,
        fun = fun,
        fun_name = fun_name,
        diameter_fun = diameter_fun,
        diameter_fun_name = diameter_fun_name,
        diameter_unit = diameter_unit,
        diameter_equation = diameter_equation,
        unit = unit,
        threshold = threshold,
        equation = equation,
        selection_rule = selection_rule,
        fallback = fallback,
        variables = variables,
        required_inputs = required_inputs,
        reference = reference,
        description = description
    )

    class(method) <- c("dominant_height_method", class(method))
    method

    ##details<< The returned object stores both the executable method and the
    ##details<< documentation needed to explain it. Users can inspect the active
    ##details<< equation through \code{dominant_height_method_registry()} in the
    ##details<< same way that volume methods are inspected through the volume
    ##details<< registries.
    ##details<<
    ##details<< The object does not evaluate dominant height by itself. It only
    ##details<< records the output name, function or function name, unit,
    ##details<< dominant-tree threshold, selection rule, equation, fallback rule,
    ##details<< variable definitions, and optional reference.
    ##value<< A named \code{list} with class \code{"dominant_height_method"}.
}, ex = function() {
    m <- new_dominant_height_method(
        fun_name = "domheight",
        equation = "Hd = sum(h_i * n_i) / sum(n_i)",
        selection_rule = "Sort trees by decreasing diameter and select dominant trees.",
        fallback = "Use the weighted mean height of all valid trees."
    )
    m$equation
})


default_dominant_height_methods <- structure(function
##title<< Default dominant-height methods
##description<< Return the bundled dominant-height method specifications used by \code{nfiMetrics()} and external metric workflows.
() {
    list(
        Hd = new_dominant_height_method(
            output = "Hd",
            fun = domheight,
            fun_name = "domheight",
            diameter_fun = domdiameter,
            diameter_fun_name = "domdiameter",
            diameter_unit = "mm",
            diameter_equation = paste(
                "Using the same diameter ordering, threshold, and fallback",
                "rule as Hd, Dd = sum_{i in S_d}(d_i * n_i) /",
                "sum_{i in S_d}(n_i)."
            ),
            unit = "m",
            threshold = 100,
            equation = paste(
                "After ordering trees by decreasing diameter, let S be the",
                "smallest set for which cumulative n_i > 100 trees ha-1.",
                "Hd = sum_{i in S}(h_i * n_i) / sum_{i in S}(n_i)."
            ),
            selection_rule = paste(
                "Within each resolved group, order valid trees by decreasing",
                "diameter d and accumulate their expansion factors n until the",
                "cumulative value exceeds 100 trees ha-1."
            ),
            fallback = paste(
                "If the cumulative expansion factor never exceeds 100 trees ha-1,",
                "return the weighted mean height of all valid trees in the group."
            ),
            variables = c(
                h = "tree height returned by basifoR in metres",
                d = "diameter at breast height returned by basifoR in millimetres",
                n = "tree expansion factor returned by basifoR in trees per hectare"
            ),
            required_inputs = c("h", "d", "n"),
            reference = "Legacy basifoR dominant-height helper adapted from IFNdyn dominantHeight logic.",
            description = "Backward-compatible SNFI dominant height used by nfiMetrics()."
        ),
        Hd_strict = new_dominant_height_method(
            output = "Hd",
            fun = domheight_strict,
            fun_name = "domheight_strict",
            diameter_fun = domdiameter_strict,
            diameter_fun_name = "domdiameter_strict",
            diameter_unit = "mm",
            diameter_equation = paste(
                "Using the same diameter ordering, threshold, and fallback",
                "rule as Hd, Dd = sum_{i in S_d}(d_i * n_i) /",
                "sum_{i in S_d}(n_i)."
            ),
            unit = "m",
            threshold = 100,
            equation = paste(
                "After ordering trees by decreasing diameter, let S be the",
                "smallest set for which cumulative n_i >= 100 trees ha-1.",
                "Hd = sum_{i in S}(h_i * n_i) / sum_{i in S}(n_i)."
            ),
            selection_rule = paste(
                "Within each resolved group, order valid trees by decreasing",
                "diameter d and accumulate their expansion factors n until the",
                "cumulative value reaches 100 trees ha-1."
            ),
            fallback = "Return NA when valid trees do not reach 100 trees ha-1.",
            variables = c(
                h = "tree height returned by basifoR in metres",
                d = "diameter at breast height returned by basifoR in millimetres",
                n = "tree expansion factor returned by basifoR in trees per hectare"
            ),
            required_inputs = c("h", "d", "n"),
            reference = "Transparent strict implementation shipped with basifoR.",
            description = "Strict dominant height for sparse or external inventories."
        ),
        Hd_mean_fallback = new_dominant_height_method(
            output = "Hd",
            fun = domheight_mean_fallback,
            fun_name = "domheight_mean_fallback",
            diameter_fun = domdiameter_mean_fallback,
            diameter_fun_name = "domdiameter_mean_fallback",
            diameter_unit = "mm",
            diameter_equation = paste(
                "Using the same diameter ordering, threshold, and fallback",
                "rule as Hd, Dd = sum_{i in S_d}(d_i * n_i) /",
                "sum_{i in S_d}(n_i)."
            ),
            unit = "m",
            threshold = 100,
            equation = paste(
                "After ordering trees by decreasing diameter, let S be the",
                "smallest set for which cumulative n_i >= 100 trees ha-1.",
                "Hd = sum_{i in S}(h_i * n_i) / sum_{i in S}(n_i)."
            ),
            selection_rule = paste(
                "Within each resolved group, order valid trees by decreasing",
                "diameter d and accumulate their expansion factors n until the",
                "cumulative value reaches 100 trees ha-1."
            ),
            fallback = paste(
                "If valid trees do not reach 100 trees ha-1, return the weighted",
                "mean height of all valid trees in the group."
            ),
            variables = c(
                h = "tree height returned by basifoR in metres",
                d = "diameter at breast height returned by basifoR in millimetres",
                n = "tree expansion factor returned by basifoR in trees per hectare"
            ),
            required_inputs = c("h", "d", "n"),
            reference = "Transparent mean-fallback implementation shipped with basifoR.",
            description = "Dominant height with explicit mean fallback."
        )
    )

    ##details<< The default registry contains the backward-compatible method
    ##details<< \code{"Hd"} plus stricter explicit alternatives. The default
    ##details<< \code{"Hd"} mirrors the previous internal \code{domheight()}
    ##details<< behaviour, so existing analyses keep the same numerical convention
    ##details<< while the equation and fallback rule become visible.
    ##value<< A named \code{list} of \code{"dominant_height_method"} objects.
}, ex = function() {
    methods <- default_dominant_height_methods()
    names(methods)
    methods$Hd$equation
})


dominant_height_method_registry <- structure(function
##title<< Build the active dominant-height method registry
##description<< Return the registry of dominant-height methods used by \code{nfiMetrics()} and related metric workflows.
(
    methods = get0( ##<< Optional named \code{list} of method definitions. Each element should follow the structure returned by \code{new_dominant_height_method()}.
                   "dominant_height_methods",
                   inherits = TRUE,
                   ifnotfound = NULL)
) {
    defaults <- default_dominant_height_methods()

    if (is.null(methods))
        methods <- defaults

    if (!is.list(methods) || is.null(names(methods)))
        stop("'dominant_height_methods' must be a named list.", call. = FALSE)

    extra <- getOption("basifoR.dominant_height_methods")
    if (!is.null(extra)) {
        if (!is.list(extra) || is.null(names(extra)))
            stop("Option 'basifoR.dominant_height_methods' must be a named list.",
                 call. = FALSE)
        methods <- utils::modifyList(methods, extra)
    }

    utils::modifyList(defaults, methods)

    ##details<< The returned registry is built in three steps. First,
    ##details<< \code{default_dominant_height_methods()} provides the bundled
    ##details<< definitions. Second, \code{methods} replaces or extends those
    ##details<< defaults. Third, named entries in option
    ##details<< \code{"basifoR.dominant_height_methods"} override both. This
    ##details<< mirrors the volume-method registry pattern and lets users inspect
    ##details<< or replace the dominant-height method without editing package code.
    ##value<< A named \code{list} of dominant-height method definitions.
}, ex = function() {
    reg <- dominant_height_method_registry()
    names(reg)
    reg$Hd$equation

    custom <- list(
        Hd = new_dominant_height_method(
            fun_name = "domheight_strict",
            equation = "Hd = sum(h_i * n_i) / sum(n_i)",
            selection_rule = "Use the largest trees until n reaches 100 trees ha-1.",
            fallback = "Return NA if the threshold is not reached."
        )
    )
    dominant_height_method_registry(custom)$Hd$fallback
})


# Resolve a dominant-height method and its paired dominant-diameter function.
resolve_dominant_height_method <- function(method = "Hd",
                                           registry = dominant_height_method_registry()) {
    if (is.null(method) || !length(method))
        stop("'domheight_method' must identify one dominant-height method.",
             call. = FALSE)

    if (!is.list(registry) || is.null(names(registry)))
        stop("'domheight_registry' must be a named list.", call. = FALSE)

    key <- as.character(method[1L])
    hit <- names(registry)[toupper(names(registry)) == toupper(key)]
    if (!length(hit))
        stop("Unknown dominant-height method: ", key, call. = FALSE)
    hit <- hit[1L]

    def <- registry[[hit]]

    resolve_fun <- function(fun, fun_name, label, required = TRUE) {
        if (is.null(fun) && !is.null(fun_name) && nzchar(fun_name)) {
            fun <- get0(fun_name, mode = "function",
                        envir = parent.frame(), inherits = TRUE)

            if (is.null(fun))
                fun <- get0(fun_name, mode = "function",
                            envir = environment(), inherits = TRUE)

            if (is.null(fun))
                fun <- get0(fun_name, mode = "function",
                            envir = .GlobalEnv, inherits = TRUE)

            if (is.null(fun) && "basifoR" %in% loadedNamespaces())
                fun <- get0(fun_name, mode = "function",
                            envir = asNamespace("basifoR"), inherits = TRUE)
        }

        if (required && !is.function(fun))
            stop("Could not resolve ", label, " for dominant method '", hit, "'.",
                 call. = FALSE)

        if (!is.null(fun) && !is.function(fun))
            stop("Invalid ", label, " for dominant method '", hit, "'.",
                 call. = FALSE)

        fun
    }

    fun <- resolve_fun(def$fun, def$fun_name,
                       "dominant-height function", required = TRUE)
    diameter_fun <- resolve_fun(
        def$diameter_fun,
        def$diameter_fun_name,
        "dominant-diameter function",
        required = FALSE
    )

    meta <- def[setdiff(names(def), c("fun", "diameter_fun"))]
    meta$method <- hit

    list(
        method = hit,
        definition = def,
        fun = fun,
        diameter_fun = diameter_fun,
        meta = meta
    )
}


# Validate, filter, and order inputs used to calculate dominant height.
prepare_domheight_inputs <- function(h, d, n) {
    h <- as.numeric(h)
    d <- as.numeric(d)
    n <- as.numeric(n)

    if (!(length(h) == length(d) && length(d) == length(n))) {
        stop("'h', 'd', and 'n' must have the same length.", call. = FALSE)
    }

    ok <- is.finite(h) & is.finite(d) & is.finite(n) & h > 0 & d > 0 & n > 0
    if (!any(ok)) {
        return(list(h = numeric(0), d = numeric(0), n = numeric(0)))
    }

    h <- h[ok]
    d <- d[ok]
    n <- n[ok]

    o <- order(d, decreasing = TRUE)
    list(h = h[o], d = d[o], n = n[o])
}


# Calculate an expansion-factor-weighted mean height.
weighted_height_mean <- function(h, n) {
    sw <- sum(n, na.rm = TRUE)
    if (!is.finite(sw) || sw <= 0) {
        return(NA_real_)
    }
    sum(h * n, na.rm = TRUE) / sw
}


# Validate, filter, and order inputs used to calculate dominant diameter.
prepare_domdiameter_inputs <- function(d, n) {
    d <- as.numeric(d)
    n <- as.numeric(n)

    if (length(d) != length(n))
        stop("'d' and 'n' must have the same length.", call. = FALSE)

    ok <- is.finite(d) & is.finite(n) & d > 0 & n > 0
    if (!any(ok))
        return(list(d = numeric(0), n = numeric(0)))

    d <- d[ok]
    n <- n[ok]
    o <- order(d, decreasing = TRUE)
    list(d = d[o], n = n[o])
}


# Calculate an expansion-factor-weighted mean diameter.
weighted_diameter_mean <- function(d, n) {
    sw <- sum(n, na.rm = TRUE)
    if (!is.finite(sw) || sw <= 0)
        return(NA_real_)

    sum(d * n, na.rm = TRUE) / sw
}


# Select the leading trees that reach the dominant-tree density threshold.
select_domheight_slice <- function(n, threshold = 100) {
    threshold <- as.numeric(threshold)[1]
    if (!is.finite(threshold) || threshold <= 0) {
        stop("'threshold' must be a single positive number.", call. = FALSE)
    }

    i <- which(cumsum(n) >= threshold)[1]
    if (is.na(i)) {
        return(integer(0))
    }

    seq_len(i)
}


# Calculate dominant height using the legacy basifoR selection rule.
domheight <- function(h, d, n) {
    ## Legacy basifoR dominant-height helper. Kept as the default method so
    ## existing analyses retain the previous numerical convention.
    o <- order(d, decreasing = TRUE)
    h <- h[o]
    n <- n[o]
    ncum <- 0

    for (i in seq_along(h)) {
        ncum <- ncum + n[i]
        if (!is.na(ncum) && ncum > 100) {
            return(sum(h[1:i] * n[1:i], na.rm = TRUE) /
                       sum(h[1:i] * n[1:i] / h[1:i], na.rm = TRUE))
        }
    }

    sum(h * n, na.rm = TRUE) / sum(n, na.rm = TRUE)
}


## Dominant diameter paired with the legacy Hd method: use the largest trees
## until cumulative expansion exceeds the threshold, then use a weighted mean.
# Calculate dominant diameter paired with the legacy dominant-height rule.
domdiameter <- function(d, n, threshold = 100) {
    x <- prepare_domdiameter_inputs(d, n)
    if (!length(x$d))
        return(NA_real_)

    i <- which(cumsum(x$n) > threshold)[1L]
    if (is.na(i))
        return(weighted_diameter_mean(x$d, x$n))

    weighted_diameter_mean(x$d[seq_len(i)], x$n[seq_len(i)])
}


## Strict dominant height: return NA when the valid trees do not reach the
## threshold.
# Calculate dominant height and return missing when the threshold is not reached.
domheight_strict <- function(h, d, n, threshold = 100) {
    x <- prepare_domheight_inputs(h, d, n)
    if (!length(x$h)) {
        return(NA_real_)
    }

    idx <- select_domheight_slice(x$n, threshold = threshold)
    if (!length(idx)) {
        return(NA_real_)
    }

    weighted_height_mean(x$h[idx], x$n[idx])
}


## Dominant diameter paired with Hd_strict.
# Calculate dominant diameter and return missing when the threshold is not reached.
domdiameter_strict <- function(d, n, threshold = 100) {
    x <- prepare_domdiameter_inputs(d, n)
    if (!length(x$d))
        return(NA_real_)

    idx <- select_domheight_slice(x$n, threshold = threshold)
    if (!length(idx))
        return(NA_real_)

    weighted_diameter_mean(x$d[idx], x$n[idx])
}


## Backward-compatible explicit method: return the weighted mean height of all
## valid trees when the threshold is not reached.
# Calculate dominant height with an all-tree weighted-mean fallback.
domheight_mean_fallback <- function(h, d, n, threshold = 100) {
    x <- prepare_domheight_inputs(h, d, n)
    if (!length(x$h)) {
        return(NA_real_)
    }

    idx <- select_domheight_slice(x$n, threshold = threshold)
    if (!length(idx)) {
        return(weighted_height_mean(x$h, x$n))
    }

    weighted_height_mean(x$h[idx], x$n[idx])
}


## Dominant diameter paired with Hd_mean_fallback.
# Calculate dominant diameter with an all-tree weighted-mean fallback.
domdiameter_mean_fallback <- function(d, n, threshold = 100) {
    x <- prepare_domdiameter_inputs(d, n)
    if (!length(x$d))
        return(NA_real_)

    idx <- select_domheight_slice(x$n, threshold = threshold)
    if (!length(idx))
        return(weighted_diameter_mean(x$d, x$n))

    weighted_diameter_mean(x$d[idx], x$n[idx])
}


## Flexible dominant height:
## fallback = "mean" reproduces the current package logic more clearly.
## fallback = "na" is the safer option for sparse external NFI height data.
# Calculate dominant height with a selectable sparse-plot fallback.
domheight_flexible <- function(h, d, n, threshold = 100,
                               fallback = c("mean", "na")) {
    fallback <- match.arg(fallback)
    x <- prepare_domheight_inputs(h, d, n)
    if (!length(x$h)) {
        return(NA_real_)
    }

    idx <- select_domheight_slice(x$n, threshold = threshold)
    if (!length(idx)) {
        if (identical(fallback, "na")) {
            return(NA_real_)
        }
        return(weighted_height_mean(x$h, x$n))
    }

    weighted_height_mean(x$h[idx], x$n[idx])
}

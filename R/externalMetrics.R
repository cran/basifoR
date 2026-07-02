externalMetrics <- structure(function
##title<< Compute tree-level metrics from external inventory data
##description<< Standardize external tree measurements for external inventory workflows and return requested tree-level metrics in basifoR units.
(
    x, ##<< Input data.frame with one row per tree or stem.
    var = c("d", "h", "ba", "n", "Hd", "Dd"), ##<< Requested metrics to return. Supported values are \code{"d"}, \code{"h"}, \code{"ba"}, \code{"n"}, \code{"Hd"}, and \code{"Dd"}. Requesting \code{"Hd"} requires \code{"d"}, \code{"h"}, and \code{"n"}; requesting \code{"Dd"} requires \code{"d"} and \code{"n"}.
    levels = NULL, ##<< Columns defining the groups within which dominant
                   ## metrics are computed.
    design, ##<< Inventory design used to compute expansion factors for \code{n}.
    colmap = NULL,
### Named list of candidate raw column names for diameter and height.
### Matching is case-insensitive and also accepts numeric suffixes such
### as \code{diameter_1} or \code{height.2}.
    d_unit = c("mm", "cm")[1], ##<< Unit of raw diameter columns in \code{colmap$d}.
    h_unit = c("m", "dm", "cm")[1], ##<< Unit of raw height columns in \code{colmap$h}.
    keep_cols = NULL, ##<< Additional source columns to carry into the result
                      ## without changing the groups defined by \code{levels}.
    domheight_fun = NULL, ##<< Optional function used when \code{"Hd"} is requested. If supplied, it overrides \code{domheight_method}.
    domdiameter_fun = NULL, ##<< Optional function used when \code{"Dd"} is
                            ## requested together with a custom
                            ## \code{domheight_fun}. It must accept \code{d}
                            ## and \code{n}.
    domheight_method = "Hd_strict", ##<< Dominant-height method code resolved through \code{domheight_registry} when \code{domheight_fun = NULL}.
    domheight_registry = dominant_height_method_registry() ##<< Named registry created with \code{\link{dominant_height_method_registry}}.
) {
    ##details<< The function first resolves measurement columns from \code{colmap}. Exact matches are preferred, then case-insensitive matches with optional numeric suffixes are considered. When several repeated measurement columns are found for the same variable, row-wise non-missing means are used.
    ##details<< Zero values in resolved diameter or height columns are treated as missing before aggregation. Returned units are standardized to millimetres for \code{d} and \code{Dd}, metres for \code{h} and \code{Hd}, square metres per tree for \code{ba}, and trees per hectare for \code{n}.
    ##details<< When \code{var} includes \code{"n"}, the function uses \code{design} to obtain expansion factors. Fixed-area designs call \code{trees_per_ha()}, while concentric designs choose the proper factor from the design thresholds. Dominant height and dominant diameter are computed within each resolved group defined only by \code{levels}; \code{keep_cols} preserves source columns without changing those groups. Both metrics use the diameter ordering, threshold, and fallback convention selected through \code{domheight_registry}; \code{Dd} uses valid diameter and expansion-factor values and therefore may be available when \code{Hd} is missing because heights are unavailable. When both metrics are requested with custom functions, supply both \code{domheight_fun} and \code{domdiameter_fun} so the pair remains explicit.
    ##value<< A data.frame containing the requested tree-level metrics, optionally preceded by resolved grouping columns.
    ##value<< The returned object inherits from classes \code{"externalMetrics"} and \code{"nfiMetrics"}. Unit metadata are stored in \code{attr(out, "units")}. When \code{var} includes \code{"n"}, \code{"Hd"}, or \code{"Dd"}, the result also stores sampling design metadata in \code{attr(out, "design_meta")}.

    x0 <- x
    if (is.null(x0))
        return(x)

    if (!is.data.frame(x))
        stop("'x' must be a data.frame.", call. = FALSE)

    if (missing(design) || is.null(design))
        stop("'design' must be supplied.", call. = FALSE)

    if (!inherits(design, "inventory_design"))
        stop("'design' must inherit from 'inventory_design'.", call. = FALSE)

    if (is.null(colmap)) {
        colmap <- list(
            d = c("d", "dbh", "diameter", "diameter_mm"),
            h = c("h", "height", "height_m")
        )
    }
        
    method_info <- resolve_dominant_height_method(
        method = domheight_method,
        registry = domheight_registry
    )
    custom_domheight <- !is.null(domheight_fun)
    custom_domdiameter <- !is.null(domdiameter_fun)

    if (custom_domheight && !is.function(domheight_fun))
        stop("'domheight_fun' must be a function or NULL.", call. = FALSE)

    if (custom_domheight && "Dd" %in% var && !custom_domdiameter) {
        stop(
            "When 'Dd' is requested with a custom 'domheight_fun', ",
            "supply the paired 'domdiameter_fun'.",
            call. = FALSE
        )
    }
    if (custom_domdiameter && "Hd" %in% var && !custom_domheight) {
        stop(
            "When 'Hd' is requested with a custom 'domdiameter_fun', ",
            "supply the paired 'domheight_fun'.",
            call. = FALSE
        )
    }

    if (is.null(domdiameter_fun))
        domdiameter_fun <- method_info$diameter_fun
    if (!is.null(domdiameter_fun) && !is.function(domdiameter_fun))
        stop("'domdiameter_fun' must be a function or NULL.", call. = FALSE)

    dominant_height_meta <- NULL
    dominant_diameter_meta <- NULL
    if (is.null(domheight_fun)) {
        domheight_fun <- method_info$fun
        dominant_height_meta <- method_info$meta
    } else {
        dominant_height_meta <- list(
            method = "custom_function",
            output = "Hd",
            fun_name = NA_character_,
            unit = "m",
            equation = NA_character_,
            selection_rule = "User-supplied dominant-height function.",
            fallback = NA_character_
        )
    }
    
    d_unit <- match.arg(d_unit, c("mm", "cm"))
    h_unit <- match.arg(h_unit, c("m", "dm", "cm"))

    resolve_measure_cols <- function(dt, aliases) {
        if (is.null(aliases) || !length(aliases))
            return(character(0))

        nm0 <- names(dt)
        nml <- tolower(nm0)
        ali <- tolower(aliases)

        ii <- match(ali, nml)
        ii <- ii[!is.na(ii)]
        if (length(ii) > 0L)
            return(nm0[ii[1L]])

        hits <- integer(0)
        for (a in ali) {
            rx <- paste0("^", a, "([._]?[0-9]+)?$")
            hits <- c(hits, grep(rx, nml, perl = TRUE))
        }
        hits <- unique(hits)

        if (!length(hits))
            return(character(0))

        cols <- nm0[hits]
        base <- sub("([._]?[0-9]+)$", "", tolower(cols))

        if (length(unique(base)) > 1L) {
            warning(
                "Ambiguous measurement columns matched: ",
                paste(cols, collapse = ", "),
                ". Using ", cols[1L],
                call. = FALSE
            )
            return(cols[1L])
        }

        cols[order(cols)]
    }

    resolve_group_cols <- function(dt, cols) {
        if (is.null(cols) || !length(cols))
            return(character(0))

        nm0 <- names(dt)
        nml <- tolower(nm0)
        out <- character(0)

        for (cl in cols) {
            i <- match(tolower(cl), nml)

            if (!is.na(i)) {
                out <- c(out, nm0[i])
                next
            }

            j <- grep(tolower(cl), nml, fixed = TRUE)
            if (length(j) == 1L) {
                out <- c(out, nm0[j])
            } else if (length(j) > 1L) {
                warning(
                    "Ambiguous grouping column '", cl,
                    "'. Using ", nm0[j[1L]],
                    call. = FALSE
                )
                out <- c(out, nm0[j[1L]])
            }
        }

        unique(out)
    }

    get_numeric_matrix <- function(dt, cols) {
        if (!length(cols))
            return(NULL)

        y <- dt[, cols, drop = FALSE]
        y <- lapply(y, function(z) as.numeric(as.character(z)))
        y <- as.data.frame(y, check.names = FALSE, stringsAsFactors = FALSE)
        as.matrix(y)
    }

    convert_d_to_mm <- function(z, unit) {
        if (unit == "mm") return(z)
        if (unit == "cm") return(z * 10)
        stop("Unsupported diameter unit: ", unit, call. = FALSE)
    }

    convert_h_to_m <- function(z, unit) {
        if (unit == "m")  return(z)
        if (unit == "dm") return(z / 10)
        if (unit == "cm") return(z / 100)
        stop("Unsupported height unit: ", unit, call. = FALSE)
    }

    mm_to_cm <- function(z) z / 10

    trees_per_ha_vec <- function(dbh_cm, design) {
        if (inherits(design, "concentric_design")) {
            out <- rep(NA_real_, length(dbh_cm))
            ok <- !is.na(dbh_cm) & dbh_cm >= design$min_dbh_cm[1]
            if (any(ok)) {
                idx <- findInterval(dbh_cm[ok], design$min_dbh_cm)
                out[ok] <- design$sf[idx]
            }
            return(out)
        }

        vapply(
            dbh_cm,
            function(v) trees_per_ha(design = design, dbh_cm = v),
            numeric(1)
        )
    }

    var0 <- unique(var)
    if ("Hd" %in% var0 && !all(c("d", "h", "n") %in% var0))
        stop("Hd requires var to include 'd', 'h', and 'n'.", call. = FALSE)
    if ("Dd" %in% var0 && !all(c("d", "n") %in% var0))
        stop("Dd requires var to include 'd' and 'n'.", call. = FALSE)
    if ("Dd" %in% var0 && !is.function(domdiameter_fun)) {
        stop(
            "Dominant method '", method_info$method,
            "' does not define a paired dominant-diameter function.",
            call. = FALSE
        )
    }

    var_base <- setdiff(var0, c("Hd", "Dd"))

    diam_cols <- character(0)
    ht_cols   <- character(0)

    if (any(var_base %in% c("d", "ba", "n")))
        diam_cols <- resolve_measure_cols(x, colmap$d %||% character(0))

    if (any(var_base %in% c("h")))
        ht_cols <- resolve_measure_cols(x, colmap$h %||% character(0))

    if (any(var_base %in% c("d", "ba", "n")) && !length(diam_cols))
        stop("Could not resolve diameter column(s). Check 'colmap$d'.",
             call. = FALSE)

    if (any(var_base %in% c("h")) && !length(ht_cols))
        stop("Could not resolve height column(s). Check 'colmap$h'.",
             call. = FALSE)

    diam_mm <- NULL
    diam_cm <- NULL
    ht_m    <- NULL
    trees_ha <- NULL

    if (length(diam_cols)) {
        diam_mat <- get_numeric_matrix(x, diam_cols)
        diam_mat[diam_mat == 0] <- NA_real_

        if (ncol(diam_mat) == 1L) {
            diam_raw <- diam_mat[, 1L]
        } else {
            nn <- rowSums(!is.na(diam_mat))
            diam_raw <- rowMeans(diam_mat, na.rm = TRUE)
            diam_raw[nn == 0L] <- NA_real_
        }

        diam_mm <- convert_d_to_mm(diam_raw, d_unit)
        diam_cm <- mm_to_cm(diam_mm)

        if ("n" %in% var_base)
            trees_ha <- trees_per_ha_vec(diam_cm, design)
    }

    if (length(ht_cols)) {
        ht_mat <- get_numeric_matrix(x, ht_cols)
        ht_mat[ht_mat == 0] <- NA_real_

        if (ncol(ht_mat) == 1L) {
            ht_raw <- ht_mat[, 1L]
        } else {
            nn <- rowSums(!is.na(ht_mat))
            ht_raw <- rowMeans(ht_mat, na.rm = TRUE)
            ht_raw[nn == 0L] <- NA_real_
        }

        ht_m <- convert_h_to_m(ht_raw, h_unit)
    }

    out_metrics <- list()

    for (met in var_base) {
        if (met == "d")
            out_metrics[[met]] <- diam_mm

        if (met == "h")
            out_metrics[[met]] <- ht_m

        if (met == "ba")
            out_metrics[[met]] <- pi * diam_cm^2 / 40000

        if (met == "n")
            out_metrics[[met]] <- trees_ha
    }

    out <- data.frame(out_metrics, check.names = FALSE, stringsAsFactors = FALSE)

    group_cols <- resolve_group_cols(x, levels)
    retained_cols <- resolve_group_cols(x, unique(c(levels, keep_cols)))
    if (length(retained_cols)) {
        out <- data.frame(
            x[, retained_cols, drop = FALSE],
            out,
            check.names = FALSE,
            stringsAsFactors = FALSE
        )
    }

    dominant_requested <- intersect(c("Hd", "Dd"), var0)
    if (length(dominant_requested)) {
        if (is.null(domheight_fun))
            stop("Hd requested but no dominant-height function is available.",
                 call. = FALSE)

        grp <- if (length(group_cols)) {
            interaction(x[, group_cols, drop = FALSE], drop = TRUE, lex.order = TRUE)
        } else {
            factor(rep("all", nrow(x)))
        }

        spl <- split(out, grp, drop = TRUE)

        spl <- lapply(spl, function(y) {
            if ("Hd" %in% dominant_requested) {
                y$Hd <- tryCatch(
                    domheight_fun(h = y$h, d = y$d, n = y$n),
                    error = function(e) NA_real_
                )
            }
            if ("Dd" %in% dominant_requested) {
                y$Dd <- tryCatch(
                    domdiameter_fun(d = y$d, n = y$n),
                    error = function(e) NA_real_
                )
            }
            y
        })

        out <- do.call(rbind, spl)
        rownames(out) <- NULL
    }

    metric_units <- c(
        d  = "mm",
        h  = "m",
        ba = "m2",
        n  = "",
        Hd = "m",
        Dd = "mm"
    )
    attr(out, "units") <- metric_units[intersect(names(out), names(metric_units))]

    if ("Hd" %in% var0 && !is.null(dominant_height_meta))
        attr(out, "dominant_height_meta") <- dominant_height_meta

    if ("Dd" %in% var0) {
        if (custom_domdiameter) {
            dominant_diameter_meta <- list(
                method = "custom_function",
                output = "Dd",
                fun_name = NA_character_,
                unit = "mm",
                equation = NA_character_,
                selection_rule = "User-supplied dominant-diameter function.",
                fallback = NA_character_
            )
        } else {
            dominant_diameter_meta <- method_info$meta
            dominant_diameter_meta$output <- "Dd"
            dominant_diameter_meta$unit <- method_info$definition$diameter_unit
            dominant_diameter_meta$equation <- method_info$definition$diameter_equation
            dominant_diameter_meta$fun_name <- method_info$definition$diameter_fun_name
        }
        attr(out, "dominant_diameter_meta") <- dominant_diameter_meta
    }

    if (any(var0 %in% c("n", "Hd", "Dd"))) {
        attr(out, "design_meta") <- list(
            name = design$name %||% NA_character_,
            class = class(design),
            min_dbh_cm = design$min_dbh_cm %||% NA_real_,
            sample_area_m2 = design$sample_area_m2 %||% NA_real_,
            expansion_factor = design$sf %||% NA_real_,
            metadata = design$metadata %||% list(),
            used_for = "n",
            returned_unit = "trees/ha"
        )
    }

    class(out) <- unique(c("externalMetrics", "nfiMetrics", class(out)))
    out
}, ex = function() {
    sq_0.1ha <- new_inventory_design(
        sample_area_m2 = 1000,
        min_dbh_cm = 7.5,
        name = "Square 0.1-ha plot",
        metadata = list(shape = "square", side_m = sqrt(1000))
    )

    x <- data.frame(
        plot = c("P1", "P1", "P2"),
        species = c("sp1", "sp1", "sp2"),
        diameter_mm = c(120, 185, 260),
        height_m = c(7.1, 9.4, 13.2),
        stringsAsFactors = FALSE
    )

    externalMetrics(
        x = x,
        var = c("d", "h", "ba", "n"),
        levels = c("plot", "species"),
        design = sq_0.1ha,
        colmap = list(d = "diameter_mm", h = "height_m"),
        d_unit = "mm",
        h_unit = "m"
    )
})

external_dendroMetrics <- structure(function
##title<< Summarize external inventory tree data and optional volume outputs
##description<< Process external tree data through a unified workflow that standardizes measurements, computes missing dendrometric variables, optionally derives volume outputs, and returns either tree-level records or grouped stand-level summaries.
##description<< The function can work from already standardized inputs or from raw external tables, and it supports repeated processing of several tables with optional parallel execution.
##details<< This wrapper mirrors the arguments and behaviour of \code{dendroMetrics()}.
##details<< When \code{summ.vr = NULL}, the function returns tree-level records after applying \code{cut.dt}. When \code{summ.vr} contains one or more grouping variables, the function aggregates by those groups, reporting weighted means for \code{d}, \code{h}, and \code{Hd}, arithmetic sums for \code{ba}, \code{n}, and volume variables, and quadratic mean diameter \code{dg} when diameter and trees-per-hectare are available.
##details<< The function can work from already standardized inputs or from raw external tables. If requested metrics are missing and \code{compute_metrics_if_needed = TRUE}, it calls \code{externalMetrics()} internally, so raw inputs usually need a valid \code{design} plus diameter and, when relevant, height mappings. When \code{schema} is supplied, it provides reusable defaults for column aliases, units, grouping variables, and retained columns, while explicit arguments supplied in the call override those defaults.
##details<< Volume outputs are optional. They are computed only when \code{parametro} is supplied explicitly or can be inferred from \code{var} and \code{method_registry}. This lets the same entry point handle metric-only workflows, mixed metric-plus-volume workflows, and repeated processing of several input tables with optional parallel execution through \code{mc.cores}.
(
    x,
    ### External input table, processed table, or list of such objects.
### Raw inputs must contain enough columns to resolve the requested metrics
### and, if needed, the volume selectors.
    summ.vr = NULL,
    ### Grouping variable or character vector of grouping variables.
### Leave \code{NULL} to keep tree-level output; supply one or more column
### names to obtain grouped summaries.
    cut.dt = "d == d",
    ### Character filter evaluated on the final table.
### The expression is evaluated with \verb{eval(parse(...))} after metrics or
### summaries have been computed.
    report = FALSE,
    ### Whether to write the returned table to \file{report.csv}.
### The file is written in the working directory only when \code{report = TRUE}.
    mc.cores = getOption("mc.cores", 1L),
    ### Number of worker processes used when \code{x} is a list.
### Values smaller than 1 are reset to 1.
    var = c("d", "h", "ba", "n", "Hd"),
    ### Requested variables.
### Typical metric requests are \code{"d"}, \code{"h"}, \code{"ba"},
### \code{"n"}, and \code{"Hd"}; volume-like names can also trigger volume
### processing.
    parametro = NULL,
    ### Optional volume-method codes, for example \code{"V"}.
### If \code{NULL}, the function tries to infer required methods from
### \code{var} and \code{method_registry}.
    design = NULL,
    ### Inventory design used when missing metrics must be computed.
### Supply an object inheriting from \code{"inventory_design"} when the
### function must derive metrics such as \code{n} from raw external data.
    parameter_table = NULL,
    ### Optional parameter table for the volume stage.
### Passed to \code{externalMetrics2Vol()} for method-specific coefficients or
### selection metadata.
    method_registry = external_volume_method_registry(),
    ### Volume method registry.
### Usually created by \code{external_volume_method_registry()} or a custom
### registry following the same structure.
    levels = NULL,
    ### Grouping variables forwarded to schema resolution.
### When \code{schema} is not used, these serve as defaults for workflow
### grouping metadata.
    metric_levels = NULL,
    ### Grouping variables used during internal metric computation.
### These are forwarded specifically to \code{externalMetrics()} when missing
### metrics are derived internally.
    keep_cols = NULL,
    ### Source columns to preserve in the output or schema defaults.
### These columns are retained in the returned data whenever possible.
    metric_keep_cols = NULL,
    ### Columns to preserve during internal metric computation.
### These are passed to \code{externalMetrics()} when metrics are derived
### internally.
    colmap = NULL,
    ### Optional aliases that update the default mappings.
### Use this when source names differ from the expected volume or metric names.
    metric_colmap = list(
        d = c("d", "dbh", "diameter", "diameter_mm"),
        h = c("h", "height", "height_m")
    ),
    ### Aliases used to resolve raw diameter and height columns.
### These aliases are used during internal metric computation.
    d_unit = NULL,
    ### Optional diameter unit override shared with schema resolution.
### Accepted values are \code{"mm"} and \code{"cm"}.
    metric_d_unit = c("mm", "cm")[1],
    ### Diameter unit expected by \code{metric_colmap}.
### Used when metrics must be computed internally.
    h_unit = NULL,
    ### Optional height unit override shared with schema resolution.
### Accepted values are \code{"m"}, \code{"dm"}, and \code{"cm"}.
    metric_h_unit = c("m", "dm", "cm")[1],
    ### Height unit expected by \code{metric_colmap}.
### Used when metrics must be computed internally.
    tree_d_unit_out = NULL,
    ### Optional output unit for tree-level diameter values.
### Only used when \code{summ.vr = NULL}. Accepted values are \code{"mm"}
### and \code{"cm"}.
    tree_h_unit_out = NULL,
    ### Optional output unit for tree-level height values.
### Only used when \code{summ.vr = NULL}. Accepted values are \code{"m"},
### \code{"dm"}, and \code{"cm"}.
    volume_colmap = list(
        d = c("d"),
        h = c("h"),
        dnm = c("dnm", "d_nm", "D.n.m."),
        v = c("v"),
        species = c("species", "spec", "especie"),
        region = c("region", "pr"),
        equation_set = c("equation_set", "eqset", "tariff", "model_set")
    ),
    ### Aliases used by the optional volume stage.
### Used to locate metrics, species, region, and equation-set selectors.
    selector = c("first", "priority")[1],
    ### Rule used by \code{externalMetrics2Vol()} when several matches remain.
### \code{"first"} keeps the first surviving row; \code{"priority"} uses the
### highest numeric \code{priority} value when available.
    track_provenance = FALSE,
    ### Whether to keep provenance columns from the volume workflow.
### When \code{TRUE}, provenance columns are retained until the final summary
### stage removes them from grouped output.
    compute_metrics_if_needed = TRUE,
    ### Whether to compute missing metrics from raw inputs.
### If \code{FALSE}, the function stops when required standardized columns are
### absent.
    schema = NULL,
    ### Optional \code{"external_schema"} object created by \code{new_external_schema()}.
### It centralizes column aliases, units, grouping defaults, and kept columns
### for repeated workflows.
    domheight_fun = null_or(
    get0("domheight_strict", mode = "function", inherits = TRUE),
    get0("domheight", mode = "function", inherits = TRUE)
),
        ### Function used to compute dominant height.
### Used when \code{"Hd"} is requested during internal metric computation.
    ...
    ### Additional arguments passed to downstream helpers.
### These are mainly useful for custom options in the internal metric or
### volume-processing steps.
) {
    ##value<< A \code{data.frame} with class \verb{c("external_dendroMetrics", "dendroMetrics", ...)}.
    ##value<< With \code{summ.vr = NULL}, the returned rows represent tree-level records, optionally converted to \code{tree_d_unit_out} and \code{tree_h_unit_out}. With \code{summ.vr} supplied, the returned rows represent grouped summaries and include standardized summary units such as \code{"cm"}, \code{"m"}, \code{"m2 ha-1"}, \code{"ha-1"}, and \code{"m3 ha-1"} when those variables are present.
    ##value<< The returned object stores the matched call in \code{attr(out, "call")}. When available, it also preserves \code{"units"}, \code{"design_meta"}, and \code{"volume_meta"} attributes from upstream processing, which makes the result suitable for downstream inspection and update methods.

    call0 <- match.call(expand.dots = TRUE)

    dots <- list(...)

    if (!is.null(schema)) {
        resolve_schema_fun <- get0(
            ".resolve_external_schema",
            mode = "function",
            inherits = TRUE
        )
        if (is.null(resolve_schema_fun)) {
            stop(
                "Could not find '.resolve_external_schema()'. Load schema support first.",
                call. = FALSE
            )
        }

        sch <- resolve_schema_fun(
            schema = schema,
            levels = levels,
            keep_cols = keep_cols,
            colmap = colmap,
            d_unit = d_unit,
            h_unit = h_unit,
            metric_colmap = metric_colmap,
            volume_colmap = volume_colmap,
            metric_levels = metric_levels,
            metric_keep_cols = metric_keep_cols,
            metric_d_unit = metric_d_unit,
            metric_h_unit = metric_h_unit
        )

        levels <- sch$levels
        keep_cols <- sch$keep_cols
        colmap <- sch$colmap
        d_unit <- sch$d_unit
        h_unit <- sch$h_unit
        metric_colmap <- sch$metric_colmap
        volume_colmap <- sch$volume_colmap
        metric_levels <- sch$metric_levels
        metric_keep_cols <- sch$metric_keep_cols
        metric_d_unit <- sch$metric_d_unit
        metric_h_unit <- sch$metric_h_unit
    }

    if (!is.null(d_unit))
        metric_d_unit <- d_unit
    if (!is.null(h_unit))
        metric_h_unit <- h_unit
    metric_d_unit <- match.arg(metric_d_unit, c("mm", "cm"))
    metric_h_unit <- match.arg(metric_h_unit, c("m", "dm", "cm"))

    if (!is.null(tree_d_unit_out))
        tree_d_unit_out <- match.arg(tree_d_unit_out, c("mm", "cm"))
    if (!is.null(tree_h_unit_out))
        tree_h_unit_out <- match.arg(tree_h_unit_out, c("m", "dm", "cm"))

    metric_levels <- metric_levels %||% levels
    metric_keep_cols <- metric_keep_cols %||% keep_cols

    if (!is.null(colmap)) {
        volume_colmap <- utils::modifyList(volume_colmap, colmap)
        dh_keys <- intersect(names(colmap), c("d", "h"))
        if (length(dh_keys)) {
            metric_colmap <- utils::modifyList(metric_colmap, colmap[dh_keys])
        }
    }

    if (is.null(parametro)) {
        output_to_param <- setNames(
            toupper(names(method_registry)),
            tolower(vapply(
                method_registry,
                function(z) z$output %||% NA_character_,
                character(1)
            ))
        )
        var_low <- tolower(var %||% character(0))
        p1 <- toupper(intersect(var_low, tolower(names(method_registry))))
        p2_keys <- intersect(var_low, names(output_to_param))
        p2 <- unname(output_to_param[p2_keys])
        parametro <- unique(c(p1, p2))
        if (!length(parametro))
            parametro <- NULL
    }

    finalize_output <- function(out, call) {
        if (is.null(out))
            return(NULL)

        attr(out, "call") <- call
        class(out) <- unique(c("external_dendroMetrics", "dendroMetrics", class(out)))
        out
    }

    get_units_map <- function(x) {
        un <- attr(x, "units")
        if (is.null(un))
            return(setNames(character(0), character(0)))

        if (is.null(names(un)) || anyNA(names(un)) || any(names(un) == ""))
            stop("'attr(x, \"units\")' must be a named vector.", call. = FALSE)

        un[!duplicated(names(un))]
    }

    resolve_cols <- function(dt, cols, required = TRUE) {
        if (is.null(cols))
            return(character(0))

        cols <- as.character(cols)
        cols <- cols[!is.na(cols) & nzchar(cols)]
        if (!length(cols))
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
            } else if (isTRUE(required)) {
                stop("Could not resolve grouping column '", cl, "'.",
                     call. = FALSE)
            }
        }

        unique(out)
    }

    has_standardized_metric <- function(dt, key) {
        aliases <- switch(
            tolower(key),
            d  = c("d"),
            h  = c("h"),
            ba = c("ba"),
            n  = c("n"),
            hd = c("hd", "Hd"),
            character(0)
        )

        if (!length(aliases))
            return(FALSE)

        nml <- tolower(names(dt))
        ali <- tolower(aliases)
        ii <- match(ali, nml)
        ii <- ii[!is.na(ii)]
        if (!length(ii))
            return(FALSE)

        nm <- names(dt)[ii[1L]]
        nm %in% names(get_units_map(dt))
    }

    convert_cols_to_units <- function(dt, mapping) {
        un <- get_units_map(dt)
        if (!length(un))
            return(dt)

        mapping <- mapping[!is.na(mapping) & nzchar(mapping)]
        if (!length(mapping))
            return(dt)

        mapping <- mapping[names(mapping) %in% names(dt)]
        if (!length(mapping))
            return(dt)

        mapping <- mapping[names(mapping) %in% names(un)]
        if (!length(mapping))
            return(dt)

        conv_units(
            nfi = dt,
            var = names(mapping),
            un = unname(mapping)
        )
    }

    build_metric_var <- function(var, summ.vr, parametro, method_registry) {
        var <- unique(var %||% character(0))
        var <- var[!is.na(var) & nzchar(var)]

        volume_requested <- length(parametro %||% character(0)) > 0L
        needs_n_for_summary <- !is.null(summ.vr) && (
            length(intersect(tolower(var), c("d", "h", "hd", "ba"))) > 0L ||
            volume_requested
        )

        out <- unique(c(var, if (needs_n_for_summary) "n"))

        if ("Hd" %in% out || "hd" %in% tolower(out))
            out <- unique(c(out, "d", "h", "n"))

        out
    }

    materialize_metrics_only <- function(
        x,
        tree_var_needed,
        design,
        metric_levels,
        metric_keep_cols,
        metric_colmap,
        metric_d_unit,
        metric_h_unit,
        compute_metrics_if_needed,
        domheight_fun,
        ...
    ) {
        if (is.null(x))
            return(x)

        missing_metrics <- tree_var_needed[
            !vapply(tolower(tree_var_needed), function(k) {
                has_standardized_metric(x, k)
            }, logical(1))
        ]

        if (!length(missing_metrics) &&
            inherits(x, c("externalMetrics", "metrics2vol")))
            return(x)

        if (!isTRUE(compute_metrics_if_needed)) {
            stop(
                "Requested metric columns are missing and 'compute_metrics_if_needed = FALSE'.",
                call. = FALSE
            )
        }

        metric_fun <- get0("externalMetrics", mode = "function", inherits = TRUE)
        if (is.null(metric_fun))
            stop("Could not find 'externalMetrics()'.", call. = FALSE)

        if (is.null(design))
            stop(
                "To compute metrics from raw external data, supply 'design'.",
                call. = FALSE
            )

        keep_needed <- unique(c(metric_keep_cols, metric_levels))
        keep_needed <- keep_needed[!is.na(keep_needed) & nzchar(keep_needed)]

        do.call(
            metric_fun,
            list(
                x = x,
                var = tree_var_needed,
                levels = metric_levels,
                design = design,
                colmap = metric_colmap,
                d_unit = metric_d_unit,
                h_unit = metric_h_unit,
                keep_cols = keep_needed,
                domheight_fun = domheight_fun
            )
        )
    }

    external_one <- function(
        x,
        summ.vr,
        cut.dt,
        report,
        var,
        parametro,
        design,
        parameter_table,
        method_registry,
        metric_levels,
        metric_keep_cols,
        metric_colmap,
        metric_d_unit,
        metric_h_unit,
        tree_d_unit_out,
        tree_h_unit_out,
        volume_colmap,
        selector,
        track_provenance,
        compute_metrics_if_needed,
        domheight_fun,
        ...
    ) {
        x0 <- x
        if (is.null(x0))
            return(x)

        if (!is.data.frame(x))
            stop("'x' must be a data.frame, processed data.frame, or list thereof.",
                 call. = FALSE)

        tree_var_needed <- build_metric_var(
            var = var,
            summ.vr = summ.vr,
            parametro = parametro,
            method_registry = method_registry
        )

        effective_metric_levels <- unique(c(metric_levels, summ.vr))
        effective_metric_levels <- effective_metric_levels[
            !is.na(effective_metric_levels) & nzchar(effective_metric_levels)
        ]

        effective_metric_keep <- unique(c(
            metric_keep_cols,
            effective_metric_levels,
            unlist(
                volume_colmap[
                    intersect(names(volume_colmap), c("species", "region", "equation_set"))
                ],
                use.names = FALSE
            )
        ))
        effective_metric_keep <- effective_metric_keep[
            !is.na(effective_metric_keep) & nzchar(effective_metric_keep)
        ]

        has_param <- length(parametro %||% character(0)) > 0L

        if (has_param) {
            dt <- externalMetrics2Vol(
                x = x,
                parametro = parametro,
                parameter_table = parameter_table,
                method_registry = method_registry,
                colmap = volume_colmap,
                selector = selector,
                track_provenance = track_provenance,
                compute_metrics_if_needed = compute_metrics_if_needed,
                design = design,
                metric_var = tree_var_needed,
                metric_levels = effective_metric_levels,
                metric_keep_cols = effective_metric_keep,
                metric_colmap = metric_colmap,
                metric_d_unit = metric_d_unit,
                metric_h_unit = metric_h_unit,
                domheight_fun = domheight_fun,
                ...
            )
        } else {
            dt <- materialize_metrics_only(
                x = x,
                tree_var_needed = tree_var_needed,
                design = design,
                metric_levels = effective_metric_levels,
                metric_keep_cols = effective_metric_keep,
                metric_colmap = metric_colmap,
                metric_d_unit = metric_d_unit,
                metric_h_unit = metric_h_unit,
                compute_metrics_if_needed = compute_metrics_if_needed,
                domheight_fun = domheight_fun,
                ...
            )
        }

        design_meta <- attr(dt, "design_meta")
        volume_meta <- attr(dt, "volume_meta")

        names(dt) <- tolower(names(dt))

        frm <- attr(dt, "units")
        if (!is.null(frm)) {
            names(frm) <- tolower(names(frm))
            keep_units <- !is.na(names(frm)) & nzchar(names(frm))
            frm <- frm[keep_units]
            frm <- frm[!duplicated(names(frm))]
            frm <- frm[names(frm) %in% names(dt)]
            attr(dt, "units") <- frm
        }

        if (is.null(summ.vr)) {
            tree_target_units <- c(
                d = tree_d_unit_out,
                h = tree_h_unit_out,
                hd = tree_h_unit_out
            )
            tree_target_units <- tree_target_units[
                !is.na(tree_target_units) & nzchar(tree_target_units)
            ]

            if (length(tree_target_units))
                dt <- convert_cols_to_units(dt, tree_target_units)

            frm <- attr(dt, "units")
            dt <- subset(dt, eval(parse(text = cut.dt)))

            if (!is.null(frm))
                attr(dt, "units") <- frm[intersect(names(dt), names(frm))]
            if (!is.null(design_meta))
                attr(dt, "design_meta") <- design_meta
            if (!is.null(volume_meta))
                attr(dt, "volume_meta") <- volume_meta

            if (report)
                write.csv(dt, file = "report.csv", row.names = FALSE)

            return(dt)
        }

        provenance_cols <- grepl(
            "_(source|status|raw_unit|scale|model)$",
            names(dt),
            ignore.case = TRUE
        )
        if (any(provenance_cols))
            dt <- dt[, !provenance_cols, drop = FALSE]

        summ_cols <- resolve_cols(dt, summ.vr, required = TRUE)

        target_units <- c(
            d = "cm",
            h = "m",
            hd = "m",
            ba = "m2",
            n = "",
            v = "m3",
            vcc = "m3",
            vsc = "m3",
            iavc = "m3",
            vle = "m3"
        )
        dt <- convert_cols_to_units(dt, target_units)

        weighted_mean_vars <- intersect(c("d", "h", "hd"), names(dt))
        sum_vars <- intersect(c("ba", "n", "v", "vcc", "vsc", "iavc", "vle"),
                              names(dt))

        if (length(unique(c(weighted_mean_vars, sum_vars))) && !"n" %in% names(dt)) {
            stop(
                "Summarization requires column 'n' (trees/ha). ",
                "Provide raw data plus 'design', or pass a processed input that already contains 'n'.",
                call. = FALSE
            )
        }

        msp <- split(dt, dt[summ_cols], drop = TRUE)
        msp <- Filter("nrow", msp)

        fsum <- function(z) {
            weighted_source_vars <- intersect(c("d", "h", "hd"), names(z))
            sum_source_vars <- intersect(c("ba", "n", "v", "vcc", "vsc", "iavc", "vle"), names(z))

            z_work <- z
            scale_vars <- setdiff(unique(c(weighted_source_vars, sum_source_vars)), "n")
            if (length(scale_vars))
                z_work[, scale_vars] <- z_work[, scale_vars, drop = FALSE] * z_work[, "n"]

            summ_names <- unique(c(intersect("n", names(z_work)), weighted_source_vars, sum_source_vars))

            if (length(summ_names)) {
                sum_or_na <- function(v) {
                    if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
                }
                summ <- vapply(z_work[, summ_names, drop = FALSE], sum_or_na, numeric(1))
            } else {
                summ <- numeric(0)
            }

            keep_avg <- intersect(weighted_source_vars, names(summ))
            if (length(keep_avg) && "n" %in% names(summ) &&
                is.finite(summ["n"]) && summ["n"] > 0) {
                summ[keep_avg] <- summ[keep_avg] / summ["n"]
            }

            if (all(c("d", "n") %in% names(z))) {
                ok_dg <- !is.na(z$d) & !is.na(z$n) & is.finite(z$d) & is.finite(z$n) & z$n > 0
                if (any(ok_dg)) {
                    n_sum_dg <- sum(z$n[ok_dg], na.rm = TRUE)
                    if (is.finite(n_sum_dg) && n_sum_dg > 0) {
                        summ["dg"] <- sqrt(sum(z$n[ok_dg] * z$d[ok_dg]^2, na.rm = TRUE) / n_sum_dg)
                    } else {
                        summ["dg"] <- NA_real_
                    }
                } else {
                    summ["dg"] <- NA_real_
                }
            }

            summ <- summ[order(names(summ))]
            summ <- sapply(summ, function(v) round(v, 3))
            summ <- t(as.matrix(summ))

            non_metric <- names(z)[!names(z) %in% unique(c(weighted_source_vars, sum_source_vars))]
            non_metric <- intersect(non_metric, summ_cols)
            fcs <- z[1, non_metric, drop = FALSE]

            cbind(fcs, summ)
        }

        bind_rows_fill_local <- function(a, b) {
            if (is.null(a))
                return(data.frame(b, check.names = FALSE))
            if (is.null(b))
                return(data.frame(a, check.names = FALSE))

            a <- data.frame(a, check.names = FALSE)
            b <- data.frame(b, check.names = FALSE)

            cols <- union(names(a), names(b))
            missing_a <- setdiff(cols, names(a))
            missing_b <- setdiff(cols, names(b))

            if (length(missing_a))
                a[missing_a] <- NA
            if (length(missing_b))
                b[missing_b] <- NA

            rbind(a[, cols, drop = FALSE], b[, cols, drop = FALSE])
        }

        resm <- lapply(msp, fsum)
        resm <- Reduce(bind_rows_fill_local, resm, init = NULL)
        resm <- data.frame(resm, check.names = FALSE)
        resm <- subset(resm, eval(parse(text = cut.dt)))
        rownames(resm) <- NULL

        units_out <- c(
            d = "cm",
            h = "m",
            hd = "m",
            dg = "cm",
            ba = "m2 ha-1",
            n = "ha-1",
            v = "m3 ha-1",
            vcc = "m3 ha-1",
            vsc = "m3 ha-1",
            iavc = "m3 ha-1",
            vle = "m3 ha-1"
        )
        attr(resm, "units") <- units_out[intersect(names(resm), names(units_out))]
        if (!is.null(design_meta))
            attr(resm, "design_meta") <- design_meta
        if (!is.null(volume_meta))
            attr(resm, "volume_meta") <- volume_meta

        if (report)
            write.csv(resm, file = "report.csv", row.names = FALSE)

        resm
    }

    dots0 <- list(...)

    recycle_arg <- function(x, n, arg) {
        if (is.null(x))
            return(rep(list(NULL), n))

        if (is.data.frame(x))
            return(rep(list(x), n))

        if (inherits(x, "inventory_design"))
            return(rep(list(x), n))

        if (is.list(x)) {
            is_named <- !is.null(names(x)) && any(nzchar(names(x)))
            is_scalar_list <- is_named || inherits(x, c("inventory_design", "data.frame"))
            if (is_scalar_list)
                return(rep(list(x), n))

            if (n == 1L)
                return(list(x))

            if (length(x) == 1L)
                return(rep(x, n))

            if (length(x) != n)
                stop("'", arg, "' must have length 1 or length ", n, ".",
                     call. = FALSE)

            return(x)
        }

        rep(list(x), n)
    }

    n_inputs <- if (is.data.frame(x)) 1L else max(length(x), 1L)

    arg_names <- c(
        "design", "parameter_table", "method_registry", "metric_levels",
        "metric_keep_cols", "metric_colmap", "metric_d_unit", "metric_h_unit",
        "tree_d_unit_out", "tree_h_unit_out", "volume_colmap", "selector", "track_provenance",
        "compute_metrics_if_needed", "var", "parametro", "domheight_fun"
    )

    arg_values <- list(
        design = design,
        parameter_table = parameter_table,
        method_registry = method_registry,
        metric_levels = metric_levels,
        metric_keep_cols = metric_keep_cols,
        metric_colmap = metric_colmap,
        metric_d_unit = metric_d_unit,
        metric_h_unit = metric_h_unit,
        tree_d_unit_out = tree_d_unit_out,
        tree_h_unit_out = tree_h_unit_out,
        volume_colmap = volume_colmap,
        selector = selector,
        track_provenance = track_provenance,
        compute_metrics_if_needed = compute_metrics_if_needed,
        var = var,
        parametro = parametro,
        domheight_fun = domheight_fun
    )

    x_list <- recycle_arg(x, n_inputs, "x")
    arg_lists <- lapply(arg_names, function(nm) recycle_arg(arg_values[[nm]], n_inputs, nm))
    names(arg_lists) <- arg_names

    jobs <- lapply(seq_len(n_inputs), function(i) {
        args_i <- lapply(arg_lists, function(y) y[[i]])
        list(x = x_list[[i]], args = args_i)
    })

    run_job <- function(job) {
        tryCatch(
            do.call(
                external_one,
                c(
                    list(
                        x = job$x,
                        summ.vr = summ.vr,
                        cut.dt = cut.dt,
                        report = FALSE
                    ),
                    job$args,
                    dots0
                )
            ),
            error = function(e) {
                structure(
                    list(
                        message = conditionMessage(e),
                        x = job$x
                    ),
                    class = "external_dendroMetrics_error"
                )
            }
        )
    }

    if (length(jobs) == 1L) {
        out <- do.call(
            external_one,
            c(
                list(
                    x = jobs[[1]]$x,
                    summ.vr = summ.vr,
                    cut.dt = cut.dt,
                    report = report
                ),
                jobs[[1]]$args,
                dots0
            )
        )
        return(finalize_output(out, call0))
    }

    mc.cores <- as.integer(mc.cores)
    if (is.na(mc.cores) || mc.cores < 1L)
        mc.cores <- 1L

    use_parallel <- length(jobs) > 1L && mc.cores > 1L

    if (!use_parallel) {

        res_list <- lapply(jobs, run_job)

    } else if (.Platform$OS.type == "windows") {

        cl <- parallel::makeCluster(mc.cores)
        on.exit(parallel::stopCluster(cl), add = TRUE)

        parallel::clusterExport(
            cl = cl,
            varlist = c("jobs", "run_job", "external_one", "summ.vr", "cut.dt", "dots0"),
            envir = environment()
        )

        res_list <- parallel::parLapply(cl = cl, X = jobs, fun = run_job)

    } else {

        res_list <- parallel::mclapply(
            X = jobs,
            FUN = run_job,
            mc.cores = mc.cores
        )
    }

    errs <- vapply(res_list, inherits, logical(1), what = "external_dendroMetrics_error")

    if (any(errs)) {
        msg <- vapply(res_list[errs], function(z) {
            paste0(
                "external_dendroMetrics failed: ",
                z$message
            )
        }, character(1))

        stop(paste(msg, collapse = "\n"), call. = FALSE)
    }

    res_list <- Filter(Negate(is.null), res_list)
    if (!length(res_list))
        return(NULL)

    bind_rows_fill <- function(a, b) {
        if (is.null(a))
            return(b)
        if (is.null(b))
            return(a)

        a <- data.frame(a, check.names = FALSE)
        b <- data.frame(b, check.names = FALSE)

        cols <- union(names(a), names(b))
        missing_a <- setdiff(cols, names(a))
        missing_b <- setdiff(cols, names(b))

        if (length(missing_a))
            a[missing_a] <- NA
        if (length(missing_b))
            b[missing_b] <- NA

        rbind(a[, cols, drop = FALSE], b[, cols, drop = FALSE])
    }

    combined <- Reduce(bind_rows_fill, res_list, init = NULL)
    combined <- data.frame(combined, check.names = FALSE)

    if (report)
        write.csv(combined, file = "report.csv", row.names = FALSE)

    finalize_output(combined, call0)
},
class = c("function", "basifoR"))

attr(external_dendroMetrics, "ex") <- function() {
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

    external_dendroMetrics(
        x = x,
        summ.vr = "plot",
        var = c("d", "h", "ba", "n"),
        design = sq_0.1ha,
        metric_colmap = list(d = "diameter_mm", h = "height_m"),
        metric_d_unit = "mm",
        metric_h_unit = "m"
    )
}

## S3 plot methods for basifoR spatial objects
## Version: v9 default-derived-variables NA-safe with hidden cex text scaling
## ------------------------------------------------------------
## These methods use ggplot2 when available and fall back to base
## graphics when ggplot2 is not installed. The plot methods use
## the standard S3 signature plot.<class>(x, y, ...), where y can
## optionally provide the variables to plot. They draw one panel per
## selected numeric variable. Point size represents variable-specific
## quantile classes based on P5, P25, P50, P75, and P95. Quantile
## classes are computed from the finite, non-missing values of each
## selected variable in the object supplied to plot(). Thus,
## readNFI_spatial objects are classified from tree-record values,
## whereas inventoryMetrics_spatial objects summarized with summ.vr
## are classified from plot- or summary-level metric values. Quantiles
## are unweighted; expansion factors are not used by the plot method.
## A hidden cex argument can be passed through ... to scale text in paper figures.
##
## Important S3 note:
##   For plain plot(x) to call plot.inventoryMetrics_spatial() rather
##   than plot.sf(), spatial basifoR classes must be before "sf" in
##   class(x). The .spatial_inherit_class() helper below should replace
##   the previous helper used by readNFI_spatial(), nfiMetrics_spatial(),
##   metrics2Vol_spatial(), and inventoryMetrics_spatial().

## Avoid R CMD check notes from ggplot2 facet/aesthetic variables.
if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(".plot_q", ".plot_variable", ".plot_value"))
}

.spatial_inherit_class <- function(x, spatial_class, old_class = NULL) {
    cls <- class(x)
    cls <- setdiff(cls, c(spatial_class, old_class))

    ## Put the basifoR spatial class before sf so plot(x) dispatches to
    ## plot.<basifoR_spatial_class>(), while keeping sf in the class vector
    ## so sf::st_* and ggplot2::geom_sf() still work.
    if (inherits(x, "sf")) {
        class(x) <- unique(c(
            spatial_class,
            old_class,
            "sf",
            setdiff(cls, "sf")
        ))
    } else {
        class(x) <- unique(c(
            spatial_class,
            old_class,
            cls
        ))
    }

    x
}

# Restore the class order required for spatial NFI plot dispatch.
fixNFIspatial_plot_dispatch <- function(
    x # Spatial basifoR object whose class order should be restored so its package-specific plot method dispatches before plot.sf.
) {
    cls <- class(x)
    spatial_priority <- c(
        "inventoryMetrics_spatial",
        "metrics2Vol_spatial",
        "nfiMetrics_spatial",
        "readNFI_spatial"
    )
    old_priority <- c("inventoryMetrics", "metrics2Vol", "nfiMetrics", "readNFI")

    sp <- spatial_priority[spatial_priority %in% cls]
    old <- old_priority[old_priority %in% cls]

    if (!length(sp))
        return(x)

    if (inherits(x, "sf")) {
        class(x) <- unique(c(
            sp,
            old,
            "sf",
            setdiff(cls, c(sp, old, "sf"))
        ))
    } else {
        class(x) <- unique(c(
            sp,
            old,
            setdiff(cls, c(sp, old))
        ))
    }

    x
}

.nfisp_require_sf <- function() {
    if (!requireNamespace("sf", quietly = TRUE))
        stop("Package 'sf' is required for plotting spatial NFI objects.", call. = FALSE)
    invisible(TRUE)
}

.nfisp_has_ggplot2 <- function() {
    requireNamespace("ggplot2", quietly = TRUE)
}

.nfisp_require_ggplot2 <- function() {
    if (!.nfisp_has_ggplot2())
        stop(
            "Package 'ggplot2' is required for engine = 'ggplot2'. ",
            "Use engine = 'base' or install ggplot2.",
            call. = FALSE
        )
    invisible(TRUE)
}

.nfisp_is_sf <- function(x) inherits(x, "sf")

.nfisp_extra_attrs <- function(x) {
    at <- attributes(x)
    at[setdiff(names(at), c("names", "row.names", "class", "sf_column", "agr"))]
}

.nfisp_restore_extra_attrs <- function(x, attrs) {
    protected <- c("names", "row.names", "class", "sf_column", "agr")
    for (nm in setdiff(names(attrs), protected))
        attr(x, nm) <- attrs[[nm]]
    x
}

.nfisp_as_sf <- function(x) {
    .nfisp_require_sf()

    if (.nfisp_is_sf(x))
        return(fixNFIspatial_plot_dispatch(x))

    if (exists("asNFI_spatial_sf", mode = "function", inherits = TRUE)) {
        attrs <- .nfisp_extra_attrs(x)
        old_class <- class(x)
        y <- asNFI_spatial_sf(x)
        y <- .nfisp_restore_extra_attrs(y, attrs)

        ## asNFI_spatial_sf() may legitimately rebuild the object with the
        ## reader class (readNFI_spatial).  For metric outputs, restore the
        ## most advanced spatial class before plotting so plot(x) and titles
        ## reflect the last metric wrapper actually used.
        class(y) <- unique(c(old_class, class(y)))
        y <- fixNFIspatial_plot_dispatch(y)
        return(y)
    }

    stop(
        "Object is not sf and asNFI_spatial_sf() is not available.",
        call. = FALSE
    )
}

.nfisp_boundary_object <- function(x, boundary = NULL) {
    ## boundary = NULL means "auto": draw the boundary only when a
    ## boundary sidecar already exists. This avoids requiring users to
    ## remember whether boundary = TRUE was used upstream.
    if (identical(boundary, FALSE))
        return(NULL)

    if (inherits(boundary, "sf"))
        return(boundary)

    if (is.list(boundary) && inherits(boundary$geometry, "sf"))
        return(boundary$geometry)

    if (is.null(boundary) || isTRUE(boundary)) {
        b <- NULL
        if (exists("getNFIboundary_spatial", mode = "function", inherits = TRUE))
            b <- tryCatch(getNFIboundary_spatial(x), error = function(e) NULL)
        if (is.null(b))
            b <- attr(x, "nfi_boundary", exact = TRUE)
        if (is.list(b) && inherits(b$geometry, "sf"))
            return(b$geometry)
        if (inherits(b, "sf"))
            return(b)

        if (isTRUE(boundary) &&
            isTRUE(attr(x, "nfi_boundary_failed", exact = TRUE))) {
            warning(
                paste(
                    "The object records a failed automatic boundary download.",
                    "plot() will not retry the GADM download. Recreate the",
                    "object after pre-caching the boundary, or pass an sf",
                    "boundary object directly to 'boundary'."
                ),
                call. = FALSE
            )
            return(NULL)
        }

        ## When the user explicitly asks for a boundary while plotting an
        ## older object that lacks the boundary sidecar, try the same internal
        ## GADM builder used by readNFI_spatial().  The default boundary = NULL
        ## remains conservative and only draws a sidecar that already exists.
        if (isTRUE(boundary) &&
            exists(".basifoR_spatial_make_boundary_safe", mode = "function", inherits = TRUE) &&
            exists(".basifoR_spatial_boundary_context", mode = "function", inherits = TRUE)) {
            external_input <- identical(
                attr(x, "backend", exact = TRUE),
                "external"
            ) || inherits(
                x,
                c(
                    "external_nfi",
                    "external_nfiMetrics",
                    "external_metrics2vol",
                    "external_dendroMetrics"
                )
            )
            reg <- attr(x, "nfi_geometry_registry", exact = TRUE)
            ctx <- tryCatch(
                .basifoR_spatial_boundary_context(data = x, registry = reg),
                error = function(e) NULL
            )
            if (!is.null(ctx)) {
                b <- tryCatch(
                    .basifoR_spatial_make_boundary_safe(
                        nfi = ctx,
                        boundary = TRUE,
                        allow.gadm = !external_input
                    ),
                    error = function(e) NULL
                )
                if (is.list(b) && inherits(b$geometry, "sf"))
                    return(b$geometry)
                if (inherits(b, "sf"))
                    return(b)
            }
        }
    }

    NULL
}

.nfisp_make_valid_quiet <- function(x) {
    if (!inherits(x, "sf"))
        return(x)

    out <- tryCatch(sf::st_make_valid(x), error = function(e) x)
    out
}

.nfisp_transform_boundary <- function(boundary, target) {
    if (is.null(boundary) || !inherits(boundary, "sf"))
        return(NULL)

    boundary <- .nfisp_make_valid_quiet(boundary)

    crs_target <- sf::st_crs(target)
    crs_boundary <- sf::st_crs(boundary)

    if (is.na(crs_target)) {
        warning(
            "The plotted geometry has no CRS, so its boundary cannot be aligned reliably.",
            call. = FALSE
        )
        return(NULL)
    }

    if (is.na(crs_boundary)) {
        warning(
            "The boundary has no CRS and cannot be aligned with the plotted geometry.",
            call. = FALSE
        )
        return(NULL)
    }

    if (!is.na(crs_target) && !is.na(crs_boundary) && crs_target != crs_boundary) {
        boundary <- tryCatch(
            sf::st_transform(boundary, crs_target),
            error = function(e) {
                warning(
                    "Could not transform the boundary to the plotted CRS: ",
                    conditionMessage(e),
                    call. = FALSE
                )
                NULL
            }
        )
    }

    boundary
}

.nfisp_first_col <- function(x, candidates) {
    nm <- names(x)
    hit <- match(tolower(candidates), tolower(nm))
    hit <- hit[!is.na(hit)]
    if (!length(hit))
        return(NA_character_)
    nm[hit[1L]]
}

.nfisp_active_spatial_class <- function(x) {
    priority <- c(
        "inventoryMetrics_spatial",
        "metrics2Vol_spatial",
        "nfiMetrics_spatial",
        "readNFI_spatial"
    )
    hit <- priority[priority %in% class(x)]
    if (length(hit)) hit[1L] else NA_character_
}


.nfisp_has_usable_numeric <- function(z) {
    is.numeric(z) && any(is.finite(z) & !is.na(z))
}

.nfisp_metric_candidates <- function(x) {
    nm <- names(x)
    geom_col <- attr(x, "sf_column", exact = TRUE)
    nm <- setdiff(nm, geom_col)

    units <- attr(x, "units", exact = TRUE)
    if (!is.null(units) && length(units)) {
        cand <- intersect(names(units), nm)
        cand <- cand[vapply(x[cand], .nfisp_has_usable_numeric, logical(1))]
        if (length(cand))
            return(cand)
    }

    ## Class-specific fallbacks for objects produced by the spatial metric
    ## wrappers. These are the variables normally derived by each stage.
    cls <- class(x)
    if (any(cls %in% c("inventoryMetrics_spatial", "metrics2Vol_spatial"))) {
        preferred <- c(
            "d", "h", "ba", "n_tot", "n", "Hd",
            "v", "vcc", "vsc", "iavu", "biomasa", "carbono"
        )
    } else if (any(cls %in% "nfiMetrics_spatial")) {
        preferred <- c("d", "h", "ba", "n", "Hd")
    } else {
        preferred <- character(0)
    }

    if (length(preferred)) {
        idx <- match(tolower(preferred), tolower(nm))
        cand <- nm[idx[!is.na(idx)]]
        cand <- cand[vapply(x[cand], .nfisp_has_usable_numeric, logical(1))]
        if (length(cand))
            return(unique(cand))
    }

    character(0)
}

.nfisp_default_vars <- function(x, vars = NULL) {
    nm <- names(x)
    geom_col <- attr(x, "sf_column", exact = TRUE)
    nm <- setdiff(nm, geom_col)

    if (!is.null(vars) && length(vars)) {
        idx <- match(tolower(vars), tolower(nm))
        ok <- !is.na(idx)
        if (any(!ok)) {
            warning(
                "Variable(s) not found and omitted from the plot: ",
                paste(vars[!ok], collapse = ", "),
                call. = FALSE
            )
        }
        if (!any(ok)) {
            stop(
                "None of the requested variables were found in the spatial object.",
                call. = FALSE
            )
        }
        cand <- nm[idx[ok]]
        cand <- cand[vapply(x[cand], .nfisp_has_usable_numeric, logical(1))]
        if (!length(cand)) {
            stop(
                "Requested variables were found, but none are numeric.",
                call. = FALSE
            )
        }
        return(cand)
    }

    ## Preferred default: variables actually produced by the metric stage.
    ## This usually follows attr(x, "units"), because metric functions set
    ## units for derived outputs such as d, h, ba, n, n_tot, Hd, and volume columns.
    cand <- .nfisp_metric_candidates(x)
    if (length(cand))
        return(cand)

    ## Fallback for raw spatial readers. Avoid obvious identifiers and
    ## field/navigation variables so plot.readNFI_spatial() does not produce
    ## a page of administrative codes by default.
    id_like <- c(
        "nfi.nr", "nfi_nr", "pr", "provincia", "nprov", "prov",
        "estadillo", "numpar", "plot", "plot_id", "idp",
        "campagne", "cla", "subclase", "narbol", "arbol", "a",
        "ordenif3", "ordenif4", "rumbo", "distanci", "distancia",
        "especie", "espar", "huso", "source_epsg", "target_epsg"
    )
    cand <- nm[vapply(x[nm], .nfisp_has_usable_numeric, logical(1))]
    cand <- cand[!tolower(cand) %in% id_like]

    if (!length(cand)) {
        stop(
            "No derived numeric variables were detected for plotting. Supply 'vars'.",
            call. = FALSE
        )
    }

    cand
}

.nfisp_quantile_class <- function(z, probs = c(0.05, 0.25, 0.50, 0.75, 0.95)) {
    labels <- c("<=P5", "P5-P25", "P25-P50", "P50-P75", "P75-P95", ">P95")
    out <- rep(NA_character_, length(z))
    ok <- !is.na(z)

    if (!any(ok))
        return(factor(out, levels = labels))

    q <- stats::quantile(z[ok], probs = probs, na.rm = TRUE, names = FALSE, type = 7)

    out[ok & z <= q[1L]] <- labels[1L]
    out[ok & z > q[1L] & z <= q[2L]] <- labels[2L]
    out[ok & z > q[2L] & z <= q[3L]] <- labels[3L]
    out[ok & z > q[3L] & z <= q[4L]] <- labels[4L]
    out[ok & z > q[4L] & z <= q[5L]] <- labels[5L]
    out[ok & z > q[5L]] <- labels[6L]

    factor(out, levels = labels)
}

.nfisp_var_label <- function(x, v) {
    units <- attr(x, "units", exact = TRUE)
    if (!is.null(units) && v %in% names(units) && !is.na(units[[v]]) && nzchar(units[[v]]))
        return(paste0(v, " [", units[[v]], "]"))
    v
}

.nfisp_long_sf <- function(x, vars) {
    geom <- sf::st_geometry(x)
    pieces <- lapply(vars, function(v) {
        sf::st_sf(
            data.frame(
                .plot_variable = .nfisp_var_label(x, v),
                .plot_value = x[[v]],
                .plot_q = .nfisp_quantile_class(x[[v]]),
                stringsAsFactors = FALSE
            ),
            geometry = geom
        )
    })

    out <- do.call(rbind, pieces)

    ## ggplot2 treats NA values in mapped aesthetics as an additional legend
    ## key. That can make guide override vectors longer/shorter than the
    ## quantile size vector. For metric maps, rows with missing metric values
    ## should not be plotted, so remove them before building the panel.
    keep <- !is.na(out$.plot_value) & is.finite(out$.plot_value) & !is.na(out$.plot_q)
    out <- out[keep, , drop = FALSE]

    if (!nrow(out))
        stop("Selected variables contain only missing or non-finite values.", call. = FALSE)

    out$.plot_variable <- factor(
        out$.plot_variable,
        levels = vapply(vars, .nfisp_var_label, character(1), x = x)
    )
    out
}

.nfisp_plot_ggplot2 <- function(x,
                               vars = NULL,
                               boundary = NULL,
                               point.sizes = c(0.6, 1.0, 1.5, 2.1, 2.9, 3.8),
                               point.alpha = 0.75,
                               ncol = NULL,
                               title = NULL,
                               subtitle = NULL,
                               legend.position = "bottom",
                               cex = 1,
                               grid.col = "grey92",
                               point.border.col = "grey35",
                               boundary.col = "grey45",
                               boundary.lwd = 0.7,
                               ...) {
    .nfisp_require_sf()
    .nfisp_require_ggplot2()

    cex <- as.numeric(cex)[1L]
    if (is.na(cex) || cex <= 0)
        cex <- 1

    x <- .nfisp_as_sf(x)
    vars <- .nfisp_default_vars(x, vars = vars)

    long <- .nfisp_long_sf(x, vars)
    boundary_obj <- .nfisp_boundary_object(x, boundary = boundary)
    boundary_obj <- .nfisp_transform_boundary(boundary_obj, x)

    size_levels <- levels(long$.plot_q)
    point.sizes <- rep(point.sizes, length.out = length(size_levels))
    names(point.sizes) <- size_levels
    point.fills <- grDevices::hcl.colors(length(size_levels), "Viridis")
    names(point.fills) <- size_levels

    if (is.null(title)) {
        cls <- .nfisp_active_spatial_class(x)
        title <- if (!is.na(cls)) cls else "Spatial NFI variables"
    }

    if (is.null(subtitle)) {
        subtitle <- paste(
            "Point size classes use unweighted variable-specific P5, P25,",
            "P50, P75, and P95 thresholds from the plotted object."
        )
    }

    p <- ggplot2::ggplot()

    if (!is.null(boundary_obj)) {
        p <- p + ggplot2::geom_sf(
            data = boundary_obj,
            inherit.aes = FALSE,
            fill = NA,
            colour = boundary.col,
            linewidth = boundary.lwd
        )
    }

    p <- p + ggplot2::geom_sf(
        data = long,
        ggplot2::aes(size = .plot_q, fill = .plot_q),
        inherit.aes = FALSE,
        shape = 21,
        colour = point.border.col,
        alpha = point.alpha,
        stroke = 0.15
    )

    ## Draw the boundary again on top of points. Thin underlaid lines can
    ## disappear on some Windows/RStudio graphics devices.
    if (!is.null(boundary_obj)) {
        p <- p + ggplot2::geom_sf(
            data = boundary_obj,
            inherit.aes = FALSE,
            fill = NA,
            colour = boundary.col,
            linewidth = boundary.lwd
        )
    }

    p <- p + ggplot2::facet_wrap(stats::as.formula("~ .plot_variable"), ncol = ncol)

    p <- p + ggplot2::scale_size_manual(
        values = point.sizes,
        breaks = size_levels,
        limits = size_levels,
        drop = FALSE,
        na.translate = FALSE,
        name = "Quantile class"
    )

    p <- p + ggplot2::scale_fill_manual(
        values = point.fills,
        breaks = size_levels,
        limits = size_levels,
        drop = FALSE,
        na.translate = FALSE,
        name = "Quantile class"
    )

    p <- p + ggplot2::guides(
        fill = ggplot2::guide_legend(
            override.aes = list(size = unname(point.sizes), alpha = 1)
        ),
        size = "none"
    )

    p <- p + ggplot2::coord_sf()

    p <- p + ggplot2::labs(
        title = title,
        subtitle = subtitle,
        x = NULL,
        y = NULL
    )

    p <- p + ggplot2::theme_bw(base_size = 9 * cex)
    p <- p + ggplot2::theme(
        legend.position = legend.position,
        panel.grid.major = ggplot2::element_line(linewidth = 0.15, colour = grid.col),
        panel.grid.minor = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "grey90", colour = "grey70"),
        strip.text = ggplot2::element_text(face = "bold", size = 9 * cex),
        axis.text = ggplot2::element_text(size = 8 * cex),
        axis.title = ggplot2::element_text(size = 9 * cex),
        legend.title = ggplot2::element_text(size = 9 * cex),
        legend.text = ggplot2::element_text(size = 8 * cex),
        plot.title = ggplot2::element_text(face = "bold", size = 11 * cex),
        plot.subtitle = ggplot2::element_text(size = 9 * cex)
    )

    p
}


.nfisp_layout_dims <- function(n, ncol = NULL) {
    if (is.null(ncol))
        ncol <- ceiling(sqrt(n))
    ncol <- max(1L, as.integer(ncol))
    nrow <- ceiling(n / ncol)
    c(nrow = nrow, ncol = ncol)
}

.nfisp_base_title <- function(x, title = NULL, subtitle = NULL) {
    if (!is.null(title))
        return(title)
    cls <- .nfisp_active_spatial_class(x)
    if (!is.na(cls)) cls else "Spatial NFI variables"
}

.nfisp_cex_values <- function(point.sizes, n) {
    point.sizes <- rep(point.sizes, length.out = n)
    ## ggplot point-size values are slightly large for base graphics.
    pmax(0.4, point.sizes * 0.65)
}

.nfisp_plot_base <- function(x,
                             vars = NULL,
                             boundary = NULL,
                             point.sizes = c(0.6, 1.0, 1.5, 2.1, 2.9, 3.8),
                             point.alpha = 0.75,
                             ncol = NULL,
                             title = NULL,
                             subtitle = NULL,
                             legend.position = "bottomleft",
                             legend = TRUE,
                             cex = 1,
                             grid.col = "grey92",
                             point.border.col = "grey35",
                             boundary.col = "grey45",
                             boundary.lwd = 0.9,
                             ...) {
    .nfisp_require_sf()

    text.cex <- as.numeric(cex)[1L]
    if (is.na(text.cex) || text.cex <= 0)
        text.cex <- 1

    x <- .nfisp_as_sf(x)
    vars <- .nfisp_default_vars(x, vars = vars)

    boundary_obj <- .nfisp_boundary_object(x, boundary = boundary)
    boundary_obj <- .nfisp_transform_boundary(boundary_obj, x)

    size_levels <- levels(.nfisp_quantile_class(rep(1, 2)))
    cex_values <- .nfisp_cex_values(point.sizes, length(size_levels))
    names(cex_values) <- size_levels

    dims <- .nfisp_layout_dims(length(vars), ncol = ncol)
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::par(
        mfrow = dims,
        mar = c(2.2, 2.2, 2.8, 0.8),
        oma = c(0, 0, if (is.null(title) && is.null(subtitle)) 0 else 2.5, 0),
        cex.axis = 0.8 * text.cex,
        cex.lab = 0.9 * text.cex,
        cex.main = 1.0 * text.cex
    )

    geom_x <- sf::st_geometry(x)
    border_col <- boundary.col
    point_border_col <- point.border.col
    qcols <- grDevices::hcl.colors(length(size_levels), "Viridis")
    names(qcols) <- size_levels
    qcols <- grDevices::adjustcolor(qcols, alpha.f = point.alpha)

    for (i in seq_along(vars)) {
        v <- vars[i]
        z <- x[[v]]
        q <- .nfisp_quantile_class(z)
        point_cex <- cex_values[as.character(q)]
        point_cex[is.na(point_cex)] <- min(cex_values, na.rm = TRUE)
        point_col <- qcols[as.character(q)]
        point_col[is.na(point_col)] <- grDevices::adjustcolor("grey60", alpha.f = point.alpha)

        if (!is.null(boundary_obj)) {
            plot(
                sf::st_geometry(boundary_obj),
                border = border_col,
                col = NA,
                axes = TRUE,
                main = .nfisp_var_label(x, v),
                lwd = boundary.lwd,
                ...
            )
            graphics::grid(col = grid.col, lty = "solid")
            plot(
                geom_x,
                add = TRUE,
                pch = 21,
                bg = point_col,
                col = point_border_col,
                cex = point_cex,
                lwd = 0.35
            )
            plot(
                sf::st_geometry(boundary_obj),
                add = TRUE,
                border = border_col,
                col = NA,
                lwd = boundary.lwd
            )
        } else {
            plot(
                geom_x,
                pch = 21,
                bg = NA,
                col = NA,
                axes = TRUE,
                main = .nfisp_var_label(x, v),
                lwd = 0.2,
                ...
            )
            graphics::grid(col = grid.col, lty = "solid")
            plot(
                geom_x,
                add = TRUE,
                pch = 21,
                bg = point_col,
                col = point_border_col,
                cex = point_cex,
                lwd = 0.35
            )
        }

        if (isTRUE(legend) && i == 1L) {
            graphics::legend(
                legend.position,
                legend = size_levels,
                pt.cex = cex_values,
                pch = 21,
                pt.bg = qcols,
                col = point_border_col,
                title = "Quantile class",
                bty = "n",
                cex = 0.75 * text.cex,
                y.intersp = 1.1
            )
        }
    }

    main_title <- .nfisp_base_title(x, title = title, subtitle = subtitle)
    if (!is.null(title) || !is.null(subtitle)) {
        graphics::mtext(main_title, side = 3, outer = TRUE, line = 1.0, font = 2, cex = text.cex)
        if (!is.null(subtitle))
            graphics::mtext(subtitle, side = 3, outer = TRUE, line = 0.0, cex = 0.75 * text.cex)
    }

    invisible(x)
}

.nfisp_plot_engine <- function(x,
                               vars = NULL,
                               boundary = NULL,
                               engine = c("auto", "ggplot2", "base"),
                               ...) {
    engine <- match.arg(engine)

    if (engine == "auto")
        engine <- if (.nfisp_has_ggplot2()) "ggplot2" else "base"

    if (engine == "ggplot2") {
        p <- .nfisp_plot_ggplot2(x, vars = vars, boundary = boundary, ...)
        print(p)
        return(invisible(p))
    }

    .nfisp_plot_base(x, vars = vars, boundary = boundary, ...)
}

.nfisp_plot_method <- function(x, y = NULL, ...) {
    ## Standard S3 plot-method interface. Users may pass variables either
    ## as y, e.g. plot(x, c("ba", "n_tot")), or as vars in ....
    dots <- list(...)

    if (!is.null(y)) {
        if (!(is.character(y) || is.numeric(y))) {
            stop(
                "For spatial NFI plot methods, 'y' must be a character vector of variable names ",
                "or numeric column positions. Alternatively, use vars = ... .",
                call. = FALSE
            )
        }
        if (is.numeric(y)) {
            nm <- names(x)
            y <- nm[y]
        }
        if (!is.null(dots$vars)) {
            warning(
                "Both 'y' and 'vars' were supplied; using 'y' as the variable selection.",
                call. = FALSE
            )
        }
        dots$vars <- y
    }

    do.call(.nfisp_plot_engine, c(list(x = x), dots))
}

### Plot a spatial readNFI object.
plot.readNFI_spatial <- function(
    x, ##<< A spatial \code{readNFI_spatial} object to plot.
    y = NULL, ##<< Optional character vector of variable names or numeric vector of column positions to plot; \code{NULL} uses the default numeric variables.
    ... ##<< Additional arguments passed to the internal spatial plotting engine, including options such as \code{vars}, \code{boundary}, and \code{engine}.
) {
    .nfisp_plot_method(x, y = y, ...)
}

### Plot a spatial nfiMetrics object.
plot.nfiMetrics_spatial <- function(
    x, ##<< A spatial \code{nfiMetrics_spatial} object to plot.
    y = NULL, ##<< Optional character vector of variable names or numeric vector of column positions to plot; \code{NULL} uses the default numeric variables.
    ... ##<< Additional arguments passed to the internal spatial plotting engine, including options such as \code{vars}, \code{boundary}, and \code{engine}.
) {
    .nfisp_plot_method(x, y = y, ...)
}

### Plot a spatial metrics2Vol object.
plot.metrics2Vol_spatial <- function(
    x, ##<< A spatial \code{metrics2Vol_spatial} object to plot.
    y = NULL, ##<< Optional character vector of variable names or numeric vector of column positions to plot; \code{NULL} uses the default numeric variables.
    ... ##<< Additional arguments passed to the internal spatial plotting engine, including options such as \code{vars}, \code{boundary}, and \code{engine}.
) {
    .nfisp_plot_method(x, y = y, ...)
}

### Plot a spatial inventoryMetrics object.
plot.inventoryMetrics_spatial <- function(
    x, ##<< A spatial \code{inventoryMetrics_spatial} object to plot.
    y = NULL, ##<< Optional character vector of variable names or numeric vector of column positions to plot; \code{NULL} uses the default numeric variables.
    ... ##<< Additional arguments passed to the internal spatial plotting engine, including options such as \code{vars}, \code{boundary}, and \code{engine}.
) {
    .nfisp_plot_method(x, y = y, ...)
}

## Extracted from basifoR_spatial_functions_v14_plotdispatch.R
## Source lines 3484-4393.
## Source readNFI_spatial_v15_coord_qa.R before this file so shared
## sidecar, GADM, and sf-reconstruction helpers are available.

## Spatial sidecar wrapper for inventoryMetrics()
## ------------------------------------------------------------
## This file does not replace inventoryMetrics().  It defines a parallel
## function, inventoryMetrics_spatial(), that keeps the original tabular
## calculation engines and carries plot geometry as an attribute sidecar.
## Source after readNFI_spatial_v6.R, nfiMetrics_spatial_v5_boundaries.R, and
## metrics2Vol_spatial_v3_boundaries.R while testing.
## v4: passes huso.candidates to readNFI_spatial_v6() so mixed-Huso
##     IFN2 provinces such as 49 can use the CoorX rule before province
##     fallback.

.invsp_first_col <- function(x, candidates) {
    nm <- names(x)
    hit <- match(tolower(candidates), tolower(nm))
    hit <- hit[!is.na(hit)]
    if (!length(hit)) return(NA_character_)
    nm[hit[1L]]
}

.invsp_numeric <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) x <- gsub(",", ".", trimws(x), fixed = TRUE)
    suppressWarnings(as.numeric(x))
}

.invsp_key_part <- function(x) {
    x0 <- x
    x <- trimws(as.character(x))
    xn <- .invsp_numeric(x)
    ok <- !is.na(xn)
    out <- x
    out[ok] <- format(xn[ok], scientific = FALSE, trim = TRUE)
    out[is.na(x0)] <- NA_character_
    out
}

.invsp_plot_cols <- function(x, required = TRUE) {
    pr_col <- .invsp_first_col(
        x,
        c("pr", "Provincia", "PROVINCIA", "NPROV", "nprov", "prov")
    )
    plot_col <- .invsp_first_col(
        x,
        c("Estadillo", "ESTADILLO", "estadillo", "NUMPAR", "numpar",
          "plot", "plot_id", "idp", "sample_id", "site_id")
    )

    if (required && is.na(plot_col)) {
        stop(
            paste(
                "Cannot attach spatial output because no plot identifier",
                "was found. Expected a column such as 'Estadillo',",
                "'NUMPAR', 'plot', 'plot_id', 'sample_id', or 'site_id'."
            ),
            call. = FALSE
        )
    }

    list(pr = pr_col, plot = plot_col)
}

.invsp_has_plot_col <- function(x) {
    is.data.frame(x) && !is.na(.invsp_plot_cols(x, required = FALSE)$plot)
}

.invsp_plot_key <- function(x, cols = .invsp_plot_cols(x), use_pr = TRUE) {
    if (is.na(cols$plot))
        stop("Plot identifier not found while building spatial keys.", call. = FALSE)

    plot <- .invsp_key_part(x[[cols$plot]])
    has_pr <- !is.na(cols$pr) && cols$pr %in% names(x)

    if (isTRUE(use_pr) && has_pr)
        paste(.invsp_key_part(x[[cols$pr]]), plot, sep = "\r")
    else
        plot
}

.invsp_resolve_cols <- function(x, requested, required = TRUE) {
    if (is.null(requested) || !length(requested))
        return(character(0))

    nm <- names(x)
    idx <- match(tolower(requested), tolower(nm))
    ok <- !is.na(idx)

    if (required && any(!ok)) {
        stop(
            "Column(s) not found while restoring spatial output: ",
            paste(requested[!ok], collapse = ", "),
            call. = FALSE
        )
    }

    nm[idx[ok]]
}

.invsp_group_key <- function(x, cols) {
    if (!length(cols))
        return(rep("", nrow(x)))

    z <- lapply(cols, function(nm) .invsp_key_part(x[[nm]]))
    do.call(paste, c(z, sep = "\r"))
}

.invsp_extra_attrs <- function(x) {
    at <- attributes(x)
    at[setdiff(names(at), c("names", "row.names", "class", "sf_column", "agr"))]
}

.invsp_restore_extra_attrs <- function(x, attrs) {
    protected <- c("names", "row.names", "class", "sf_column", "agr")
    for (nm in setdiff(names(attrs), protected))
        attr(x, nm) <- attrs[[nm]]
    x
}

.invsp_is_sf <- function(x) inherits(x, "sf")

.invsp_any_sf <- function(x) {
    if (.invsp_is_sf(x)) return(TRUE)
    if (is.list(x) && !is.data.frame(x))
        return(any(vapply(x, .invsp_any_sf, logical(1))))
    FALSE
}

.invsp_drop_geometry_keep_attrs <- function(x) {
    if (.invsp_is_sf(x)) {
        if (!requireNamespace("sf", quietly = TRUE))
            stop("Package 'sf' is required to drop geometry safely.", call. = FALSE)
        attrs <- .invsp_extra_attrs(x)
        y <- sf::st_drop_geometry(x)
        y <- .invsp_restore_extra_attrs(y, attrs)
        class(y) <- unique(c(setdiff(class(y), c("sf", "sfc", "tbl_df", "tbl")),
                             "data.frame"))
        return(y)
    }

    if (is.list(x) && !is.data.frame(x))
        return(lapply(x, .invsp_drop_geometry_keep_attrs))

    x
}

.invsp_normalize_spatial <- function(spatial) {
    if (is.logical(spatial))
        return(if (isTRUE(spatial)) "sf" else "none")
    match.arg(spatial, c("attribute", "sf", "none", "inherit"))
}

.invsp_has_sidecar <- function(x) {
    exists("hasNFIgeometry_spatial", mode = "function", inherits = TRUE) &&
        isTRUE(hasNFIgeometry_spatial(x))
}

.invsp_has_boundary <- function(x) {
    if (exists("hasNFIboundary_spatial", mode = "function", inherits = TRUE))
        return(isTRUE(hasNFIboundary_spatial(x)))

    b <- attr(x, "nfi_boundary", exact = TRUE)
    !is.null(b) && is.list(b) && inherits(b$geometry, "sf")
}

.invsp_get_sidecar <- function(x) {
    if (!exists("getNFIgeometry_spatial", mode = "function", inherits = TRUE))
        return(NULL)
    getNFIgeometry_spatial(x)
}

.invsp_copy_sidecar <- function(from, to) {
    if (exists("copyNFIspatial_sidecars", mode = "function", inherits = TRUE))
        return(copyNFIspatial_sidecars(from, to))
    if (exists("copyNFIgeometry_spatial", mode = "function", inherits = TRUE))
        return(copyNFIgeometry_spatial(from, to))

    reg <- attr(from, "nfi_geometry_registry", exact = TRUE)
    if (!is.null(reg)) {
        attr(to, "nfi_geometry_registry") <- reg
        attr(to, "has_nfi_geometry") <- TRUE
    }

    b <- attr(from, "nfi_boundary", exact = TRUE)
    if (!is.null(b)) {
        attr(to, "nfi_boundary") <- b
        attr(to, "has_nfi_boundary") <- TRUE
    }

    br <- attr(from, "boundary_requested", exact = TRUE)
    if (!is.null(br))
        attr(to, "boundary_requested") <- br

    bf <- attr(from, "nfi_boundary_failed", exact = TRUE)
    if (!is.null(bf))
        attr(to, "nfi_boundary_failed") <- bf

    to
}

.invsp_resolve_mode <- function(spatial, nfi) {
    spatial <- .invsp_normalize_spatial(spatial)
    if (spatial != "inherit") return(spatial)
    if (.invsp_any_sf(nfi)) return("sf")
    if (.invsp_has_sidecar(nfi)) return("attribute")
    "none"
}

.invsp_schema_has_spatial <- function(schema) {
    if (is.null(schema)) return(FALSE)
    z <- tryCatch(schema$defaults$spatial, error = function(e) NULL)
    is.list(z) && length(z) > 0L
}

.invsp_compact <- function(x) {
    keep <- !vapply(x, is.null, logical(1))
    x[keep]
}

.invsp_filter_read_args <- function(dots) {
    if (!length(dots)) return(list())
    nms <- names(dots)
    keep <- !is.na(nms) & nzchar(nms)
    dots <- dots[keep]
    nms <- names(dots)
    allowed <- c(
        "dir", "timeOut", "timeout", "check", "quiet", "encoding",
        "fileEncoding", "stringsAsFactors", "as.is", "dec", "sep"
    )
    dots[nms %in% allowed]
}

.invsp_detect_backend <- function(nfi, schema, parameter_table, dots) {
    if (!is.null(schema) || !is.null(parameter_table)) return("external")
    if (inherits(nfi, c("external_nfi", "external_nfiMetrics",
                        "external_metrics2vol", "external_dendroMetrics")))
        return("external")
    if (!is.null(dots[["backend_hint"]]) && identical(dots[["backend_hint"]], "external"))
        return("external")
    "snfi"
}

.invsp_prepare_input <- function(nfi, backend, spatial_run, dots,
                                 nfi.nr = NULL, dt.nm = NULL,
                                 file_ext = NULL, file_name = NULL,
                                 geometry.dt.nm = NULL, coord.nm = NULL,
                                 schema = NULL, coords = NULL,
                                 x.col = NULL, y.col = NULL,
                                 pr.col = NULL, plot.col = NULL,
                                 crs = NULL, coord.units = "m",
                                 geometry.source = "auto",
                                 coord.factor = NULL,
                                 huso.method = "auto",
                                 target.crs = NULL,
                                 huso.candidates = NULL,
                                 boundary = TRUE,
                                 boundary.source = c("auto", "gisco", "gadm", "user", "none"),
                                 boundary.object = NULL,
                                 boundary.level = 2,
                                 boundary.path = tools::R_user_dir("basifoR", "cache"),
                                 boundary.ext = "json",
                                 boundary.version = "4.1",
                                 boundary.crs = NULL,
                                 infer.huso = NULL,
                                 validate = TRUE) {
    ## Return a tabular object for the metric backend and, when requested,
    ## a plot-level geometry registry stored as an attribute sidecar.
    if (identical(spatial_run, "none")) {
        return(list(nfi = .invsp_drop_geometry_keep_attrs(nfi), registry = NULL))
    }

    if (.invsp_has_sidecar(nfi)) {
        z <- .invsp_drop_geometry_keep_attrs(nfi)
        registry <- .invsp_get_sidecar(z)
        if (isTRUE(boundary) &&
            (!.invsp_has_boundary(z) || !is.null(boundary.object)) &&
            exists(".basifoR_spatial_make_boundary_safe",
                   mode = "function", inherits = TRUE) &&
            exists(".basifoR_spatial_attach_boundary",
                   mode = "function", inherits = TRUE)) {
            boundary_context <- if (identical(backend, "snfi") &&
                                    exists(".basifoR_spatial_boundary_context",
                                           mode = "function", inherits = TRUE)) {
                .basifoR_spatial_boundary_context(
                    data = z,
                    registry = registry,
                    fallback = nfi
                )
            } else {
                NULL
            }
            boundary_registry <- .basifoR_spatial_make_boundary_safe(
                nfi = boundary_context,
                boundary = boundary,
                boundary.source = boundary.source,
                boundary.object = boundary.object,
                boundary.level = boundary.level,
                boundary.path = boundary.path,
                boundary.ext = boundary.ext,
                boundary.version = boundary.version,
                boundary.crs = boundary.crs,
                target.crs = if (!is.null(registry) &&
                                 inherits(registry$geometry, "sf")) {
                    sf::st_crs(registry$geometry)
                } else {
                    NULL
                },
                allow.gadm = identical(backend, "snfi"),
                validate = validate
            )
            z <- .basifoR_spatial_attach_boundary(
                z,
                boundary_registry = boundary_registry,
                requested = boundary
            )
        }
        return(list(nfi = z, registry = registry))
    }

    need_reader <- backend == "snfi" ||
        .invsp_any_sf(nfi) ||
        !is.null(coords) ||
        !is.null(boundary.object) ||
        .invsp_schema_has_spatial(schema) ||
        !is.null(x.col) || !is.null(y.col) || !is.null(crs)

    if (!need_reader) {
        if (identical(spatial_run, "sf") && isTRUE(validate)) {
            warning(
                paste(
                    "No spatial sidecar could be prepared for this input.",
                    "The metric workflow will return tabular output."
                ),
                call. = FALSE
            )
        }
        return(list(nfi = .invsp_drop_geometry_keep_attrs(nfi), registry = NULL))
    }

    if (!exists("readNFI_spatial", mode = "function", inherits = TRUE)) {
        if (isTRUE(validate)) {
            warning(
                paste(
                    "readNFI_spatial() is not available.",
                    "The metric workflow will return tabular output."
                ),
                call. = FALSE
            )
        }
        return(list(nfi = .invsp_drop_geometry_keep_attrs(nfi), registry = NULL))
    }

    gsrc <- match.arg(geometry.source, c("auto", "snfi", "external", "none"))
    boundary.source <- match.arg(boundary.source)
    if (backend == "external" && identical(gsrc, "auto"))
        gsrc <- "external"

    read_args <- .invsp_compact(c(
        list(
            nfi = nfi,
            nfi.nr = nfi.nr,
            dt.nm = dt.nm,
            file_ext = file_ext,
            file_name = file_name
        ),
        .invsp_filter_read_args(dots),
        list(
            spatial = "attribute",
            geometry.dt.nm = geometry.dt.nm,
            coord.nm = coord.nm,
            schema = schema,
            coords = coords,
            x.col = x.col,
            y.col = y.col,
            pr.col = pr.col,
            plot.col = plot.col,
            crs = crs,
            coord.units = coord.units,
            geometry.source = gsrc,
            validate = validate,
            coord.factor = coord.factor,
            huso.method = huso.method,
            target.crs = target.crs,
            huso.candidates = huso.candidates,
            boundary = boundary,
            boundary.source = boundary.source,
            boundary.object = boundary.object,
            boundary.level = boundary.level,
            boundary.path = boundary.path,
            boundary.ext = boundary.ext,
            boundary.version = boundary.version,
            boundary.crs = boundary.crs,
            infer.huso = infer.huso
        )
    ))

    z <- tryCatch(do.call(readNFI_spatial, read_args), error = function(e) e)
    if (inherits(z, "error")) {
        if (isTRUE(validate))
            warning(conditionMessage(z), call. = FALSE)
        return(list(nfi = .invsp_drop_geometry_keep_attrs(nfi), registry = NULL))
    }

    list(nfi = .invsp_drop_geometry_keep_attrs(z), registry = .invsp_get_sidecar(z))
}

.invsp_empty_sfc <- function(n, crs = NA) {
    sf::st_sfc(lapply(seq_len(n), function(i) sf::st_point()), crs = crs)
}

.invsp_assign_sfc <- function(out, sfc, geometry_role, spatial.summary,
                              keep.geometry.meta = TRUE,
                              geometry.role.name = NULL) {
    attrs <- .invsp_extra_attrs(out)

    if (!is.null(geometry.role.name) && nzchar(geometry.role.name))
        out[[geometry.role.name]] <- geometry_role

    sf::st_geometry(out) <- sfc
    out <- .invsp_restore_extra_attrs(out, attrs)
    attr(out, "geometry_role") <- geometry_role
    attr(out, "spatial_summary") <- spatial.summary
    attr(out, "geometry_units") <- "m"

    if (isTRUE(keep.geometry.meta)) {
        attr(out, "geometry_note") <- switch(
            geometry_role,
            plot_point = "Observed NFI plot point geometry.",
            plot_point_repeated = "Observed NFI plot point repeated over tree-level rows.",
            summary_centroid = "Representative centroid of unique plot coordinates in each summary group.",
            "Spatial geometry attached by inventoryMetrics_spatial()."
        )
    }

    out
}

.invsp_plot_registry_sf <- function(registry) {
    if (is.null(registry) || !is.list(registry) || !inherits(registry$geometry, "sf"))
        return(NULL)
    registry$geometry
}

.invsp_attach_plot_geometry <- function(out, registry,
                                        spatial.summary = "auto",
                                        geometry_role = "plot_point",
                                        keep.geometry.meta = TRUE,
                                        geometry.role.name = NULL,
                                        na.action = c("keep", "drop", "error"),
                                        validate = TRUE) {
    na.action <- match.arg(na.action)

    ## Prefer the canonical converter created with readNFI_spatial_v6.R.
    ## Important: asNFI_spatial_sf() rebuilds the object as sf and can drop
    ## custom metric attributes produced upstream by nfiMetrics(),
    ## metrics2Vol(), or dendroMetrics(). Capture those attributes before
    ## conversion and restore them afterwards, then add spatial metadata.
    if (exists("asNFI_spatial_sf", mode = "function", inherits = TRUE)) {
        metric_attrs <- .invsp_extra_attrs(out)
        z <- .invsp_copy_sidecar(structure(list(), nfi_geometry_registry = registry), out)
        ## The structure() trick above is not reliable for attributes in all R
        ## versions; set them explicitly below.
        attr(z, "nfi_geometry_registry") <- registry
        attr(z, "has_nfi_geometry") <- TRUE
        sf_out <- tryCatch(
            asNFI_spatial_sf(z, registry = registry, na.action = na.action,
                             validate = validate),
            error = function(e) e
        )
        if (!inherits(sf_out, "error")) {
            sf_out <- .invsp_restore_extra_attrs(sf_out, metric_attrs)
            attr(sf_out, "geometry_role") <- geometry_role
            attr(sf_out, "spatial_summary") <- spatial.summary
            attr(sf_out, "geometry_units") <- "m"
            if (isTRUE(keep.geometry.meta)) {
                attr(sf_out, "geometry_note") <- switch(
                    geometry_role,
                    plot_point = "Observed NFI plot point geometry.",
                    plot_point_repeated = "Observed NFI plot point repeated over tree-level rows.",
                    summary_centroid = "Representative centroid of unique plot coordinates in each summary group.",
                    "Spatial geometry attached by inventoryMetrics_spatial()."
                )
            }
            if (!is.null(geometry.role.name) && nzchar(geometry.role.name))
                sf_out[[geometry.role.name]] <- geometry_role
            return(sf_out)
        }
        if (isTRUE(validate))
            warning(conditionMessage(sf_out), call. = FALSE)
    }

    plot_reg <- .invsp_plot_registry_sf(registry)
    if (is.null(plot_reg)) return(out)

    out_cols <- .invsp_plot_cols(out, required = TRUE)
    geo_tab <- sf::st_drop_geometry(plot_reg)
    use_pr <- ".nfi_pr" %in% names(geo_tab) && any(!is.na(geo_tab$.nfi_pr))
    out_key <- .invsp_plot_key(out, cols = out_cols, use_pr = use_pr)
    reg_key <- if (use_pr) paste(geo_tab$.nfi_pr, geo_tab$.nfi_plot, sep = "\r") else geo_tab$.nfi_plot

    idx <- match(out_key, reg_key)
    missing <- is.na(idx)
    if (any(missing)) {
        msg <- paste0(sum(missing), " row(s) in the metric output did not match plot geometries.")
        if (identical(na.action, "error")) stop(msg, call. = FALSE)
        if (identical(na.action, "drop")) {
            if (isTRUE(validate)) warning(paste(msg, "These rows were dropped."), call. = FALSE)
            out <- out[!missing, , drop = FALSE]
            idx <- idx[!missing]
            missing <- missing[!missing]
        } else if (isTRUE(validate)) {
            warning(paste(msg, "They are kept with empty geometries."), call. = FALSE)
        }
    }

    g <- .invsp_empty_sfc(nrow(out), crs = sf::st_crs(plot_reg))
    ok <- !is.na(idx)
    if (any(ok)) g[ok] <- sf::st_geometry(plot_reg)[idx[ok]]

    .invsp_assign_sfc(out, g, geometry_role, spatial.summary,
                      keep.geometry.meta, geometry.role.name)
}

.invsp_centroid_geometry <- function(g) {
    g <- g[!sf::st_is_empty(g)]
    if (!length(g)) return(sf::st_point())
    sf::st_centroid(sf::st_combine(g))
}

.invsp_make_summary_centroids <- function(out, tree_level, registry, summ.vr) {
    plot_reg <- .invsp_plot_registry_sf(registry)
    if (is.null(plot_reg))
        stop("No plot geometry registry was available for summary centroids.", call. = FALSE)

    out_grp <- .invsp_resolve_cols(out, summ.vr, required = TRUE)
    tree_grp <- .invsp_resolve_cols(tree_level, summ.vr, required = TRUE)

    tree_plot_cols <- .invsp_plot_cols(tree_level, required = TRUE)
    geo_tab <- sf::st_drop_geometry(plot_reg)
    use_pr <- ".nfi_pr" %in% names(geo_tab) && any(!is.na(geo_tab$.nfi_pr))

    tree_plot_key <- .invsp_plot_key(tree_level, cols = tree_plot_cols, use_pr = use_pr)
    reg_key <- if (use_pr) paste(geo_tab$.nfi_pr, geo_tab$.nfi_plot, sep = "\r") else geo_tab$.nfi_plot
    idx <- match(tree_plot_key, reg_key)
    ok <- !is.na(idx)

    if (!any(ok))
        stop("No tree-level rows could be matched to plot geometries.", call. = FALSE)

    z <- tree_level[ok, tree_grp, drop = FALSE]
    names(z) <- out_grp
    z$.invsp_plot_key <- tree_plot_key[ok]
    z$.invsp_group_key <- .invsp_group_key(z, out_grp)
    z$.invsp_geom_index <- idx[ok]

    ## One plot contributes once per summary group; this avoids tree-count bias.
    z <- z[!duplicated(paste(z$.invsp_group_key, z$.invsp_plot_key, sep = "\r")), , drop = FALSE]
    split_z <- split(z, z$.invsp_group_key, drop = TRUE)

    cent_rows <- lapply(split_z, function(zz) {
        row <- zz[1L, out_grp, drop = FALSE]
        geom <- .invsp_centroid_geometry(sf::st_geometry(plot_reg)[zz$.invsp_geom_index])
        list(row = row, geom = geom)
    })

    cent_df <- do.call(rbind, lapply(cent_rows, `[[`, "row"))
    rownames(cent_df) <- NULL
    cent_sfc <- sf::st_sfc(lapply(cent_rows, `[[`, "geom"), crs = sf::st_crs(plot_reg))
    sf::st_geometry(cent_df) <- cent_sfc

    out_key <- .invsp_group_key(out, out_grp)
    cent_key <- .invsp_group_key(sf::st_drop_geometry(cent_df), out_grp)
    idx_out <- match(out_key, cent_key)

    g <- .invsp_empty_sfc(nrow(out), crs = sf::st_crs(plot_reg))
    ok_out <- !is.na(idx_out)
    if (any(ok_out)) g[ok_out] <- sf::st_geometry(cent_df)[idx_out[ok_out]]

    if (any(!ok_out)) {
        warning(
            sum(!ok_out),
            " row(s) in the metric output did not match summary centroids.",
            call. = FALSE
        )
    }

    g
}

.invsp_neutral_cut <- function(cut.dt) "d == d"

.invsp_restore_geometry <- function(out, registry, backend, nfi_for_aux,
                                    summ.vr, cut.dt, report, mc.cores,
                                    design, schema, method_registry,
                                    domheight_method, domheight_registry,
                                    parameter_table, dots,
                                    spatial.summary = c("auto", "centroid", "none"),
                                    keep.geometry.meta = TRUE,
                                    geometry.role.name = NULL,
                                    na.action = c("keep", "drop", "error"),
                                    validate = TRUE) {
    spatial.summary <- match.arg(spatial.summary)
    na.action <- match.arg(na.action)

    if (is.null(out) || is.null(registry) || spatial.summary == "none")
        return(out)

    if (!requireNamespace("sf", quietly = TRUE))
        stop("Package 'sf' is required for spatial output.", call. = FALSE)

    if (is.null(summ.vr)) {
        return(.invsp_attach_plot_geometry(
            out, registry, spatial.summary,
            geometry_role = "plot_point_repeated",
            keep.geometry.meta = keep.geometry.meta,
            geometry.role.name = geometry.role.name,
            na.action = na.action,
            validate = validate
        ))
    }

    out_has_plot <- .invsp_has_plot_col(out)
    summ_has_plot <- any(tolower(summ.vr) %in%
                             c("estadillo", "plot", "plot_id", "idp", "numpar",
                               "sample_id", "site_id"))

    if (isTRUE(out_has_plot) && isTRUE(summ_has_plot)) {
        return(.invsp_attach_plot_geometry(
            out, registry, spatial.summary,
            geometry_role = "plot_point",
            keep.geometry.meta = keep.geometry.meta,
            geometry.role.name = geometry.role.name,
            na.action = na.action,
            validate = validate
        ))
    }

    if (!identical(spatial.summary, "auto") && !identical(spatial.summary, "centroid"))
        return(out)

    ## Compute representative centroids only after metrics are finished.
    ## For SNFI, use the existing dendroMetrics() backend to align grouping
    ## names with the output. For external workflows, attempt the same only
    ## when external_dendroMetrics() is available and summ.vr columns can be
    ## resolved; otherwise keep a tabular object with sidecar metadata.
    tree_level <- NULL
    if (identical(backend, "snfi")) {
        aux_args <- .invsp_compact(c(
            list(
                nfi_for_aux,
                cut.dt = .invsp_neutral_cut(cut.dt),
                report = FALSE,
                mc.cores = mc.cores,
                design = design,
                method_registry = method_registry,
                domheight_method = domheight_method,
                domheight_registry = domheight_registry
            ),
            dots
        ))
        ## Explicit NULL requests the tree-level table needed for centroids.
        aux_args["summ.vr"] <- list(NULL)
        tree_level <- tryCatch(do.call(dendroMetrics, aux_args), error = function(e) e)
    } else if (exists("external_dendroMetrics", mode = "function", inherits = TRUE)) {
        aux_args <- .invsp_compact(c(
            list(
                nfi_for_aux,
                cut.dt = .invsp_neutral_cut(cut.dt),
                report = FALSE,
                mc.cores = mc.cores,
                design = design,
                schema = schema,
                method_registry = method_registry,
                parameter_table = parameter_table,
                domheight_method = domheight_method,
                domheight_registry = domheight_registry
            ),
            dots
        ))
        aux_args["summ.vr"] <- list(NULL)
        tree_level <- tryCatch(do.call(external_dendroMetrics, aux_args), error = function(e) e)
    }

    if (inherits(tree_level, "error") || is.null(tree_level)) {
        if (isTRUE(validate)) {
            warning(
                paste(
                    "Could not build an auxiliary tree-level table for spatial centroids.",
                    "Returning tabular output carrying the geometry sidecar."
                ),
                call. = FALSE
            )
        }
        return(out)
    }

    g <- tryCatch(
        .invsp_make_summary_centroids(out, tree_level, registry, summ.vr),
        error = function(e) e
    )
    if (inherits(g, "error")) {
        if (isTRUE(validate)) warning(conditionMessage(g), call. = FALSE)
        return(out)
    }

    .invsp_assign_sfc(
        out = out,
        sfc = g,
        geometry_role = "summary_centroid",
        spatial.summary = spatial.summary,
        keep.geometry.meta = keep.geometry.meta,
        geometry.role.name = geometry.role.name
    )
}

.invsp_append_class <- function(x,
                                spatial_class = "inventoryMetrics_spatial",
                                old_class = "inventoryMetrics") {
    if (is.null(x))
        return(x)

    cls <- class(x)
    cls <- setdiff(cls, c(spatial_class, old_class))

    if (inherits(x, "sf")) {
        ## Keep the basifoR spatial class before "sf" so plot(x)
        ## dispatches to plot.<basifoR_spatial_class>(). The "sf" class
        ## remains in the vector, so sf::st_* and ggplot2::geom_sf()
        ## continue to work.
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

inventoryMetrics_spatial <- structure(function#Complete inventory workflow carrying a spatial sidecar
### Run the complete inventory workflow while preserving optional plot geometry
###
### This function is a conservative spatial companion to
### \code{\link{inventoryMetrics}}.  It keeps the original calculation
### engines for Spanish NFI and external inventories, but prepares or reuses a
### plot-level geometry registry and carries that registry in attributes.  The
### geometry column is reconstructed only at the end when \code{spatial =
### "sf"}.  The original \code{inventoryMetrics()} function is not replaced.
(
    nfi, ##<< Inventory source accepted by the selected backend.  For Spanish
         ## NFI this can be a province code/name, URL, ZIP file, decompressed
         ## files, a \code{readNFI} object, a \code{readNFI_spatial}
         ## object, or an \code{sf} object.  For external inventories it can
         ## be the same input accepted by \code{external_dendroMetrics()}.
    backend = c("auto", "snfi", "external"), ##<< Backend selector.  The
         ## default infers \code{"external"} when \code{schema} or
         ## \code{parameter_table} is supplied, otherwise uses
         ## \code{"snfi"}.
    summ.vr = "Estadillo", ##<< Grouping variable passed to the selected
                            ## backend. When omitted, Spanish NFI uses
                            ## \code{"Estadillo"} and external inventories use
                            ## the schema level or detected plot identifier.
                            ## Supply a vector such as plot and species for
                            ## finer groups, or \code{NULL} for tree-level
                            ## output.
    cut.dt = "d == d", ##<< Logical expression used by the backend to subset
                        ## the metric output.
    report = FALSE, ##<< Request backend reports when supported.
    mc.cores = getOption("mc.cores", 1L), ##<< Number of cores passed to the
                                           ## selected backend.
    design = NULL, ##<< Optional sampling design passed to the backend.
    schema = NULL, ##<< Optional external schema.  If it contains
                   ## \code{defaults$spatial}, it can also define external
                   ## coordinates for the spatial sidecar.
    method_registry = NULL, ##<< Optional volume method registry passed to
                            ## the selected backend.
    domheight_method = "Hd_strict", ##<< Dominant-height method passed to
                            ## \code{dendroMetrics()} for SNFI inputs.
                            ## For external inputs, this method is resolved
                            ## to a function and passed as
                            ## \code{domheight_fun}.
    domheight_registry = dominant_height_method_registry(), ##<< Named
                            ## dominant-height registry created with
                            ## \code{dominant_height_method_registry()}.
    parameter_table = NULL, ##<< Optional parameter table for external
                            ## inventories.
    ..., ##<< Additional arguments forwarded to the selected backend.  A safe
         ## subset such as \code{nfi.nr}, \code{dt.nm}, \code{dir}, and
         ## \code{timeOut} is also used by \code{readNFI_spatial()} when a
         ## sidecar must be prepared from a source.
    spatial = c("attribute", "sf", "none", "inherit"), ##<< Spatial policy.
         ## \code{"attribute"} returns a tabular result with
         ## \code{attr(x, "nfi_geometry_registry")}.  \code{"sf"}
         ## returns an \code{sf} object when geometry can be restored.
         ## \code{"none"} runs the original tabular workflow.
         ## \code{"inherit"} returns \code{sf} for \code{sf} input,
         ## \code{"attribute"} for sidecar input, and tabular output
         ## otherwise.
    spatial.summary = c("auto", "centroid", "none"), ##<< Geometry rule after
         ## summarization.  \code{"auto"} preserves observed plot points
         ## when plot identity remains and uses centroids for multi-plot
         ## summaries.  \code{"centroid"} forces centroid construction for
         ## multi-plot summaries.  \code{"none"} leaves the result tabular
         ## while preserving the sidecar attribute.
    keep.geometry.meta = TRUE, ##<< Store geometry role and spatial-summary
                               ## metadata as attributes.
    geometry.role.name = NULL, ##<< Optional visible column containing the
                               ## geometry role, for example
                               ## \code{"plot_point"} or
                               ## \code{"summary_centroid"}.
    geometry.dt.nm = NULL, ##<< Optional table name used by
                           ## \code{readNFI_spatial()} to build the sidecar.
    coord.nm = NULL, ##<< Optional coordinate table name for Spanish NFI.
    coords = NULL, ##<< Optional external coordinate table.
    x.col = NULL, ##<< External X/easting/longitude coordinate column.
    y.col = NULL, ##<< External Y/northing/latitude coordinate column.
    pr.col = NULL, ##<< Optional province/region key for coordinate joins.
    plot.col = NULL, ##<< Optional plot identifier key for coordinate joins.
    crs = NULL, ##<< CRS for external coordinates, for example EPSG 25830.
    coord.units = "m", ##<< Coordinate units recorded in the sidecar.
    geometry.source = c("auto", "snfi", "external", "none"), ##<< Geometry
         ## source passed to \code{readNFI_spatial()}.
    file_ext = NULL, ##<< Optional file extension passed to
                     ## \code{readNFI_spatial()}.
    file_name = NULL, ##<< Optional file name filter passed to
                      ## \code{readNFI_spatial()}.
    coord.factor = NULL, ##<< Optional coordinate multiplier for Spanish NFI
                         ## tables.  IFN2 kilometre coordinates are converted
                         ## to metres by default in \code{readNFI_spatial()}.
    huso.method = c("auto", "candidate", "xgap", "province", "none"), ##<< UTM-zone
                         ## assignment method passed to
                         ## \code{readNFI_spatial()}.
    huso.candidates = NULL, ##<< Optional candidate UTM zones passed to
                            ## \code{readNFI_spatial()}, for example
                            ## \code{c(29, 30)} for IFN2 provinces that
                            ## span Huso 29 and 30 when no Huso columns are
                            ## available in the coordinate table.
    target.crs = NULL, ##<< Optional common CRS for Spanish NFI geometries.
    boundary = TRUE, ##<< \code{logical}. If \code{TRUE} (default), attach an optional
                      ## administrative boundary sidecar for plot maps.
    boundary.source = c("auto", "gisco", "gadm", "user", "none"), ##<< Boundary
                      ## source passed to \code{readNFI_spatial()}. For external
                      ## inventories, \code{"auto"} uses only a supplied
                      ## \code{boundary.object}; it does not infer a Spanish
                      ## built-in boundary. For Spanish inputs, \code{"auto"}
                      ## tries GISCO/NUTS before GADM.
    boundary.object = NULL, ##<< Optional user-provided \code{sf} polygon or
                      ## multipolygon layer. This is the supported boundary
                      ## route for external inventories.
    boundary.level = 2, ##<< GADM administrative level, used when
                        ## \code{boundary.source} resolves to \code{"gadm"}.
    boundary.path = tools::R_user_dir("basifoR", "cache"), ##<< Cache directory
                        ## for optional boundary downloads.
    boundary.ext = "json", ##<< GADM extension used by \code{gadm_spatial()}.
    boundary.version = "4.1", ##<< GADM version used by \code{gadm_spatial()}.
    boundary.crs = NULL, ##<< CRS assigned to \code{boundary.object} when
                      ## missing. It is required when the supplied object has
                      ## no CRS metadata.
    infer.huso = NULL, ##<< Backward-compatible Huso inference argument passed
                       ## to \code{readNFI_spatial()}.
    validate = TRUE, ##<< Warn about sidecar or geometry-restoration problems.
    na.action = c("keep", "drop", "error") ##<< Handling of metric rows that
                       ## do not match the plot geometry registry when
                       ## \code{spatial = "sf"}.
) {
    ##title<< Complete inventory metrics with optional sf reconstruction
    ##details<<
    ##details<< The function does not send \code{sf} geometries through the
    ##details<< metric engines.  It prepares a plot-level sidecar with
    ##details<< \code{readNFI_spatial()} with \code{spatial = "attribute"}, drops any
    ##details<< active geometry before running the backend, computes metrics
    ##details<< with the existing tabular engines, copies the sidecar to the
    ##details<< result, and reconstructs \code{sf} only at the end when
    ##details<< requested.  This protects metric metadata such as units,
    ##details<< design metadata, and volume metadata. Tree-level expansion
    ##details<< factors remain \code{n}; grouped stand density is \code{n_tot}.
    ##details<<
    ##details<< External inventories keep their original backend path.  When
    ##details<< external coordinates are supplied through an \code{sf} input,
    ##details<< \code{coords}, or \code{schema$defaults$spatial}, the geometry
    ##details<< registry is carried in attributes and may be restored if the
    ##details<< output keeps compatible grouping keys.  Without external
    ##details<< coordinate information, external outputs remain tabular.
    ##details<< When \code{summ.vr} is omitted for an external inventory, the
    ##details<< function uses \code{schema$levels} when available and otherwise
    ##details<< detects the plot identifier from the prepared input.
    ##details<< External boundaries are never inferred from province-like
    ##details<< columns. Supply an \code{sf} polygon or multipolygon through
    ##details<< \code{boundary.object}; its CRS must be present or supplied
    ##details<< through \code{boundary.crs}.
    ##value<< A result equivalent to the selected backend output, augmented
    ##value<< with class \code{"inventoryMetrics_spatial"}.  With
    ##value<< \code{spatial = "attribute"}, the output is tabular and carries
    ##value<< the plot geometry in \code{attr(x, "nfi_geometry_registry")}.
    ##value<< With \code{spatial = "sf"}, the result is an \code{sf} object
    ##value<< when geometry restoration succeeds.

    call0 <- match.call(expand.dots = TRUE)
    dots0 <- list(...)
    summ_missing <- missing(summ.vr)
    backend <- match.arg(backend)
    geometry.source <- match.arg(geometry.source)
    huso.method <- match.arg(huso.method)
    boundary.source <- match.arg(boundary.source)
    spatial.summary <- match.arg(spatial.summary)
    na.action <- match.arg(na.action)
    spatial_run <- .invsp_resolve_mode(spatial, nfi)

    if (backend == "auto")
        backend <- .invsp_detect_backend(nfi, schema, parameter_table, dots0)

    if (backend == "snfi" && (!is.null(schema) || !is.null(parameter_table))) {
        stop("'schema' and 'parameter_table' are only valid for backend = 'external'.",
             call. = FALSE)
    }

    if (backend == "external" &&
        !exists("external_dendroMetrics", mode = "function", inherits = TRUE)) {
        stop("Backend 'external' requires external_dendroMetrics().", call. = FALSE)
    }

    nfi.nr <- dots0[["nfi.nr"]]
    dt.nm <- dots0[["dt.nm"]]

    prepared <- .invsp_prepare_input(
        nfi = nfi,
        backend = backend,
        spatial_run = spatial_run,
        dots = dots0,
        nfi.nr = nfi.nr,
        dt.nm = dt.nm,
        file_ext = file_ext,
        file_name = file_name,
        geometry.dt.nm = geometry.dt.nm,
        coord.nm = coord.nm,
        schema = schema,
        coords = coords,
        x.col = x.col,
        y.col = y.col,
        pr.col = pr.col,
        plot.col = plot.col,
        crs = crs,
        coord.units = coord.units,
        geometry.source = geometry.source,
        coord.factor = coord.factor,
        huso.method = huso.method,
        target.crs = target.crs,
        huso.candidates = huso.candidates,
        boundary = boundary,
        boundary.source = boundary.source,
        boundary.object = boundary.object,
        boundary.level = boundary.level,
        boundary.path = boundary.path,
        boundary.ext = boundary.ext,
        boundary.version = boundary.version,
        boundary.crs = boundary.crs,
        infer.huso = infer.huso,
        validate = validate
    )

    nfi_backend <- prepared$nfi
    registry <- prepared$registry

    requested_var <- dots0[["var"]]
    hd_requested <- if (is.null(requested_var)) {
        TRUE
    } else {
        any(tolower(as.character(requested_var)) == "hd")
    }

    dominant_height_meta <- NULL
    if (hd_requested) {
        custom_domheight <- dots0[["domheight_fun"]]
        dominant_height_meta <- if (is.function(custom_domheight)) {
            list(
                method = "custom_function",
                output = "Hd",
                fun_name = NA_character_,
                unit = "m",
                equation = NA_character_,
                selection_rule = "User-supplied dominant-height function.",
                fallback = NA_character_
            )
        } else {
            tryCatch(
                resolve_dominant_height_method(
                    method = domheight_method,
                    registry = domheight_registry
                )$meta,
                error = function(e) NULL
            )
        }
    }

    if (backend == "snfi") {
        summ_vr_backend <- if (summ_missing) "Estadillo" else summ.vr
        args <- .invsp_compact(c(
            list(
                nfi_backend,
                cut.dt = cut.dt,
                report = report,
                mc.cores = mc.cores,
                design = design,
                method_registry = method_registry,
                domheight_method = domheight_method,
                domheight_registry = domheight_registry
            ),
            dots0
        ))
        ## Preserve explicit summ.vr = NULL; NULL selects tree-level output.
        args["summ.vr"] <- list(summ_vr_backend)
        out <- do.call(dendroMetrics, args)
        summ_vr_for_geometry <- summ_vr_backend
    } else {
        summ_vr_backend <- if (summ_missing) {
            schema_levels <- if (!is.null(schema)) schema$levels else NULL
            if (length(schema_levels)) {
                schema_levels
            } else {
                plot_info <- .invsp_plot_cols(nfi_backend, required = FALSE)
                if (is.na(plot_info$plot)) {
                    stop(
                        paste(
                            "Could not determine the external plot grouping.",
                            "Supply 'summ.vr' or define 'levels' in the external schema."
                        ),
                        call. = FALSE
                    )
                }
                plot_info$plot
            }
        } else {
            summ.vr
        }

        base_args <- list(
            nfi_backend,
            cut.dt = cut.dt,
            report = report,
            mc.cores = mc.cores,
            design = design,
            schema = schema,
            method_registry = method_registry,
            parameter_table = parameter_table,
            domheight_method = domheight_method,
            domheight_registry = domheight_registry
        )
        args <- .invsp_compact(c(base_args, dots0))
        ## Preserve explicit summ.vr = NULL; NULL selects tree-level output.
        args["summ.vr"] <- list(summ_vr_backend)
        out <- do.call(external_dendroMetrics, args)
        summ_vr_for_geometry <- summ_vr_backend
    }

    if (hd_requested &&
        !is.null(dominant_height_meta) &&
        is.null(attr(out, "dominant_height_meta", exact = TRUE)))
        attr(out, "dominant_height_meta") <- dominant_height_meta

    if (!is.null(registry)) {
        attr(nfi_backend, "nfi_geometry_registry") <- registry
        attr(nfi_backend, "has_nfi_geometry") <- TRUE
    }
    if (!is.null(registry) || .invsp_has_boundary(nfi_backend)) {
        out <- .invsp_copy_sidecar(nfi_backend, out)
    }

    if (identical(spatial_run, "sf") && !is.null(registry)) {
        out <- .invsp_restore_geometry(
            out = out,
            registry = registry,
            backend = backend,
            nfi_for_aux = nfi_backend,
            summ.vr = summ_vr_for_geometry,
            cut.dt = cut.dt,
            report = report,
            mc.cores = mc.cores,
            design = design,
            schema = schema,
            method_registry = method_registry,
            domheight_method = domheight_method,
            domheight_registry = domheight_registry,
            parameter_table = parameter_table,
            dots = dots0,
            spatial.summary = spatial.summary,
            keep.geometry.meta = keep.geometry.meta,
            geometry.role.name = geometry.role.name,
            na.action = na.action,
            validate = validate
        )
    }

    attr(out, "call") <- call0
    attr(out, "backend") <- backend
    attr(out, "spatial_requested") <- spatial_run
    .invsp_append_class(out, "inventoryMetrics_spatial", "inventoryMetrics")
	}, ex = function() {
	    ext <- data.frame(
	        plot = c("P1", "P1", "P2"),
	        species = c("sp1", "sp1", "sp2"),
	        diameter_mm = c(120, 185, 260),
	        height_m = c(7.1, 9.4, 13.2),
	        x = c(-3.70, -3.70, -3.69),
	        y = c(40.40, 40.40, 40.41)
	    )

	    sch <- new_external_schema(
	        colmap = list(
	            plot = "plot",
	            species = "species",
	            d = "diameter_mm",
	            h = "height_m"
	        ),
	        units = list(d = "mm", h = "m"),
	        levels = "plot",
	        keep_cols = c("plot", "species"),
	        defaults = list(
	            spatial = list(plot = "plot", x = "x", y = "y", crs = 4326)
	        )
	    )

	    dsg <- new_inventory_design(
	        sample_area_m2 = 1000,
	        min_dbh_cm = 7.5,
	        name = "Square 0.1-ha plot"
	    )

	    x <- inventoryMetrics_spatial(
	        ext,
	        backend = "external",
	        schema = sch,
	        design = dsg,
	        summ.vr = "plot",
	        var = c("d", "h", "ba", "n"),
	        spatial = "attribute",
	        geometry.source = "external",
	        boundary = FALSE
	    )

	    inherits(x, "inventoryMetrics")
	    hasNFIgeometry_spatial(x)

	    ## Tabular sidecar output for Spanish NFI:
	    ## x <- inventoryMetrics_spatial(28, nfi.nr = 2, dir = tempdir())
	    ## hasNFIgeometry_spatial(x)

    ## Direct sf output:
    ## xsf <- inventoryMetrics_spatial(28, nfi.nr = 2, dir = tempdir(), spatial = "sf")
    ## inherits(xsf, "sf")
    ## attr(xsf, "geometry_role")

    ## Dominant-height calculations use the same inspectable registry exposed
    ## by nfiMetrics(). Metadata is preserved in attr(x, "dominant_height_meta")
    ## when Hd is computed.
    ##
	    ## External inventories keep the original external backend. If the schema
	    ## contains defaults$spatial or the input is sf, the sidecar can be carried.
	})

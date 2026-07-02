## Extracted from basifoR_spatial_functions_v14_plotdispatch.R
## Source lines 2903-3483.
## Source readNFI_spatial_v15_coord_qa.R before this file so shared
## sidecar, GADM, and sf-reconstruction helpers are available.

## Spatial sidecar companion for metrics2Vol().
##
## This script intentionally does not replace metrics2Vol().  It defines
## metrics2Vol_spatial(), a conservative wrapper that keeps the original
## metrics2Vol() calculations unchanged and only carries the plot-level
## geometry registry created by readNFI_spatial()/nfiMetrics_spatial() as an
## attribute sidecar.  The function can optionally reconstruct an sf object at
## the end.
##
## Expected workflow while developing the spatial branch:
##   source("readNFI_spatial_v11_boundaries.R")
##   source("nfiMetrics_spatial_v5_boundaries.R")
##   source("metrics2Vol_spatial_v3_boundaries.R")
##
## Internal helpers use the .basifoR_metrics2Vol_spatial_* prefix to avoid
## overwriting basifoR internals and other spatial helpers.

.basifoR_metrics2Vol_spatial_get_registry <- function(x) {
    attr(x, "nfi_geometry_registry", exact = TRUE)
}

.basifoR_metrics2Vol_spatial_has_registry <- function(x) {
    if (exists("hasNFIgeometry_spatial", mode = "function", inherits = TRUE))
        return(hasNFIgeometry_spatial(x))

    reg <- .basifoR_metrics2Vol_spatial_get_registry(x)
    !is.null(reg) && is.list(reg) && !is.null(reg$geometry)
}

.basifoR_metrics2Vol_spatial_copy_registry <- function(from, to) {
    if (exists("copyNFIspatial_sidecars", mode = "function", inherits = TRUE))
        return(copyNFIspatial_sidecars(from = from, to = to))
    if (exists("copyNFIgeometry_spatial", mode = "function", inherits = TRUE))
        return(copyNFIgeometry_spatial(from = from, to = to))

    keep <- c(
        "nfi_geometry_registry",
        "has_nfi_geometry",
        "spatial_requested",
        "nfi_boundary",
        "has_nfi_boundary",
        "boundary_requested",
        "nfi_boundary_failed"
    )

    for (nm in keep) {
        val <- attr(from, nm, exact = TRUE)
        if (!is.null(val))
            attr(to, nm) <- val
    }

    to
}

.basifoR_metrics2Vol_spatial_copy_custom_attrs <- function(from, to) {
    ## Preserve metadata produced by nfiMetrics()/metrics2Vol(), such as
    ## units, design_meta, volume_meta, and nfi.nr, after optional sf
    ## conversion.  Do not copy base data-frame/sf structural attributes.
    at <- attributes(from)
    if (is.null(at) || !length(at))
        return(to)

    skip <- c(
        "names", "row.names", "class",
        "sf_column", "agr", "bbox", "geometry"
    )

    for (nm in setdiff(names(at), skip))
        attr(to, nm) <- at[[nm]]

    to
}

.basifoR_metrics2Vol_spatial_append_class <- function(x,
                                                                spatial_class,
                                                                old_class) {
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

.basifoR_metrics2Vol_spatial_resolve_mode <- function(spatial, nfi) {
    if (!identical(spatial, "inherit"))
        return(spatial)

    if (inherits(nfi, "sf"))
        return("sf")

    if (.basifoR_metrics2Vol_spatial_has_registry(nfi))
        return("attribute")

    "none"
}

.basifoR_metrics2Vol_spatial_drop_sf <- function(x) {
    ## metrics2Vol() is a tabular routine.  If an sf object enters this
    ## wrapper, drop the geometry column before calling metrics2Vol(), but keep
    ## custom metadata and the geometry registry sidecar.
    if (!inherits(x, "sf"))
        return(x)

    if (!requireNamespace("sf", quietly = TRUE)) {
        stop("Package 'sf' is required to process sf input.", call. = FALSE)
    }

    y <- sf::st_drop_geometry(x)
    y <- .basifoR_metrics2Vol_spatial_copy_custom_attrs(from = x, to = y)
    y <- .basifoR_metrics2Vol_spatial_copy_registry(from = x, to = y)
    y
}

.basifoR_metrics2Vol_spatial_prepare_metrics <- function(nfi,
                                                          dots,
                                                          spatial,
                                                          var,
                                                          levels,
                                                          design,
                                                          domheight_method = "Hd",
                                                          domheight_registry = dominant_height_method_registry(),
                                                          geometry.dt.nm = NULL,
                                                          schema = NULL,
                                                          coords = NULL,
                                                          x.col = NULL,
                                                          y.col = NULL,
                                                          pr.col = NULL,
                                                          plot.col = NULL,
                                                          crs = NULL,
                                                          coord.units = "m",
                                                          geometry.source = "auto",
                                                          validate = TRUE,
                                                          coord.nm = NULL,
                                                          coord.factor = NULL,
                                                          huso.method = c("auto", "candidate", "province", "none"),
                                                          target.crs = NULL,
                                                          boundary = TRUE,
                                                          boundary.source = c("auto", "gisco", "gadm", "user", "none"),
                                                          boundary.object = NULL,
                                                          boundary.level = 2,
                                                          boundary.path = tools::R_user_dir("basifoR", "cache"),
                                                          boundary.ext = "json",
                                                          boundary.version = "4.1",
                                                          boundary.crs = NULL,
                                                          infer.huso = NULL) {
    huso.method <- match.arg(huso.method)
    boundary.source <- match.arg(boundary.source)

    if (identical(spatial, "none"))
        return(.basifoR_metrics2Vol_spatial_drop_sf(nfi))

    ## Already computed nfiMetrics objects should not be recomputed.  Just make
    ## them safe for the tabular metrics2Vol() calculation.
    if (inherits(nfi, "nfiMetrics"))
        return(.basifoR_metrics2Vol_spatial_drop_sf(nfi))

    ## If a readNFI/readNFI_spatial object already carries a geometry sidecar,
    ## metrics2Vol() can call the original nfiMetrics() internally.  We still
    ## drop sf geometry, if present, before passing it to metrics2Vol().
    if (.basifoR_metrics2Vol_spatial_has_registry(nfi))
        return(.basifoR_metrics2Vol_spatial_drop_sf(nfi))

    ## Prefer nfiMetrics_spatial() when it is available.  It creates the
    ## sidecar, calls the original nfiMetrics(), and preserves metric metadata.
    if (exists("nfiMetrics_spatial", mode = "function", inherits = TRUE)) {
        args <- c(
            list(
                nfi = nfi,
                var = var,
                levels = levels,
                design = design,
                domheight_method = domheight_method,
                domheight_registry = domheight_registry,
                spatial = "attribute",
                geometry.dt.nm = geometry.dt.nm,
                schema = schema,
                coords = coords,
                x.col = x.col,
                y.col = y.col,
                pr.col = pr.col,
                plot.col = plot.col,
                crs = crs,
                coord.units = coord.units,
                geometry.source = geometry.source,
                validate = validate,
                coord.nm = coord.nm,
                coord.factor = coord.factor,
                huso.method = huso.method,
                target.crs = target.crs,
                boundary = boundary,
                boundary.source = boundary.source,
                boundary.object = boundary.object,
                boundary.level = boundary.level,
                boundary.path = boundary.path,
                boundary.ext = boundary.ext,
                boundary.version = boundary.version,
                boundary.crs = boundary.crs
            ),
            dots
        )

        if (is.null(geometry.dt.nm))
            args$geometry.dt.nm <- NULL
        if (!is.null(infer.huso))
            args$infer.huso <- infer.huso

        z <- tryCatch(
            do.call(nfiMetrics_spatial, args),
            error = function(e) e
        )

        if (!inherits(z, "error"))
            return(.basifoR_metrics2Vol_spatial_drop_sf(z))

        if (isTRUE(validate)) {
            warning(
                paste(
                    "Could not prepare nfiMetrics_spatial() before metrics2Vol().",
                    conditionMessage(z),
                    "Falling back to the original metrics2Vol() workflow."
                ),
                call. = FALSE
            )
        }
    }

    ## Fallback: create a readNFI_spatial sidecar and let the original
    ## metrics2Vol() call nfiMetrics() internally.
    if (exists("readNFI_spatial", mode = "function", inherits = TRUE)) {
        args <- c(
            list(
                nfi = nfi,
                spatial = "attribute",
                schema = schema,
                coords = coords,
                x.col = x.col,
                y.col = y.col,
                pr.col = pr.col,
                plot.col = plot.col,
                crs = crs,
                coord.units = coord.units,
                geometry.source = geometry.source,
                validate = validate,
                coord.nm = coord.nm,
                coord.factor = coord.factor,
                huso.method = huso.method,
                target.crs = target.crs,
                boundary = boundary,
                boundary.source = boundary.source,
                boundary.object = boundary.object,
                boundary.level = boundary.level,
                boundary.path = boundary.path,
                boundary.ext = boundary.ext,
                boundary.version = boundary.version,
                boundary.crs = boundary.crs
            ),
            dots
        )

        if (!is.null(geometry.dt.nm))
            args$geometry.dt.nm <- geometry.dt.nm
        if (!is.null(infer.huso))
            args$infer.huso <- infer.huso

        z <- tryCatch(
            do.call(readNFI_spatial, args),
            error = function(e) e
        )

        if (!inherits(z, "error"))
            return(.basifoR_metrics2Vol_spatial_drop_sf(z))

        if (isTRUE(validate)) {
            warning(
                paste(
                    "Could not prepare readNFI_spatial() before metrics2Vol().",
                    conditionMessage(z),
                    "Falling back to the original metrics2Vol() workflow."
                ),
                call. = FALSE
            )
        }
    }

    .basifoR_metrics2Vol_spatial_drop_sf(nfi)
}

.basifoR_metrics2Vol_spatial_finalize <- function(out,
                                                   source,
                                                   spatial,
                                                   na.action = "keep",
                                                   validate = TRUE) {
    if (!is.data.frame(out))
        return(out)

    if (!identical(spatial, "none") &&
        .basifoR_metrics2Vol_spatial_has_registry(source)) {
        out <- .basifoR_metrics2Vol_spatial_copy_registry(
            from = source,
            to = out
        )
        attr(out, "spatial_metric_stage") <- "metrics2Vol_spatial"
        attr(out, "spatial_requested") <- spatial
    }

    if (identical(spatial, "sf") &&
        .basifoR_metrics2Vol_spatial_has_registry(out)) {

        if (!exists("asNFI_spatial_sf", mode = "function", inherits = TRUE)) {
            if (isTRUE(validate)) {
                warning(
                    paste(
                        "asNFI_spatial_sf() was not found.",
                        "Returning the tabular metrics2Vol output with the spatial sidecar."
                    ),
                    call. = FALSE
                )
            }
        } else {
            sf_out <- tryCatch(
                asNFI_spatial_sf(
                    out,
                    na.action = na.action,
                    validate = validate
                ),
                error = function(e) e
            )

            if (inherits(sf_out, "error")) {
                if (isTRUE(validate)) {
                    warning(
                        paste(
                            "Could not reconstruct sf geometry after metrics2Vol().",
                            conditionMessage(sf_out),
                            "Returning the tabular output with the spatial sidecar."
                        ),
                        call. = FALSE
                    )
                }
            } else {
                ## sf::st_sf()/st_as_sf() rebuilds the object and may drop
                ## custom metric/volume metadata.  Restore all non-structural
                ## attributes from the tabular metrics2Vol() output before
                ## adding spatial-stage flags.
                sf_out <- .basifoR_metrics2Vol_spatial_copy_custom_attrs(
                    from = out,
                    to = sf_out
                )

                out <- sf_out
                attr(out, "spatial_metric_stage") <- "metrics2Vol_spatial"
                attr(out, "spatial_requested") <- "sf"
                if (is.null(attr(out, "geometry_role", exact = TRUE)))
                    attr(out, "geometry_role") <- "plot_point_repeated"
            }
        }
    }

    .basifoR_metrics2Vol_spatial_append_class(out, "metrics2Vol_spatial", "metrics2Vol")
}

metrics2Vol_spatial <- structure(function#Tree-level volumes carrying a spatial sidecar
### Compute tree-level SNFI volume variables while preserving plot geometry
###
### This function mirrors the public arguments and calculations of
### \code{\link{metrics2Vol}} but adds an optional spatial sidecar.  The volume
### computation itself is delegated to the original \code{metrics2Vol()}, so the
### equation registry, fallback logic, provenance, units, and volume metadata
### remain consistent with the non-spatial workflow.
###
### Spatial information is prepared before the volume call by invoking
### \code{nfiMetrics_spatial()} with \code{spatial = "attribute"} when the input is not
### already an \code{"nfiMetrics"} object, and is copied back to the volume
### output after \code{metrics2Vol()} finishes.  Thus the geometry column never
### travels through the volume-equation calculations.  If \code{spatial =
### "sf"}, the function reconstructs an \code{sf} point object only at the end.
(
    nfi, ##<< Input accepted by \code{\link{metrics2Vol}}.  This can be a
         ## province code/name, a file path, a \code{"readNFI"} object, a
         ## \code{"readNFI_spatial"} object, a \code{"nfiMetrics"} object,
         ## or an \code{"sf"} object carrying plot-level SNFI records.
    cub.met = "freq", ##<< Same as \code{metrics2Vol()}. Cubication selector
                       ## used when several coefficient rows match.
    parametro = c("VCC"), ##<< Same as \code{metrics2Vol()}. One or more
                          ## volume outputs to compute.
    keep.var = TRUE, ##<< Same as \code{metrics2Vol()}. Keep auxiliary
                     ## coefficient columns when available.
    keep.legacy = FALSE, ##<< Same as \code{metrics2Vol()}. Also return the
                         ## legacy volume estimate for backward compatibility.
    method_registry = snfi_volume_method_registry(), ##<< Same as
                         ## \code{metrics2Vol()}. Registry that maps each
                         ## requested output to its equation function,
                         ## output column name, units, and fallback rule.
    track_provenance = FALSE, ##<< Same as \code{metrics2Vol()}. Add per-row
                              ## provenance columns and audit metadata.
    ..., ##<< Additional arguments passed to \code{nfiMetrics_spatial()} when
         ## metrics must be computed before volume calculation.  Typical
         ## examples are \code{nfi.nr}, \code{dt.nm}, \code{dir},
         ## \code{file_ext}, and \code{timeOut}.
    spatial = c("attribute", "sf", "none", "inherit"), ##<< Spatial output
         ## mode.  \code{"attribute"} keeps the ordinary tabular
         ## \code{metrics2Vol()} output and stores the plot geometry
         ## registry in attributes.  \code{"sf"} reconstructs point geometry
         ## at the end.  \code{"none"} runs the original non-spatial
         ## workflow. \code{"inherit"} returns \code{"sf"} for sf input,
         ## \code{"attribute"} for objects already carrying a sidecar, and
         ## \code{"none"} otherwise.
    var = c('d','h','ba','n','Hd','Dd'), ##<< Forwarded to
                                    ## \code{nfiMetrics_spatial()} if the
                                    ## input is not already an
                                    ## \code{"nfiMetrics"} object.
    levels = c('esta','espe'), ##<< Forwarded to \code{nfiMetrics_spatial()}.
                               ## Keeping the plot identifier is important
                               ## for final \code{sf} reconstruction.
    design = snfi_design(), ##<< Forwarded to \code{nfiMetrics_spatial()}.
    domheight_method = "Hd", ##<< Dominant-height method forwarded to
                                    ## \code{nfiMetrics_spatial()} or,
                                    ## when metrics are computed inside
                                    ## \code{metrics2Vol()}, to
                                    ## \code{nfiMetrics()}.
    domheight_registry = dominant_height_method_registry(), ##<< Named
                                    ## dominant-height registry created with
                                    ## \code{dominant_height_method_registry()}.
    geometry.dt.nm = NULL, ##<< Optional table name used to build the geometry
                           ## registry.  When \code{NULL}, the default of
                           ## \code{readNFI_spatial()} is used, normally the
                           ## same table requested by \code{dt.nm}.
    schema = NULL, ##<< Optional external schema forwarded to
                   ## \code{nfiMetrics_spatial()} and \code{readNFI_spatial()}.
    coords = NULL, ##<< Optional coordinate table forwarded to
                   ## \code{readNFI_spatial()} when coordinates are stored
                   ## outside the main table.
    x.col = NULL, ##<< Optional X/easting/longitude coordinate column for
                  ## external coordinate tables.
    y.col = NULL, ##<< Optional Y/northing/latitude coordinate column for
                  ## external coordinate tables.
    pr.col = NULL, ##<< Optional province, region, or stratum column used in
                   ## spatial joins.
    plot.col = NULL, ##<< Optional plot identifier column used in spatial joins.
    crs = NULL, ##<< Optional CRS for external coordinates, for example an
                ## EPSG code such as \code{25830}.
    coord.units = "m", ##<< Coordinate units recorded in the spatial sidecar
                       ## when external coordinates are supplied.
    geometry.source = c("auto", "snfi", "external", "none"), ##<< Forwarded
                       ## to \code{readNFI_spatial()}.  Use \code{"snfi"}
                       ## to force Spanish NFI coordinate discovery and
                       ## \code{"external"} when coordinates come from the
                       ## main table or a user-supplied coordinate table.
    validate = TRUE, ##<< Warn about missing spatial helpers, failed sidecar
                     ## creation, or failed sf reconstruction.  Volume
                     ## calculations still follow \code{metrics2Vol()}.
    coord.nm = NULL, ##<< Optional SNFI coordinate table name forwarded to
                     ## \code{readNFI_spatial()}.
    coord.factor = NULL, ##<< Optional coordinate multiplier for SNFI coordinate
                         ## tables. By default IFN2 kilometre coordinates are
                         ## detected and converted to metres.
    huso.method = c("auto", "candidate", "province", "none"), ##<< UTM-zone
                         ## assignment method forwarded to
                         ## \code{readNFI_spatial()}. \code{"auto"} uses
                         ## direct \code{Huso}, then historical
                         ## \code{huso1}/\code{huso2}/\code{huso3} +
                         ## \code{CoorX}, then province fallback.
    target.crs = NULL, ##<< Optional common CRS for SNFI geometries. When
                       ## \code{NULL}, mixed ED50 zones are transformed to
                       ## EPSG:23030.
    boundary = TRUE, ##<< \code{logical}. If \code{TRUE} (default), attach an optional
                      ## administrative boundary sidecar for plot maps.
    boundary.source = c("auto", "gisco", "gadm", "user", "none"), ##<< Boundary
                      ## source passed to \code{readNFI_spatial()}.
                      ## \code{"auto"} tries GISCO/NUTS before GADM for
                      ## Spanish province-like inputs.
    boundary.object = NULL, ##<< Optional user-provided \code{sf} polygon layer.
    boundary.level = 2, ##<< GADM administrative level, used when
                        ## \code{boundary.source} resolves to \code{"gadm"}.
    boundary.path = tools::R_user_dir("basifoR", "cache"), ##<< Cache directory
                        ## for optional boundary downloads.
    boundary.ext = "json", ##<< GADM extension used by \code{gadm_spatial()}.
    boundary.version = "4.1", ##<< GADM version used by \code{gadm_spatial()}.
    boundary.crs = NULL, ##<< CRS assigned to \code{boundary.object} when missing.
    infer.huso = NULL, ##<< Backward-compatible fallback argument forwarded to
                       ## \code{readNFI_spatial()}.
    na.action = c("keep", "drop", "error") ##<< Used only when
                       ## \code{spatial = "sf"}.  It controls rows that do
                       ## not match the plot geometry registry during final
                       ## \code{sf} reconstruction.
) {
    spatial <- match.arg(spatial)
    geometry.source <- match.arg(geometry.source)
    huso.method <- match.arg(huso.method)
    boundary.source <- match.arg(boundary.source)
    na.action <- match.arg(na.action)

    ##details<< \code{metrics2Vol_spatial()} is intentionally a wrapper, not a
    ##details<< rewrite of \code{metrics2Vol()}.  It prepares or reuses a
    ##details<< plot-level geometry registry, ensures the input is safe for the
    ##details<< original tabular \code{metrics2Vol()} calculation, delegates
    ##details<< volume estimation to \code{metrics2Vol()}, and finally copies
    ##details<< the spatial registry to the result.
    ##details<<
    ##details<< The default \code{spatial = "attribute"} returns a normal
    ##details<< \code{data.frame} with \code{metrics2vol} metadata intact and
    ##details<< an extra \code{attr(x, "nfi_geometry_registry")} sidecar.
    ##details<< This is the safest mode for developing
    ##details<< \code{dendroMetrics_spatial()} and \code{inventoryMetrics_spatial()}
    ##details<< because no geometry column is passed through the volume
    ##details<< internals.  Use \code{spatial = "sf"} only when direct spatial
    ##details<< output is needed at this stage.

    if (is.null(nfi))
        return(metrics2Vol(nfi,
                           cub.met = cub.met,
                           parametro = parametro,
                           keep.var = keep.var,
                           keep.legacy = keep.legacy,
                           method_registry = method_registry,
                           track_provenance = track_provenance,
                           domheight_method = domheight_method,
                           domheight_registry = domheight_registry,
                           ...))

    spatial_run <- .basifoR_metrics2Vol_spatial_resolve_mode(
        spatial = spatial,
        nfi = nfi
    )

    if (identical(spatial_run, "none")) {
        nfi0 <- .basifoR_metrics2Vol_spatial_drop_sf(nfi)
        return(metrics2Vol(nfi0,
                           cub.met = cub.met,
                           parametro = parametro,
                           keep.var = keep.var,
                           keep.legacy = keep.legacy,
                           method_registry = method_registry,
                           track_provenance = track_provenance,
                           domheight_method = domheight_method,
                           domheight_registry = domheight_registry,
                           ...))
    }

    nfi_prepared <- .basifoR_metrics2Vol_spatial_prepare_metrics(
        nfi = nfi,
        dots = list(...),
        spatial = spatial_run,
        var = var,
        levels = levels,
        design = design,
        domheight_method = domheight_method,
        domheight_registry = domheight_registry,
        geometry.dt.nm = geometry.dt.nm,
        schema = schema,
        coords = coords,
        x.col = x.col,
        y.col = y.col,
        pr.col = pr.col,
        plot.col = plot.col,
        crs = crs,
        coord.units = coord.units,
        geometry.source = geometry.source,
        validate = validate,
        coord.nm = coord.nm,
        coord.factor = coord.factor,
        huso.method = huso.method,
        target.crs = target.crs,
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

    out <- metrics2Vol(
        nfi = nfi_prepared,
        cub.met = cub.met,
        parametro = parametro,
        keep.var = keep.var,
        keep.legacy = keep.legacy,
        method_registry = method_registry,
        track_provenance = track_provenance,
        domheight_method = domheight_method,
        domheight_registry = domheight_registry,
        ...
    )

    .basifoR_metrics2Vol_spatial_finalize(
        out = out,
        source = nfi_prepared,
        spatial = spatial_run,
        na.action = na.action,
        validate = validate
    )
    ##value<< A \code{metrics2vol} object.  With
    ## \code{spatial = "attribute"}, the result remains tabular and carries a
    ## plot-level geometry registry in \code{attr(x, "nfi_geometry_registry")}
    ## for later \code{*_spatial()} functions.  With \code{spatial = "sf"},
    ## the function attempts to return an \code{sf} object by joining plot
    ## geometries back to the tree-level volume rows.  If the volume output no
    ## longer contains a plot identifier, the function warns and returns the
    ## tabular output with the sidecar instead.
	}, ex = function() {
	    toy <- data.frame(
	        nfi.nr = 4,
	        pr = 28,
	        esta = c("P1", "P2"),
	        especie = c(21, 21),
	        Dn = c(180, 260),
	        altura = c(9.5, 14.2),
	        x = c(-3.70, -3.69),
	        y = c(40.40, 40.41)
	    )
	    attr(toy, "nfi.nr") <- 4

	    demo_registry <- snfi_volume_method_registry(list(
	        VCC = list(
	            output = "vcc_demo",
	            fun = function(dbh_mm, h_m, pars) {
	                pars$k[1] * dbh_mm^2 * h_m
	            },
	            raw_unit = "m3 tree-1",
	            unit = "m3 tree-1",
	            scale_to_m3 = 1,
	            build_args = function(ctx, pars, resolved) {
	                list(dbh_mm = ctx$d_mm, h_m = ctx$h_m, pars = pars)
	            },
	            fallback = function(ctx, pars, resolved) NA_real_,
	            pars = data.frame(
	                nfi.nr = 4,
	                pr = 28,
	                especie = 21,
	                k = 1e-7
	            )
	        )
	    ))

	    x <- metrics2Vol_spatial(
	        toy,
	        parametro = "VCC",
	        method_registry = demo_registry,
	        var = c("d", "h", "ba", "n"),
	        levels = c("esta", "especie"),
	        spatial = "attribute",
	        geometry.source = "external",
	        plot.col = "esta",
	        x.col = "x",
	        y.col = "y",
	        crs = 4326,
	        boundary = FALSE
	    )

	    inherits(x, "metrics2vol")
	    hasNFIgeometry_spatial(x)

	    ## Spanish NFI: compute tree metrics, volumes, and carry geometry as an
	    ## attribute sidecar.
	    ## x <- metrics2Vol_spatial(28, nfi.nr = 3, dir = tempdir(),
	    ##                          parametro = c("VCC", "VSC"))
	    ## inherits(x, "metrics2vol")
    ## hasNFIgeometry_spatial(x)
    ## attr(x, "units")
    ## attr(x, "volume_meta")
    ## attr(x, "dominant_height_meta")

    ## Direct sf output for visual checks.
    ## xsf <- metrics2Vol_spatial(28, nfi.nr = 3, dir = tempdir(),
    ##                            parametro = c("VCC", "VSC"),
    ##                            spatial = "sf")
    ## inherits(xsf, "sf")

    ## Continue from an existing spatial nfiMetrics object.
    ## nm <- nfiMetrics_spatial(28, nfi.nr = 3, dir = tempdir())
    ## vv <- metrics2Vol_spatial(nm, parametro = "VCC")
    ## hasNFIgeometry_spatial(vv)

    ## Disable spatial behavior and recover the ordinary metrics2Vol workflow.
    ## y <- metrics2Vol_spatial(28, nfi.nr = 3, dir = tempdir(),
    ##                          spatial = "none")
})

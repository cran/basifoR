## Extracted from basifoR_spatial_functions_v14_plotdispatch.R
## Source lines 2432-2902.
## Source readNFI_spatial_v15_coord_qa.R before this file so shared
## sidecar, GADM, and sf-reconstruction helpers are available.

## Spatial sidecar companion for nfiMetrics().
##
## This script intentionally does not replace nfiMetrics().  It defines
## nfiMetrics_spatial(), a conservative wrapper that keeps the original
## nfiMetrics() calculations unchanged and only carries the plot-level geometry
## registry created by readNFI_spatial() as an attribute sidecar.  The function
## can optionally reconstruct an sf object at the end.
##
## Expected workflow while developing the spatial branch:
##   source("readNFI_spatial_v11_boundaries.R")
##   source("nfiMetrics_spatial_v5_boundaries.R")
##
## Internal helpers use the .basifoR_nfiMetrics_spatial_* prefix to avoid
## overwriting basifoR internals and readNFI_spatial() helpers.

.basifoR_nfiMetrics_spatial_get_registry <- function(x) {
    attr(x, "nfi_geometry_registry", exact = TRUE)
}

.basifoR_nfiMetrics_spatial_has_registry <- function(x) {
    if (exists("hasNFIgeometry_spatial", mode = "function", inherits = TRUE))
        return(hasNFIgeometry_spatial(x))

    reg <- .basifoR_nfiMetrics_spatial_get_registry(x)
    !is.null(reg) && is.list(reg) && !is.null(reg$geometry)
}

.basifoR_nfiMetrics_spatial_copy_registry <- function(from, to) {
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


.basifoR_nfiMetrics_spatial_copy_custom_attrs <- function(from, to) {
    ## Preserve metadata produced by nfiMetrics(), such as units and
    ## design_meta, after an optional conversion to sf.  Do not copy base
    ## data-frame/sf structural attributes because those are rebuilt by sf.
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

.basifoR_nfiMetrics_spatial_append_class <- function(x,
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

.basifoR_nfiMetrics_spatial_resolve_mode <- function(spatial, nfi) {
    if (!identical(spatial, "inherit"))
        return(spatial)

    if (inherits(nfi, "sf"))
        return("sf")

    if (.basifoR_nfiMetrics_spatial_has_registry(nfi))
        return("attribute")

    "none"
}

.basifoR_nfiMetrics_spatial_prepare_input <- function(nfi,
                                                      dots,
                                                      spatial,
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
        return(nfi)

    if (.basifoR_nfiMetrics_spatial_has_registry(nfi) && !inherits(nfi, "sf"))
        return(nfi)

    if (!exists("readNFI_spatial", mode = "function", inherits = TRUE)) {
        if (isTRUE(validate)) {
            warning(
                paste(
                    "readNFI_spatial() was not found.",
                    "nfiMetrics_spatial() will run nfiMetrics() without a spatial sidecar."
                ),
                call. = FALSE
            )
        }
        return(nfi)
    }

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

    if (inherits(z, "error")) {
        if (isTRUE(validate)) {
            warning(
                paste(
                    "Could not prepare the spatial sidecar before nfiMetrics().",
                    conditionMessage(z),
                    "The ordinary nfiMetrics() workflow will be used."
                ),
                call. = FALSE
            )
        }
        return(nfi)
    }

    z
}

.basifoR_nfiMetrics_spatial_finalize <- function(out,
                                                 source,
                                                 spatial,
                                                 na.action = "keep",
                                                 validate = TRUE) {
    if (!is.data.frame(out))
        return(out)

    if (!identical(spatial, "none") &&
        .basifoR_nfiMetrics_spatial_has_registry(source)) {
        out <- .basifoR_nfiMetrics_spatial_copy_registry(
            from = source,
            to = out
        )
        attr(out, "spatial_metric_stage") <- "nfiMetrics_spatial"
        attr(out, "spatial_requested") <- spatial
    }

    if (identical(spatial, "sf") &&
        .basifoR_nfiMetrics_spatial_has_registry(out)) {

        if (!exists("asNFI_spatial_sf", mode = "function", inherits = TRUE)) {
            if (isTRUE(validate)) {
                warning(
                    paste(
                        "asNFI_spatial_sf() was not found.",
                        "Returning the tabular nfiMetrics output with the spatial sidecar."
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
                            "Could not reconstruct sf geometry after nfiMetrics().",
                            conditionMessage(sf_out),
                            "Returning the tabular output with the spatial sidecar."
                        ),
                        call. = FALSE
                    )
                }
            } else {
                ## sf::st_sf()/st_as_sf() rebuilds the object and may drop
                ## custom metric metadata.  Restore all non-structural
                ## attributes from the tabular nfiMetrics() output before
                ## adding spatial-stage flags.
                sf_out <- .basifoR_nfiMetrics_spatial_copy_custom_attrs(
                    from = out,
                    to = sf_out
                )

                out <- sf_out
                attr(out, "spatial_metric_stage") <- "nfiMetrics_spatial"
                attr(out, "spatial_requested") <- "sf"
                if (is.null(attr(out, "geometry_role", exact = TRUE)))
                    attr(out, "geometry_role") <- "plot_point_repeated"
            }
        }
    }

    .basifoR_nfiMetrics_spatial_append_class(out, "nfiMetrics_spatial", "nfiMetrics")
}

nfiMetrics_spatial <- structure(function#Tree-level metrics carrying a spatial sidecar
### Compute tree-level Spanish NFI metrics while preserving plot geometry
###
### This function mirrors the public arguments and calculations of
### \code{\link{nfiMetrics}} but adds an optional spatial sidecar.  The metric
### computation itself is delegated to the original \code{nfiMetrics()}, so the
### returned diameter, height, basal-area, trees-per-hectare, dominant-height,
### dominant-diameter, unit, and design metadata remain consistent with the
### non-spatial workflow.
###
### Spatial information is prepared before the metric call by invoking
### \code{readNFI_spatial()} with \code{spatial = "attribute"} and copied back to the
### metric output after \code{nfiMetrics()} finishes.  Thus the geometry column
### never travels through the tree-metric calculations.  If \code{spatial =
### "sf"}, the function reconstructs an \code{sf} point object only at the end.
(
    nfi,  ##<< \code{character(1)}, \code{"readNFI"}, \code{"readNFI_spatial"},
          ## or \code{"sf"} object.  Province codes/names and file paths are
          ## first read through \code{readNFI_spatial()} when spatial output is
          ## requested.  Objects that already carry
          ## \code{attr(x, "nfi_geometry_registry")} reuse that sidecar.
    var = c('d','h','ba','n','Hd','Dd'), ##<< Same as \code{nfiMetrics()}. Metrics
                                    ## to compute: diameter \code{'d'}, height
                                    ## \code{'h'}, basal area \code{'ba'},
                                    ## trees per hectare \code{'n'}, dominant
                                    ## height \code{'Hd'}, and dominant
                                    ## diameter \code{'Dd'}.
    levels = c('esta','espe'), ##<< Same as \code{nfiMetrics()}. Column-name
                               ## patterns used to keep grouping variables
                               ## in the tree-level output.  Spatial
                               ## reconstruction to \code{sf} requires a plot
                               ## identifier to remain in the output, so the
                               ## default should usually be kept while
                               ## developing the spatial chain.
    design = snfi_design(), ##<< Same as \code{nfiMetrics()}. Sampling design
                            ## used to compute expansion factors for
                            ## \code{'n'}, dominant height, and dominant
                            ## diameter.
    domheight_method = "Hd", ##<< Dominant-height method code resolved
                            ## through \code{domheight_registry} when
                            ## \code{'Hd'} is requested.  The default
                            ## matches \code{nfiMetrics()}.
    domheight_registry = dominant_height_method_registry(), ##<< Named
                            ## dominant-height registry created with
                            ## \code{dominant_height_method_registry()}.
    ..., ##<< Additional arguments passed to \code{readNFI_spatial()} and then
         ## to \code{nfiMetrics()}.  Typical examples are \code{nfi.nr},
         ## \code{dt.nm}, \code{dir}, \code{file_ext}, and \code{timeOut}.
    spatial = c("attribute", "sf", "none", "inherit"), ##<< Spatial output
         ## mode.  \code{"attribute"} keeps the ordinary tabular
         ## \code{nfiMetrics()} output and stores the plot geometry registry
         ## in attributes.  \code{"sf"} reconstructs point geometry at the
         ## end.  \code{"none"} runs the original non-spatial workflow.
         ## \code{"inherit"} returns \code{"sf"} for sf input,
         ## \code{"attribute"} for objects already carrying a sidecar, and
         ## \code{"none"} otherwise.
    geometry.dt.nm = NULL, ##<< Optional table name used to build the geometry
                           ## registry.  When \code{NULL}, the default of
                           ## \code{readNFI_spatial()} is used, normally the
                           ## same table requested by \code{dt.nm}.
    schema = NULL, ##<< Optional external schema forwarded to
                   ## \code{readNFI_spatial()}.  This lets compatible
                   ## non-standard inventories define spatial metadata in
                   ## \code{schema$defaults$spatial} while keeping the metric
                   ## calculation unchanged.
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
                     ## creation, or failed sf reconstruction.  Metric
                     ## calculations still follow \code{nfiMetrics()}.
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

    ##details<< \code{nfiMetrics_spatial()} is intentionally a wrapper, not a
    ##details<< rewrite of \code{nfiMetrics()}.  It first creates or reuses a
    ##details<< plot-level geometry registry using \code{readNFI_spatial()},
    ##details<< then calls the original \code{nfiMetrics()} with the same
    ##details<< \code{nfi}, \code{var}, \code{levels}, \code{design},
    ##details<< \code{domheight_method}, \code{domheight_registry}, and
    ##details<< \code{...} semantics.  After the metric output is produced,
    ##details<< the spatial registry is copied to the result.
    ##details<<
    ##details<< The default \code{spatial = "attribute"} returns a normal
    ##details<< \code{data.frame} with the \code{nfiMetrics} metadata intact
    ##details<< and an extra \code{attr(x, "nfi_geometry_registry")} sidecar.
    ##details<< This is the safest mode for developing
    ##details<< \code{metrics2Vol_spatial()} and \code{dendroMetrics_spatial()}
    ##details<< because no geometry column is passed through the metric
    ##details<< internals.  Use \code{spatial = "sf"} only when direct spatial
    ##details<< output is needed at this stage.

    if (is.null(nfi))
        return(nfiMetrics(nfi, var = var, levels = levels,
                          design = design,
                          domheight_method = domheight_method,
                          domheight_registry = domheight_registry, ...))

    spatial_run <- .basifoR_nfiMetrics_spatial_resolve_mode(
        spatial = spatial,
        nfi = nfi
    )

    nfi_prepared <- .basifoR_nfiMetrics_spatial_prepare_input(
        nfi = nfi,
        dots = list(...),
        spatial = spatial_run,
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

    out <- nfiMetrics(
        nfi = nfi_prepared,
        var = var,
        levels = levels,
        design = design,
        domheight_method = domheight_method,
        domheight_registry = domheight_registry,
        ...
    )

    .basifoR_nfiMetrics_spatial_finalize(
        out = out,
        source = nfi_prepared,
        spatial = spatial_run,
        na.action = na.action,
        validate = validate
    )
    ##value<< A \code{nfiMetrics} object.  With
    ## \code{spatial = "attribute"}, the result remains tabular and carries a
    ## plot-level geometry registry in \code{attr(x, "nfi_geometry_registry")}
    ## for later \code{*_spatial()} functions.  With \code{spatial = "sf"},
    ## the function attempts to return an \code{sf} object by joining plot
    ## geometries back to the tree-level metric rows.  If the metric output no
    ## longer contains a plot identifier, the function warns and returns the
    ## tabular output with the sidecar instead.
	}, ex = function() {
	    toy <- data.frame(
	        nfi.nr = 4,
	        pr = 28,
	        esta = c("P1", "P1", "P2"),
	        espe = c(21, 21, 25),
	        Dn = c(120, 185, 260),
	        altura = c(7.1, 9.4, 13.2),
	        x = c(-3.70, -3.70, -3.69),
	        y = c(40.40, 40.40, 40.41)
	    )
	    attr(toy, "nfi.nr") <- 4

	    x <- nfiMetrics_spatial(
	        toy,
	        var = c("d", "h", "ba", "n"),
	        levels = c("esta", "espe"),
	        spatial = "attribute",
	        geometry.source = "external",
	        plot.col = "esta",
	        x.col = "x",
	        y.col = "y",
	        crs = 4326,
	        boundary = FALSE
	    )

	    inherits(x, "nfiMetrics")
	    hasNFIgeometry_spatial(x)

	    ## Spanish NFI: compute metrics and carry geometry as an attribute sidecar.
	    ## x <- nfiMetrics_spatial(28, nfi.nr = 3, dir = tempdir())
	    ## inherits(x, "nfiMetrics")
	    ## hasNFIgeometry_spatial(x)
    ## reg <- getNFIgeometry_spatial(x)
    ## reg$geometry
    ## attr(x, "dominant_height_meta")

    ## Direct sf output for visual checks.
    ## xsf <- nfiMetrics_spatial(28, nfi.nr = 3, dir = tempdir(),
    ##                           spatial = "sf")
    ## inherits(xsf, "sf")

    ## Disable spatial behavior and recover the ordinary nfiMetrics workflow.
	    ## y <- nfiMetrics_spatial(28, nfi.nr = 3, dir = tempdir(),
	    ##                         spatial = "none")
	})

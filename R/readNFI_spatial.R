## Spatial sidecar prototype for basifoR.
##
## This file intentionally does not replace readNFI().  It defines
## readNFI_spatial(), a conservative wrapper that can keep the ordinary
## readNFI() tabular output while storing plot geometries in an attribute,
## or return an sf object when spatial = "sf".
## It handles Spanish NFI geometry through readNFIsf() and external
## inventories through user-supplied sf objects or coordinate columns.
## Internal helpers use the .basifoR_spatial_* prefix to avoid collisions
## with existing .nfi_* helpers in readNFIsf.R.


.basifoR_spatial_null_or <- function(x, y) {
    if (is.null(x)) y else x
}

.basifoR_spatial_snfi_coord_accuracy <- function(nfi.nr) {
    nfi.nr <- suppressWarnings(as.integer(nfi.nr)[1L])

    if (identical(nfi.nr, 2L))
        return("official_public_kilometre_grid_location")
    if (identical(nfi.nr, 3L))
        return("official_public_cartographic_plot_location")
    if (identical(nfi.nr, 4L))
        return("official_public_pre_fieldwork_plot_location")

    "official_public_plot_location"
}

.basifoR_spatial_snfi_coord_accuracy_note <- function(nfi.nr) {
    nfi.nr <- suppressWarnings(as.integer(nfi.nr)[1L])

    if (identical(nfi.nr, 2L)) {
        return(paste(
            "IFN2 public coordinates are the official inventory plot-location",
            "coordinates derived from the kilometre UTM sampling grid. They are",
            "suitable for georeferenced database construction and spatial overlay,",
            "but should not be described as surveyed exact field-centre coordinates."
        ))
    }

    if (identical(nfi.nr, 3L)) {
        return(paste(
            "IFN3 public coordinates are official cartographic plot-location",
            "coordinates in UTM metres, derived from the inventory mapping",
            "workflow. They are suitable for georeferenced database construction",
            "and spatial overlay. When the Huso is not explicit, basifoR may infer",
            "it from candidate Huso fields, coordinate rules, or fallback methods;",
            "such inferred CRS metadata should be treated as reconstructed rather",
            "than directly observed."
        ))
    }

    if (identical(nfi.nr, 4L)) {
        return(paste(
            "IFN4 CoorX/CoorY are official public plot-location coordinates",
            "documented as coordinates available before fieldwork for the plot",
            "centre. They are suitable for georeferenced database construction",
            "and spatial overlay, but should not be documented as field-validated",
            "exact plot-centre coordinates unless replaced by an external source."
        ))
    }

    paste(
        "Spanish NFI coordinates stored by basifoR are official public",
        "plot-location coordinates distributed with the inventory. Coordinate",
        "precision and interpretation depend on the processed inventory stage. Use",
        "external coordinates when field-validated plot-centre locations are available."
    )
}


.basifoR_spatial_inherit_class <- function(x, spatial_class, old_class) {
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

.basifoR_spatial_schema_spatial <- function(schema = NULL) {
    if (is.null(schema))
        return(NULL)

    if (!inherits(schema, "external_schema"))
        stop("'schema' must inherit from 'external_schema' when supplied.",
             call. = FALSE)

    sp <- NULL

    ## Preferred future location if new_external_schema() is later extended.
    if (!is.null(schema$spatial))
        sp <- schema$spatial

    ## Backward-compatible location supported by the current constructor.
    if (is.null(sp) && !is.null(schema$defaults) &&
        !is.null(schema$defaults$spatial))
        sp <- schema$defaults$spatial

    if (is.null(sp))
        return(NULL)

    if (!is.list(sp))
        stop("schema spatial metadata must be a list.", call. = FALSE)

    sp
}

.basifoR_spatial_schema_value <- function(sp, names, default = NULL) {
    if (is.null(sp))
        return(default)

    for (nm in names) {
        if (!is.null(sp[[nm]]))
            return(sp[[nm]])
    }

    default
}

.basifoR_spatial_first_schema_alias <- function(schema, key) {
    if (is.null(schema) || is.null(schema$colmap) || is.null(schema$colmap[[key]]))
        return(NULL)

    z <- schema$colmap[[key]]
    z <- as.character(z)
    z <- z[!is.na(z) & nzchar(z)]
    if (!length(z))
        return(NULL)

    z[1L]
}

.basifoR_spatial_first_col <- function(x, candidates) {
    nm <- names(x)
    hit <- match(tolower(candidates), tolower(nm))
    hit <- hit[!is.na(hit)]
    if (!length(hit))
        return(NA_character_)
    nm[hit[1L]]
}

.basifoR_spatial_numeric <- function(x) {
    if (is.factor(x))
        x <- as.character(x)
    if (is.character(x))
        x <- gsub(",", ".", trimws(x), fixed = TRUE)
    suppressWarnings(as.numeric(x))
}

.basifoR_spatial_key_part <- function(x) {
    x0 <- x
    x <- trimws(as.character(x))
    xn <- .basifoR_spatial_numeric(x)
    ok <- !is.na(xn)
    out <- x
    out[ok] <- format(xn[ok], scientific = FALSE, trim = TRUE)
    out[is.na(x0)] <- NA_character_
    out
}

.basifoR_spatial_plot_cols <- function(x, allow_missing_province = FALSE) {
    pr_col <- .basifoR_spatial_first_col(
        x,
        c("pr", "Provincia", "PROVINCIA", "NPROV", "nprov", "prov")
    )

    plot_col <- .basifoR_spatial_first_col(
        x,
        c("Estadillo", "ESTADILLO", "estadillo", "NUMPAR", "numpar",
          "plot", "plot_id", "idp")
    )

    if (is.na(plot_col)) {
        stop(
            paste(
                "Cannot build spatial sidecar because the plot identifier",
                "was not found. Expected a column such as 'Estadillo',",
                "'ESTADILLO', 'NUMPAR', 'plot', or 'idp'."
            ),
            call. = FALSE
        )
    }

    if (is.na(pr_col) && !allow_missing_province) {
        stop(
            paste(
                "Cannot build spatial sidecar because the province identifier",
                "was not found. Expected a column such as 'pr', 'Provincia',",
                "'PROVINCIA', or 'NPROV'."
            ),
            call. = FALSE
        )
    }

    list(pr_col = pr_col, plot_col = plot_col)
}

.basifoR_spatial_plot_keys <- function(x, allow_missing_province = FALSE) {
    cols <- .basifoR_spatial_plot_cols(
        x,
        allow_missing_province = allow_missing_province
    )

    data.frame(
        .nfi_pr = if (is.na(cols$pr_col)) NA_character_
                  else .basifoR_spatial_key_part(x[[cols$pr_col]]),
        .nfi_plot = .basifoR_spatial_key_part(x[[cols$plot_col]]),
        stringsAsFactors = FALSE
    )
}

.basifoR_spatial_make_key <- function(keys, use_pr = TRUE) {
    if (isTRUE(use_pr))
        return(paste(keys$.nfi_pr, keys$.nfi_plot, sep = "\r"))
    keys$.nfi_plot
}

.basifoR_spatial_select_col <- function(x, col = NULL, candidates = character(0),
                                      required = FALSE, label = "column") {
    if (!is.null(col)) {
        if (!col %in% names(x))
            stop("'", col, "' was not found as ", label, ".", call. = FALSE)
        return(col)
    }

    hit <- .basifoR_spatial_first_col(x, candidates)
    if (is.na(hit) && isTRUE(required))
        stop("Could not detect ", label, ". Specify it explicitly.", call. = FALSE)
    hit
}

.basifoR_spatial_plot_cols2 <- function(x, pr.col = NULL, plot.col = NULL,
                                        allow_missing_province = TRUE) {
    pr_hit <- .basifoR_spatial_select_col(
        x,
        col = pr.col,
        candidates = c("pr", "Provincia", "PROVINCIA", "NPROV", "nprov", "prov",
                       "province", "province_id", "region", "region_id"),
        required = FALSE,
        label = "province/group key column"
    )

    plot_hit <- .basifoR_spatial_select_col(
        x,
        col = plot.col,
        candidates = c("Estadillo", "ESTADILLO", "estadillo", "NUMPAR", "numpar",
                       "plot", "plot_id", "plotid", "idp", "sample_plot",
                       "sample_id", "site", "site_id", "cluster", "cluster_id"),
        required = TRUE,
        label = "plot identifier column"
    )

    if (is.na(pr_hit) && !allow_missing_province) {
        stop(
            paste(
                "Cannot build spatial sidecar because the province/group identifier",
                "was not found. Use pr.col to specify it explicitly."
            ),
            call. = FALSE
        )
    }

    list(pr_col = pr_hit, plot_col = plot_hit)
}

.basifoR_spatial_xy_cols <- function(x, x.col = NULL, y.col = NULL) {
    x_hit <- .basifoR_spatial_select_col(
        x,
        col = x.col,
        candidates = c("x", "X", "coord_x", "coordx", "CoordX", "CoorX", "COORDEX",
                       "easting", "Easting", "utm_x", "UTMX", "lon", "long",
                       "longitude", "Longitude"),
        required = TRUE,
        label = "X/easting/longitude coordinate column"
    )

    y_hit <- .basifoR_spatial_select_col(
        x,
        col = y.col,
        candidates = c("y", "Y", "coord_y", "coordy", "CoordY", "CoorY", "COORDEY",
                       "northing", "Northing", "utm_y", "UTMY", "lat", "latitude",
                       "Latitude"),
        required = TRUE,
        label = "Y/northing/latitude coordinate column"
    )

    list(x = x_hit, y = y_hit)
}

.basifoR_spatial_probably_snfi_source <- function(nfi) {
    if (is.numeric(nfi) && length(nfi) == 1L && !is.na(nfi))
        return(TRUE)

    if (!is.character(nfi) || length(nfi) != 1L || is.na(nfi))
        return(FALSE)

    z <- trimws(nfi)
    if (grepl("^[0-9]{1,2}$", z))
        return(TRUE)

    ## A single province name is likely an SNFI source. Existing file paths and
    ## URLs are ambiguous, so auto mode does not force SNFI geometry for them.
    if (!file.exists(z) && !grepl("^(https?|ftp)://", z, ignore.case = TRUE) &&
        !grepl("\\.(zip|csv|dbf|mdb|accdb)$", z, ignore.case = TRUE))
        return(TRUE)

    FALSE
}

.basifoR_spatial_registry_from_sf <- function(x, source = "sf_input",
                                              source_dt.nm = NULL,
                                              validate = TRUE,
                                              pr.col = NULL,
                                              plot.col = NULL) {
    if (!requireNamespace("sf", quietly = TRUE)) {
        stop("Package 'sf' is required to build the spatial sidecar.",
             call. = FALSE)
    }

    if (!inherits(x, "sf"))
        stop("'x' must inherit from 'sf'.", call. = FALSE)

    tab <- sf::st_drop_geometry(x)
    cols <- .basifoR_spatial_plot_cols2(
        tab,
        pr.col = pr.col,
        plot.col = plot.col,
        allow_missing_province = TRUE
    )
    keys <- data.frame(
        .nfi_pr = if (is.na(cols$pr_col)) NA_character_
                  else .basifoR_spatial_key_part(tab[[cols$pr_col]]),
        .nfi_plot = .basifoR_spatial_key_part(tab[[cols$plot_col]]),
        stringsAsFactors = FALSE
    )

    use_pr <- any(!is.na(keys$.nfi_pr))
    key <- .basifoR_spatial_make_key(keys, use_pr = use_pr)

    keep <- !duplicated(key) & !is.na(keys$.nfi_plot)
    if (isTRUE(validate) && any(duplicated(key[!is.na(key)]))) {
        ndup <- length(unique(key[duplicated(key) & !is.na(key)]))
        warning(
            ndup,
            " duplicated plot key(s) were found while building the spatial sidecar. ",
            "Only the first geometry per plot is stored.",
            call. = FALSE
        )
    }

    reg_tab <- data.frame(
        .nfi_pr = keys$.nfi_pr[keep],
        .nfi_plot = keys$.nfi_plot[keep],
        stringsAsFactors = FALSE
    )

    if (!is.na(cols$pr_col))
        reg_tab[[cols$pr_col]] <- tab[[cols$pr_col]][keep]
    reg_tab[[cols$plot_col]] <- tab[[cols$plot_col]][keep]

    reg <- sf::st_sf(
        reg_tab,
        geometry = sf::st_geometry(x)[keep],
        crs = sf::st_crs(x)
    )

    list(
        version = 1L,
        geometry = reg,
        key_cols = cols,
        key_uses_province = use_pr,
        normalized_key_cols = c(".nfi_pr", ".nfi_plot"),
        geometry_level = "plot",
        geometry_role = "plot_registry",
        source = source,
        source_dt.nm = source_dt.nm,
        coord_table = attr(x, "coord_table", exact = TRUE),
        coord_reference = attr(x, "coord_reference", exact = TRUE),
        coord_type = attr(x, "coord_type", exact = TRUE),
        coord_units = attr(x, "coord_units", exact = TRUE),
        coord_source_columns = attr(x, "coord_source_columns", exact = TRUE),
        coord_factor = attr(x, "coord_factor", exact = TRUE),
        coord_source_units = attr(x, "coord_source_units", exact = TRUE),
        sf_crs = sf::st_crs(x),
        sf_crs_assigned = attr(x, "sf_crs_assigned", exact = TRUE),
        coord_accuracy = .basifoR_spatial_null_or(
            attr(x, "coord_accuracy", exact = TRUE),
            "source_spatial_object"
        ),
        coord_accuracy_note = attr(x, "coord_accuracy_note", exact = TRUE),
        coord_update_supported = TRUE,
        nfi.nr = attr(x, "nfi.nr", exact = TRUE),
        created = Sys.time()
    )
}

.basifoR_spatial_registry_from_xy <- function(x, coords = NULL,
                                             x.col = NULL, y.col = NULL,
                                             pr.col = NULL, plot.col = NULL,
                                             crs = NULL,
                                             coord.units = "m",
                                             coord.accuracy.note = NULL,
                                             source = "external_xy",
                                             source_dt.nm = NULL,
                                             validate = TRUE) {
    if (!requireNamespace("sf", quietly = TRUE)) {
        stop("Package 'sf' is required to build the spatial sidecar.",
             call. = FALSE)
    }

    z <- if (is.null(coords)) x else coords

    if (inherits(z, "sf")) {
        reg <- .basifoR_spatial_registry_from_sf(
            z,
            source = source,
            source_dt.nm = source_dt.nm,
            validate = validate,
            pr.col = pr.col,
            plot.col = plot.col
        )
        reg$coord_units <- coord.units
        reg$coord_type <- if (is.null(crs)) NULL else "user_supplied"
        reg$coord_accuracy <- "user_supplied"
        reg$coord_accuracy_note <- coord.accuracy.note
        reg$coord_update_supported <- TRUE
        return(reg)
    }

    if (!is.data.frame(z))
        stop("'coords' must be NULL, a data.frame, or an sf object.", call. = FALSE)

    cols <- .basifoR_spatial_plot_cols2(
        z,
        pr.col = pr.col,
        plot.col = plot.col,
        allow_missing_province = TRUE
    )
    xy <- .basifoR_spatial_xy_cols(z, x.col = x.col, y.col = y.col)

    keys <- data.frame(
        .nfi_pr = if (is.na(cols$pr_col)) NA_character_
                  else .basifoR_spatial_key_part(z[[cols$pr_col]]),
        .nfi_plot = .basifoR_spatial_key_part(z[[cols$plot_col]]),
        stringsAsFactors = FALSE
    )

    use_pr <- any(!is.na(keys$.nfi_pr))
    key <- .basifoR_spatial_make_key(keys, use_pr = use_pr)
    xnum <- .basifoR_spatial_numeric(z[[xy$x]])
    ynum <- .basifoR_spatial_numeric(z[[xy$y]])

    keep <- !duplicated(key) & !is.na(keys$.nfi_plot) & !is.na(xnum) & !is.na(ynum)
    if (isTRUE(validate) && any(duplicated(key[!is.na(key)]))) {
        ndup <- length(unique(key[duplicated(key) & !is.na(key)]))
        warning(
            ndup,
            " duplicated plot key(s) were found while building the external spatial sidecar. ",
            "Only the first geometry per plot is stored.",
            call. = FALSE
        )
    }
    if (isTRUE(validate) && !any(keep)) {
        warning(
            "No valid external plot coordinates were found for the spatial sidecar.",
            call. = FALSE
        )
    }

    reg_tab <- data.frame(
        .nfi_pr = keys$.nfi_pr[keep],
        .nfi_plot = keys$.nfi_plot[keep],
        stringsAsFactors = FALSE
    )
    if (!is.na(cols$pr_col))
        reg_tab[[cols$pr_col]] <- z[[cols$pr_col]][keep]
    reg_tab[[cols$plot_col]] <- z[[cols$plot_col]][keep]
    reg_tab[[xy$x]] <- xnum[keep]
    reg_tab[[xy$y]] <- ynum[keep]

    reg <- sf::st_as_sf(
        reg_tab,
        coords = c(xy$x, xy$y),
        crs = crs,
        remove = TRUE,
        na.fail = FALSE
    )

    list(
        version = 1L,
        geometry = reg,
        key_cols = cols,
        key_uses_province = use_pr,
        normalized_key_cols = c(".nfi_pr", ".nfi_plot"),
        geometry_level = "plot",
        geometry_role = "plot_registry",
        source = source,
        source_dt.nm = source_dt.nm,
        coord_table = if (is.null(coords)) "main_table" else "coords",
        coord_reference = NULL,
        coord_type = "external_user_coordinates",
        coord_units = coord.units,
        coord_source_columns = xy,
        coord_factor = 1,
        coord_source_units = coord.units,
        sf_crs = sf::st_crs(reg),
        sf_crs_assigned = crs,
        coord_accuracy = "user_supplied",
        coord_accuracy_note = coord.accuracy.note,
        coord_update_supported = TRUE,
        nfi.nr = attr(x, "nfi.nr", exact = TRUE),
        created = Sys.time()
    )
}

.basifoR_spatial_attach_registry <- function(x, registry,
                                             requested = "attribute") {
    if (is.null(registry)) {
        attr(x, "has_nfi_geometry") <- FALSE
        return(x)
    }

    attr(x, "nfi_geometry_registry") <- registry
    attr(x, "has_nfi_geometry") <- TRUE
    attr(x, "spatial_requested") <- requested
    x <- .basifoR_spatial_inherit_class(x, "readNFI_spatial", "readNFI")
    x
}

.basifoR_spatial_copy_attrs <- function(from, to) {
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

### Return the plot-geometry sidecar stored in a spatial NFI object.
getNFIgeometry_spatial <- function(
    x ##<< Object from which to retrieve the plot-geometry sidecar stored in the \code{nfi_geometry_registry} attribute..
) {
    ## Return the plot-level geometry registry stored by readNFI_spatial().
    ## This is a convenience accessor so downstream *_spatial functions do not
    ## need to know the exact attribute name.
    attr(x, "nfi_geometry_registry", exact = TRUE)
}

### Test whether an object contains a valid plot-geometry sidecar.
hasNFIgeometry_spatial <- function(
    x ##<< Object to test for a valid, non-empty plot-geometry sidecar.
) {
    ## Test whether an object carries a non-empty spatial sidecar.
    reg <- getNFIgeometry_spatial(x)
    !is.null(reg) && is.list(reg) && inherits(reg$geometry, "sf")
}

### Copy the plot-geometry sidecar between NFI objects.
copyNFIgeometry_spatial <- function(
    from, ##<< Source object carrying plot-geometry and related spatial attributes.
    to ##<< Target object that will receive the spatial attributes.
) {
    ## Copy the spatial sidecar from one object to another.  Use this at the
    ## end of nfiMetrics_spatial(), metrics2Vol_spatial(), and
    ## dendroMetrics_spatial() when those functions rebuild data frames.
    .basifoR_spatial_copy_attrs(from = from, to = to)
}


.basifoR_spatial_empty_sfc <- function(n, crs) {
    if (!requireNamespace("sf", quietly = TRUE)) {
        stop("Package 'sf' is required to create sf output.", call. = FALSE)
    }

    sf::st_sfc(
        lapply(seq_len(n), function(i) sf::st_point()),
        crs = crs
    )
}

### Reconstruct an sf object from tabular NFI data and its geometry sidecar.
asNFI_spatial_sf <- function(
    x, ##<< Tabular NFI object to convert to an \code{sf} object.
    registry = getNFIgeometry_spatial(x), ##<< Plot-geometry registry used to match one geometry to each row.
    na.action = c("keep", "drop", "error"), ##<< Action for rows whose plot key has no matching geometry: retain, remove, or raise an error.
    validate = TRUE ##<< Logical; whether to validate registry keys and geometry consistency before reconstruction.
) {
    ## Convert a tabular object carrying attr(x, "nfi_geometry_registry")
    ## into an sf object.  The conversion joins one plot geometry to each row
    ## by the normalized plot keys stored in the registry.  It is intended as
    ## an explicit final step, so metric workflows can remain tabular while
    ## still carrying the sidecar.
    ##
    ## Important for external inventories: do not assume Spanish column names.
    ## The registry stores the key columns used to build the sidecar, e.g.
    ## pr_col = "CAMPAGNE" and plot_col = "IDP" for French NFI examples.
    na.action <- match.arg(na.action)

    if (!requireNamespace("sf", quietly = TRUE)) {
        stop("Package 'sf' is required to create sf output.", call. = FALSE)
    }

    if (is.null(registry) || !is.list(registry) ||
        !inherits(registry$geometry, "sf")) {
        stop(
            paste(
                "No valid spatial sidecar was found.",
                "Use readNFI_spatial(..., spatial = 'attribute') first,",
                "or supply a valid geometry registry."
            ),
            call. = FALSE
        )
    }

    if (inherits(x, "sf"))
        x <- sf::st_drop_geometry(x)

    use_pr <- isTRUE(registry$key_uses_province)

    key_cols <- registry$key_cols
    if (is.null(key_cols) || is.null(key_cols$plot_col) || is.na(key_cols$plot_col)) {
        key_cols <- .basifoR_spatial_plot_cols2(
            x,
            allow_missing_province = !use_pr
        )
    }

    ## Resolve the registry key columns against the current table by name,
    ## case-insensitively.  This allows external keys such as CAMPAGNE + IDP.
    pr_col <- if (!is.null(key_cols$pr_col)) key_cols$pr_col else NA_character_
    plot_col <- key_cols$plot_col

    if (!is.na(plot_col) && !plot_col %in% names(x)) {
        hit <- .basifoR_spatial_first_col(x, plot_col)
        plot_col <- hit
    }
    if (is.na(plot_col) || !plot_col %in% names(x)) {
        stop(
            paste(
                "Cannot attach sf geometry because the plot key stored in",
                "the spatial sidecar was not found in the current object."
            ),
            call. = FALSE
        )
    }

    if (!is.na(pr_col) && !pr_col %in% names(x)) {
        hit <- .basifoR_spatial_first_col(x, pr_col)
        pr_col <- hit
    }
    if (isTRUE(use_pr) && (is.na(pr_col) || !pr_col %in% names(x))) {
        stop(
            paste(
                "Cannot attach sf geometry because the group/province key",
                "stored in the spatial sidecar was not found in the current object."
            ),
            call. = FALSE
        )
    }

    keys_x <- data.frame(
        .nfi_pr = if (isTRUE(use_pr)) .basifoR_spatial_key_part(x[[pr_col]]) else NA_character_,
        .nfi_plot = .basifoR_spatial_key_part(x[[plot_col]]),
        stringsAsFactors = FALSE
    )

    geo <- registry$geometry
    geo_tab <- sf::st_drop_geometry(geo)
    if (!all(c(".nfi_plot") %in% names(geo_tab))) {
        stop(
            "The spatial sidecar does not contain normalized plot keys.",
            call. = FALSE
        )
    }

    keys_geo <- data.frame(
        .nfi_pr = if (".nfi_pr" %in% names(geo_tab)) geo_tab$.nfi_pr else NA_character_,
        .nfi_plot = geo_tab$.nfi_plot,
        stringsAsFactors = FALSE
    )

    kx <- .basifoR_spatial_make_key(keys_x, use_pr = use_pr)
    kg <- .basifoR_spatial_make_key(keys_geo, use_pr = use_pr)
    idx <- match(kx, kg)

    missing <- is.na(idx)
    if (any(missing)) {
        msg <- paste0(sum(missing), " row(s) did not match the spatial sidecar.")
        if (identical(na.action, "error"))
            stop(msg, call. = FALSE)
        if (identical(na.action, "drop")) {
            if (isTRUE(validate))
                warning(paste(msg, "These rows were dropped before sf conversion."),
                        call. = FALSE)
            x <- x[!missing, , drop = FALSE]
            idx <- idx[!missing]
            missing <- missing[!missing]
        } else if (isTRUE(validate)) {
            warning(paste(msg, "They are kept with empty geometries."),
                    call. = FALSE)
        }
    }

    geom <- .basifoR_spatial_empty_sfc(nrow(x), crs = sf::st_crs(geo))
    ok <- !is.na(idx)
    if (any(ok))
        geom[ok] <- sf::st_geometry(geo)[idx[ok]]

    extra_attrs <- attributes(x)
    extra_attrs <- extra_attrs[setdiff(names(extra_attrs),
                                       c("names", "row.names", "class", "sf_column", "agr"))]

    out <- sf::st_sf(x, geometry = geom, crs = sf::st_crs(geo))

    for (nm in names(extra_attrs))
        attr(out, nm) <- extra_attrs[[nm]]

    attr(out, "nfi_geometry_registry") <- registry
    attr(out, "has_nfi_geometry") <- TRUE
    attr(out, "spatial_requested") <- "sf"
    attr(out, "geometry_role") <- "plot_point_repeated"
    attr(out, "geometry_level") <- "row_from_plot_registry"
    out <- .basifoR_spatial_inherit_class(out, "readNFI_spatial", "readNFI")
    out
}

.basifoR_spatial_input_province <- function(nfi) {
    if (is.numeric(nfi) && length(nfi) == 1L && !is.na(nfi))
        return(as.integer(nfi))

    if (is.character(nfi) && length(nfi) == 1L) {
        z <- trimws(nfi)
        if (grepl("^[0-9]+$", z))
            return(as.integer(z))
    }

    NA_integer_
}


.basifoR_spatial_first_col <- function(x, candidates) {
    nm <- names(x)
    hit <- match(tolower(candidates), tolower(nm))
    hit <- hit[!is.na(hit)]
    if (!length(hit))
        return(NA_character_)
    nm[hit[1L]]
}

.basifoR_spatial_actual_province_values <- function(x) {
    if (is.null(x))
        return(NULL)

    if (is.list(x) && !is.data.frame(x) && !is.null(x$geometry))
        x <- x$geometry

    if (inherits(x, "sf"))
        x <- sf::st_drop_geometry(x)

    if (!is.data.frame(x))
        return(NULL)

    pr_col <- .basifoR_spatial_first_col(
        x,
        c("pr", "Provincia", "PROVINCIA", "NPROV", "nprov", "prov")
    )

    if (is.na(pr_col))
        return(NULL)

    pr <- x[[pr_col]]
    pr <- pr[!is.na(pr)]
    if (!length(pr))
        return(NULL)

    pr <- unique(trimws(as.character(pr)))
    pr <- pr[nzchar(pr)]
    if (!length(pr))
        return(NULL)

    pr
}

.basifoR_spatial_boundary_context <- function(data = NULL,
                                             registry = NULL,
                                             fallback = NULL) {
    ## Resolve the boundary from the province identifiers actually present in
    ## the imported table or in the plot-geometry registry.  This respects
    ## basifoR/procods data-assignment decisions, for example when a requested
    ## province is read from an operational source associated with another
    ## province or a multi-province file.  Only if no province is visible in
    ## the data do we fall back to the original user input.
    pr <- .basifoR_spatial_actual_province_values(registry)
    if (is.null(pr))
        pr <- .basifoR_spatial_actual_province_values(data)
    if (!is.null(pr))
        return(pr)
    fallback
}

.basifoR_spatial_clean_huso <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x[x %in% c("", "NA", "NaN", "NULL", "-")] <- NA_character_
    suppressWarnings(as.integer(x))
}

.basifoR_spatial_candidate_huso_cols <- function(x) {
    list(
        huso1 = .basifoR_spatial_first_col(x, c("huso1", "Huso1", "HUSO1", "huso_1", "HUSO_1")),
        huso2 = .basifoR_spatial_first_col(x, c("huso2", "Huso2", "HUSO2", "huso_2", "HUSO_2")),
        huso3 = .basifoR_spatial_first_col(x, c("huso3", "Huso3", "HUSO3", "huso_3", "HUSO_3"))
    )
}


.basifoR_spatial_has_candidate_huso <- function(x) {
    hc <- .basifoR_spatial_candidate_huso_cols(x)
    any(!vapply(hc, is.na, logical(1)))
}

.basifoR_spatial_candidate_matrix <- function(coords, n = nrow(coords)) {
    hc <- .basifoR_spatial_candidate_huso_cols(coords)
    get_h <- function(nm) {
        if (is.na(nm))
            return(rep(NA_integer_, n))
        .basifoR_spatial_clean_huso(coords[[nm]])
    }

    cbind(
        huso1 = get_h(hc$huso1),
        huso2 = get_h(hc$huso2),
        huso3 = get_h(hc$huso3)
    )
}

.basifoR_spatial_ifn2_default_candidates <- function(pr) {
    ## Conservative built-in candidates for provinces observed to span UTM
    ## zones in SNFI coordinate tables or legacy IFN workflows. Users can
    ## override this with huso.candidates.  The row-wise rule follows the
    ## historical IFN handling: for candidate zones 29/30, CoorX > 500000
    ## belongs to zone 29 and lower CoorX to zone 30; for 30/31, CoorX <
    ## 500000 belongs to zone 31 and the remaining points to zone 30.
    pr <- sprintf("%02d", as.integer(.basifoR_spatial_numeric(pr)))
    out <- vector("list", length(pr))
    for (i in seq_along(pr)) {
        ## out[[i]] <- NULL
        if (pr[i] %in% c("32", "49"))
            out[[i]] <- c(29L, 30L)
        if (pr[i] %in% c("50"))
            out[[i]] <- c(30L, 31L)
    }
    out
}

.basifoR_spatial_candidates_to_matrix <- function(candidates, n) {
    if (is.null(candidates))
        return(matrix(NA_integer_, nrow = n, ncol = 3,
                      dimnames = list(NULL, c("huso1", "huso2", "huso3"))))

    if (is.numeric(candidates) || is.character(candidates)) {
        z <- suppressWarnings(as.integer(candidates))
        z <- z[!is.na(z)]
        mat <- matrix(NA_integer_, nrow = n, ncol = 3,
                      dimnames = list(NULL, c("huso1", "huso2", "huso3")))
        if (length(z))
            mat[, seq_len(min(3L, length(z)))] <- matrix(z[seq_len(min(3L, length(z)))],
                                                        nrow = n,
                                                        ncol = min(3L, length(z)),
                                                        byrow = TRUE)
        return(mat)
    }

    if (is.list(candidates)) {
        mat <- matrix(NA_integer_, nrow = n, ncol = 3,
                      dimnames = list(NULL, c("huso1", "huso2", "huso3")))
        for (i in seq_len(n)) {
            z <- candidates[[i]]
            z <- suppressWarnings(as.integer(z))
            z <- z[!is.na(z)]
            if (length(z))
                mat[i, seq_len(min(3L, length(z)))] <- z[seq_len(min(3L, length(z)))]
        }
        return(mat)
    }

    stop("'huso.candidates' must be NULL, an integer vector, or a list.",
         call. = FALSE)
}

.basifoR_spatial_infer_huso_from_candidate_matrix <- function(cand_mat, x_m,
                                                              source = "candidate_huso_CoorX_rule",
                                                              threshold = 500000) {
    if (is.null(cand_mat) || !nrow(cand_mat))
        return(NULL)

    x_m <- .basifoR_spatial_numeric(x_m)
    huso <- rep(NA_integer_, nrow(cand_mat))

    for (i in seq_len(nrow(cand_mat))) {
        cand <- unique(as.integer(cand_mat[i, ]))
        cand <- cand[!is.na(cand)]
        if (!length(cand))
            next

        cand <- sort(cand)
        if (length(cand) == 1L) {
            huso[i] <- cand[1L]
            next
        }

        ## Historical IFN rule for adjacent zones.  In the western zone of a
        ## two-zone pair, easting values are generally > 500000; in the eastern
        ## zone, they are generally < 500000.  This reproduces the old f.huso()
        ## logic for 29/30 and 30/31 without hard-coding only those pairs.
        if (length(cand) >= 2L && is.finite(x_m[i])) {
            if (x_m[i] > threshold) {
                huso[i] <- min(cand)
            } else if (x_m[i] < threshold) {
                huso[i] <- max(cand)
            } else {
                huso[i] <- cand[ceiling(length(cand) / 2)]
            }
            next
        }

        ## If CoorX is unavailable, use the central/standard zone when present.
        huso[i] <- if (30L %in% cand) 30L else cand[ceiling(length(cand) / 2)]
    }

    data.frame(
        huso = as.integer(huso),
        huso_source = source,
        stringsAsFactors = FALSE
    )
}

.basifoR_spatial_infer_huso_from_candidates <- function(coords, x_m) {
    if (!.basifoR_spatial_has_candidate_huso(coords))
        return(NULL)
    mat <- .basifoR_spatial_candidate_matrix(coords, n = nrow(coords))
    .basifoR_spatial_infer_huso_from_candidate_matrix(
        mat,
        x_m = x_m,
        source = "candidate_huso_CoorX_rule"
    )
}

.basifoR_spatial_infer_huso_from_pr_candidates <- function(pr, x_m,
                                                           huso.candidates = NULL) {
    n <- length(x_m)
    if (!is.null(huso.candidates)) {
        mat <- .basifoR_spatial_candidates_to_matrix(huso.candidates, n = n)
        return(.basifoR_spatial_infer_huso_from_candidate_matrix(
            mat,
            x_m = x_m,
            source = "user_candidate_huso_CoorX_rule"
        ))
    }

    cand_list <- .basifoR_spatial_ifn2_default_candidates(pr)
    if (!length(cand_list) || !any(vapply(cand_list, length, integer(1)) > 0L))
        return(NULL)

    mat <- .basifoR_spatial_candidates_to_matrix(cand_list, n = n)
    .basifoR_spatial_infer_huso_from_candidate_matrix(
        mat,
        x_m = x_m,
        source = "province_candidate_huso_CoorX_rule"
    )
}

.basifoR_spatial_huso_from_province <- function(pr) {
    pr <- sprintf("%02d", as.integer(.basifoR_spatial_numeric(pr)))
    out <- rep(NA_integer_, length(pr))
    out[!is.na(pr)] <- 30L
    out[pr %in% c("35", "38")] <- 28L
    out
}


.basifoR_spatial_invalid_snfi_coord <- function(x_m, y_m, x_raw = NULL, y_raw = NULL) {
    ## Detect missing or placeholder coordinates before building sf geometry.
    ## IFN coordinate tables may contain public placeholder values such as
    ## 999999 / 9999999; those are not valid plot locations and should not be
    ## transformed as real UTM coordinates.
    x_m <- .basifoR_spatial_numeric(x_m)
    y_m <- .basifoR_spatial_numeric(y_m)

    bad <- is.na(x_m) | is.na(y_m) | !is.finite(x_m) | !is.finite(y_m) |
        x_m <= 0 | y_m <= 0

    rx <- round(abs(x_m))
    ry <- round(abs(y_m))
    bad <- bad |
        rx %in% c(99999L, 999999L, 9999999L, 99999999L, 999999000L) |
        ry %in% c(999999L, 9999999L, 99999999L, 999999999L, 9999999000)

    if (!is.null(x_raw) && !is.null(y_raw)) {
        xr <- round(abs(.basifoR_spatial_numeric(x_raw)))
        yr <- round(abs(.basifoR_spatial_numeric(y_raw)))
        bad <- bad |
            xr %in% c(999L, 9999L, 99999L, 999999L, 9999999L) |
            yr %in% c(999L, 9999L, 99999L, 999999L, 9999999L)
    }

    bad
}

.basifoR_spatial_candidate_matrix_from_table <- function(pr, huso) {
    n <- length(huso)
    mat <- matrix(NA_integer_, nrow = n, ncol = 3,
                  dimnames = list(NULL, c("huso1", "huso2", "huso3")))
    pr_key <- ifelse(is.na(pr), NA_character_,
                     sprintf("%02d", as.integer(.basifoR_spatial_numeric(pr))))
    for (p in unique(pr_key[!is.na(pr_key)])) {
        i <- which(pr_key == p)
        hz <- sort(unique(as.integer(huso[i])))
        hz <- hz[!is.na(hz)]
        if (length(hz) >= 2L && all(diff(hz[seq_len(min(2L, length(hz)))]) == 1L)) {
            hz <- hz[seq_len(min(3L, length(hz)))]
            mat[i, seq_along(hz)] <- matrix(hz, nrow = length(i),
                                            ncol = length(hz), byrow = TRUE)
        }
    }
    mat
}

.basifoR_spatial_repair_table_huso <- function(huso, huso_source, pr, x_m,
                                               huso.candidates = NULL) {
    ## Some SNFI coordinate tables carry a Huso field, but plots close to a
    ## zone boundary can still be encoded with the neighbouring zone.  The
    ## legacy IFN correction uses the candidate zones and CoorX threshold to
    ## recover the source zone before transforming to a common CRS.
    n <- length(huso)
    if (!n)
        return(list(huso = huso, huso_source = huso_source,
                    repaired = rep(FALSE, n)))

    mat_user <- NULL
    if (!is.null(huso.candidates))
        mat_user <- .basifoR_spatial_candidates_to_matrix(huso.candidates, n = n)

    cand_list <- .basifoR_spatial_ifn2_default_candidates(pr)
    mat_default <- .basifoR_spatial_candidates_to_matrix(cand_list, n = n)
    mat_table <- .basifoR_spatial_candidate_matrix_from_table(pr = pr, huso = huso)

    mat <- matrix(NA_integer_, nrow = n, ncol = 3,
                  dimnames = list(NULL, c("huso1", "huso2", "huso3")))

    fill <- function(base, add) {
        if (is.null(add))
            return(base)
        ok <- apply(!is.na(add), 1L, any)
        base[ok, ] <- add[ok, , drop = FALSE]
        base
    }

    ## Explicit user candidates have priority, then built-in province rules,
    ## then a conservative table-derived candidate set when a province actually
    ## contains adjacent Huso values in the coordinate table.
    mat <- fill(mat, mat_table)
    mat <- fill(mat, mat_default)
    mat <- fill(mat, mat_user)

    if (!any(apply(!is.na(mat), 1L, any)))
        return(list(huso = huso, huso_source = huso_source,
                    repaired = rep(FALSE, n)))

    rule <- .basifoR_spatial_infer_huso_from_candidate_matrix(
        mat,
        x_m = x_m,
        source = "CoorX_rule_repaired"
    )

    repaired <- !is.na(rule$huso) & !is.na(huso) & rule$huso != as.integer(huso)
    if (any(repaired)) {
        huso[repaired] <- rule$huso[repaired]
        old <- huso_source[repaired]
        old[is.na(old) | !nzchar(old)] <- "huso"
        huso_source[repaired] <- paste0(old, "_CoorX_repaired")
    }

    list(huso = as.integer(huso), huso_source = huso_source, repaired = repaired)
}

.basifoR_spatial_ifn2_datum <- function(pr) {
    pr <- sprintf("%02d", as.integer(.basifoR_spatial_numeric(pr)))
    ifelse(pr %in% c("35", "38"), "WGS84", "ED50")
}

.basifoR_spatial_ifn4_datum <- function(pr) {
    pr <- sprintf("%02d", as.integer(.basifoR_spatial_numeric(pr)))

    canarias <- c("35", "38")
    ed50 <- c(
        "31",                         # Navarra
        "15", "27", "32", "36",     # Galicia
        "33",                         # Asturias
        "39",                         # Cantabria
        "30",                         # Murcia
        "07",                         # Islas Baleares
        "01", "20", "48",             # País Vasco
        "26",                         # La Rioja
        "28",                         # Madrid
        "08", "17", "25", "43"       # Cataluña
    )

    out <- rep(NA_character_, length(pr))
    out[pr %in% canarias] <- "WGS84"
    out[pr %in% ed50] <- "ED50"
    out[is.na(out) & !is.na(pr)] <- "ETRS89"
    out
}

.basifoR_spatial_ifn_datum <- function(nfi.nr, pr) {
    nfi.nr <- as.integer(nfi.nr)[1L]
    if (identical(nfi.nr, 2L))
        return(.basifoR_spatial_ifn2_datum(pr))
    if (identical(nfi.nr, 3L))
        return(rep("ED50", length(pr)))
    if (identical(nfi.nr, 4L))
        return(.basifoR_spatial_ifn4_datum(pr))
    rep(NA_character_, length(pr))
}

.basifoR_spatial_epsg <- function(datum, huso) {
    hz <- suppressWarnings(as.integer(huso))
    datum <- toupper(trimws(as.character(datum)))
    out <- rep(NA_integer_, length(datum))

    ok <- !is.na(hz) & datum == "ED50"
    out[ok] <- 23000L + hz[ok]

    ok <- !is.na(hz) & datum == "ETRS89"
    out[ok] <- 25800L + hz[ok]

    ok <- !is.na(hz) & datum == "WGS84"
    out[ok] <- 32600L + hz[ok]

    out
}

.basifoR_spatial_default_target_crs <- function(datum, epsg) {
    datum <- unique(toupper(trimws(as.character(datum[!is.na(datum)]))))
    epsg <- unique(epsg[!is.na(epsg)])

    if (length(epsg) == 1L)
        return(epsg)

    if ("ED50" %in% datum)
        return(23030L)
    if ("ETRS89" %in% datum)
        return(25830L)
    if ("WGS84" %in% datum)
        return(32630L)

    NA_integer_
}

.basifoR_spatial_guess_coord_table <- function(tables, nfi.nr) {
    if (exists(".nfi_guess_coord_table", mode = "function", inherits = TRUE)) {
        z <- tryCatch(.nfi_guess_coord_table(tables, nfi.nr = nfi.nr),
                      error = function(e) NULL)
        if (!is.null(z))
            return(z)
    }

    if (is.null(tables) || !is.data.frame(tables) || !"dt.nm" %in% names(tables))
        return(NULL)

    dt <- as.character(tables$dt.nm)
    dt_low <- tolower(dt)

    if (identical(as.integer(nfi.nr), 2L)) {
        cand <- grep("^datest[0-9]*$", dt_low)
        if (length(cand))
            return("DATEST")
        cand <- grep("^dates[0-9]*$", dt_low)
        if (length(cand))
            return(dt[cand[1L]])
    }

    cand <- c(which(dt_low == "pcdatosmap"),
              which(dt_low == "listado definitivo"),
              grep("listado", dt_low))
    cand <- unique(cand)
    if (length(cand))
        return(dt[cand[1L]])

    NULL
}

.basifoR_spatial_coord_cols_snfi <- function(coords, nfi.nr) {
    x_col <- .basifoR_spatial_first_col(
        coords,
        if (identical(as.integer(nfi.nr), 2L)) {
            c("COORDEX", "CX", "CoorX", "CoorXC", "CoordX", "X", "x")
        } else {
            c("CoorX", "CoorXC", "CoordX", "COORDEX", "CX", "X", "x")
        }
    )

    y_col <- .basifoR_spatial_first_col(
        coords,
        if (identical(as.integer(nfi.nr), 2L)) {
            c("COORDEY", "CY", "CoorY", "CoorYC", "CoordY", "Y", "y")
        } else {
            c("CoorY", "CoorYC", "CoordY", "COORDEY", "CY", "Y", "y")
        }
    )

    if (is.na(x_col) || is.na(y_col))
        stop("Could not detect SNFI coordinate columns.", call. = FALSE)

    h_col <- .basifoR_spatial_first_col(coords, c("Huso", "HUSO", "huso", "Zone", "zone"))

    list(x = x_col, y = y_col, huso = h_col)
}

.basifoR_spatial_read_coord_table <- function(nfi, nfi.nr, dots,
                                             file_ext = NULL,
                                             coord.nm = NULL) {
    if (!exists("listNFI_tables", mode = "function", inherits = TRUE))
        stop("listNFI_tables() was not found.", call. = FALSE)
    if (!exists(".nfi_read_local_table", mode = "function", inherits = TRUE))
        stop(".nfi_read_local_table() was not found. Source readNFIsf_* first.", call. = FALSE)

    tab_args <- c(list(nfi = nfi, nfi.nr = nfi.nr), dots)
    if (!is.null(file_ext))
        tab_args$file_ext <- file_ext

    tabs <- do.call(listNFI_tables, tab_args)
    if (is.null(tabs) || !is.data.frame(tabs) || !nrow(tabs))
        stop("No SNFI table paths were returned by listNFI_tables().", call. = FALSE)
    if (!"path" %in% names(tabs))
        stop("listNFI_tables() did not return a 'path' column.", call. = FALSE)

    if (is.null(coord.nm))
        coord.nm <- .basifoR_spatial_guess_coord_table(tabs, nfi.nr = nfi.nr)
    if (is.null(coord.nm))
        stop("No coordinate table was detected for SNFI geometry.", call. = FALSE)

    local_files <- unique(as.character(tabs$path))
    local_files <- local_files[!is.na(local_files) & nzchar(local_files)]
    province <- .basifoR_spatial_input_province(nfi)

    coords <- .nfi_read_local_table(
        files = local_files,
        nfi.nr = nfi.nr,
        dt.nm = coord.nm,
        province = province
    )

    if (is.null(coords) || !is.data.frame(coords))
        stop("The SNFI coordinate table could not be read.", call. = FALSE)

    list(coords = coords, coord.nm = coord.nm, tabs = tabs, local_files = local_files)
}

.basifoR_spatial_project_snfi_coords <- function(coords, nfi.nr,
                                                coord.factor = NULL,
                                                huso.method = c("auto", "candidate", "xgap", "province", "none"),
                                                target.crs = NULL,
                                                huso.candidates = NULL,
                                                repair.table.huso = TRUE,
                                                validate = TRUE) {
    huso.method <- match.arg(huso.method)

    if (!requireNamespace("sf", quietly = TRUE))
        stop("Package 'sf' is required to build SNFI geometry.", call. = FALSE)

    cc <- .basifoR_spatial_coord_cols_snfi(coords, nfi.nr = nfi.nr)
    keys <- .basifoR_spatial_plot_keys(coords, allow_missing_province = TRUE)
    use_pr <- any(!is.na(keys$.nfi_pr))
    key <- .basifoR_spatial_make_key(keys, use_pr = use_pr)

    x_raw <- .basifoR_spatial_numeric(coords[[cc$x]])
    y_raw <- .basifoR_spatial_numeric(coords[[cc$y]])

    if (is.null(coord.factor)) {
        medx <- stats::median(abs(x_raw), na.rm = TRUE)
        coord.factor <- if (identical(as.integer(nfi.nr), 2L) &&
                            is.finite(medx) && medx < 10000) 1000 else 1
    }

    x_m <- x_raw * coord.factor
    y_m <- y_raw * coord.factor
    invalid_coord <- .basifoR_spatial_invalid_snfi_coord(
        x_m = x_m,
        y_m = y_m,
        x_raw = x_raw,
        y_raw = y_raw
    )

    huso <- rep(NA_integer_, length(x_m))
    huso_source <- rep(NA_character_, length(x_m))

    if (!is.na(cc$huso)) {
        huso <- .basifoR_spatial_clean_huso(coords[[cc$huso]])
        huso_source[!is.na(huso)] <- "table"
    }

    repaired_huso <- rep(FALSE, length(huso))
    if (isTRUE(repair.table.huso) && any(!is.na(huso)) &&
        huso.method %in% c("auto", "candidate", "xgap")) {
        rep_h <- .basifoR_spatial_repair_table_huso(
            huso = huso,
            huso_source = huso_source,
            pr = keys$.nfi_pr,
            x_m = x_m,
            huso.candidates = huso.candidates
        )
        huso <- rep_h$huso
        huso_source <- rep_h$huso_source
        repaired_huso <- rep_h$repaired
    }

    missing_huso <- is.na(huso)
    if (any(missing_huso) && huso.method %in% c("auto", "candidate") &&
        .basifoR_spatial_has_candidate_huso(coords)) {
        cand <- .basifoR_spatial_infer_huso_from_candidates(coords, x_m = x_m)
        if (!is.null(cand)) {
            huso[missing_huso] <- cand$huso[missing_huso]
            huso_source[missing_huso & !is.na(huso)] <- cand$huso_source[missing_huso & !is.na(huso)]
        }
    }

    missing_huso <- is.na(huso)
    if (any(missing_huso) && huso.method %in% c("auto", "xgap")) {
        cand <- .basifoR_spatial_infer_huso_from_pr_candidates(
            pr = keys$.nfi_pr,
            x_m = x_m,
            huso.candidates = huso.candidates
        )
        if (!is.null(cand)) {
            huso[missing_huso] <- cand$huso[missing_huso]
            huso_source[missing_huso & !is.na(huso)] <- cand$huso_source[missing_huso & !is.na(huso)]
        }
    }

    missing_huso <- is.na(huso)
    if (any(missing_huso) && huso.method %in% c("auto", "province")) {
        hp <- .basifoR_spatial_huso_from_province(keys$.nfi_pr)
        huso[missing_huso] <- hp[missing_huso]
        huso_source[missing_huso & !is.na(huso)] <- "province_inferred"
    }

    datum <- .basifoR_spatial_ifn_datum(nfi.nr, keys$.nfi_pr)
    epsg <- .basifoR_spatial_epsg(datum, huso)

    if (is.null(target.crs))
        target.crs <- .basifoR_spatial_default_target_crs(datum = datum, epsg = epsg)

    keep <- !duplicated(key) & !is.na(keys$.nfi_plot) & !is.na(x_m) & !is.na(y_m) & !invalid_coord
    coord_warnings <- list(
        invalid_coordinate_rows = which(invalid_coord),
        repaired_huso_rows = which(repaired_huso),
        invalid_coordinate_count = sum(invalid_coord, na.rm = TRUE),
        repaired_huso_count = sum(repaired_huso, na.rm = TRUE)
    )
    if (isTRUE(validate) && any(invalid_coord, na.rm = TRUE)) {
        warning(sum(invalid_coord, na.rm = TRUE),
                " plot coordinate row(s) contained invalid or placeholder coordinates ",
                "and were omitted from the spatial registry.", call. = FALSE)
    }
    if (isTRUE(validate) && any(repaired_huso, na.rm = TRUE)) {
        warning(sum(repaired_huso, na.rm = TRUE),
                " plot coordinate row(s) had Huso reassigned using the CoorX threshold rule ",
                "before CRS transformation.", call. = FALSE)
    }
    if (isTRUE(validate) && any(duplicated(key[!is.na(key)]))) {
        ndup <- length(unique(key[duplicated(key) & !is.na(key)]))
        warning(ndup, " duplicated plot key(s) in the SNFI coordinate table. ",
                "Only the first geometry per plot is stored.", call. = FALSE)
    }

    reg_tab <- data.frame(
        .nfi_pr = keys$.nfi_pr[keep],
        .nfi_plot = keys$.nfi_plot[keep],
        stringsAsFactors = FALSE
    )
    cols <- .basifoR_spatial_plot_cols(coords, allow_missing_province = TRUE)
    if (!is.na(cols$pr_col))
        reg_tab[[cols$pr_col]] <- coords[[cols$pr_col]][keep]
    reg_tab[[cols$plot_col]] <- coords[[cols$plot_col]][keep]
    reg_tab$.nfi_x_m <- x_m[keep]
    reg_tab$.nfi_y_m <- y_m[keep]
    reg_tab$huso <- huso[keep]
    reg_tab$huso_source <- huso_source[keep]
    reg_tab$huso_repaired <- repaired_huso[keep]
    reg_tab$coord_invalid <- invalid_coord[keep]
    reg_tab$datum <- datum[keep]
    reg_tab$source_epsg <- epsg[keep]
    reg_tab$target_epsg <- target.crs

    valid_epsg <- !is.na(reg_tab$source_epsg)
    if (!any(valid_epsg)) {
        if (isTRUE(validate))
            warning("No valid EPSG could be derived for SNFI coordinates; geometry CRS is NA.",
                    call. = FALSE)
        reg <- sf::st_as_sf(reg_tab,
                            coords = c(".nfi_x_m", ".nfi_y_m"),
                            crs = NA,
                            remove = FALSE,
                            na.fail = FALSE)
        return(list(geometry = reg, coord_factor = coord.factor,
                    coord_source_columns = cc, target_crs = NA_integer_,
                    coordinate_warnings = coord_warnings))
    }

    pieces <- lapply(split(reg_tab[valid_epsg, , drop = FALSE], reg_tab$source_epsg[valid_epsg]), function(z) {
        epsg0 <- unique(z$source_epsg)
        pts <- sf::st_as_sf(
            z,
            coords = c(".nfi_x_m", ".nfi_y_m"),
            crs = epsg0[1L],
            remove = FALSE,
            na.fail = FALSE
        )
        if (!is.na(target.crs))
            pts <- sf::st_transform(pts, target.crs)
        pts
    })

    reg <- do.call(rbind, pieces)

    if (any(!valid_epsg) && isTRUE(validate)) {
        warning(sum(!valid_epsg),
                " plot(s) had unresolved CRS metadata and were omitted from the spatial registry.",
                call. = FALSE)
    }

    list(geometry = reg, coord_factor = coord.factor,
         coord_source_columns = cc, target_crs = target.crs,
         coordinate_warnings = coord_warnings)
}

.basifoR_spatial_registry_from_snfi_tables <- function(nfi, nfi.nr, dots,
                                                      file_ext = NULL,
                                                      coord.nm = NULL,
                                                      coord.factor = NULL,
                                                      huso.method = c("auto", "candidate", "xgap", "province", "none"),
                                                      target.crs = NULL,
                                                      huso.candidates = NULL,
                                                      repair.table.huso = TRUE,
                                                      source_dt.nm = NULL,
                                                      validate = TRUE) {
    rt <- .basifoR_spatial_read_coord_table(
        nfi = nfi,
        nfi.nr = nfi.nr,
        dots = dots,
        file_ext = file_ext,
        coord.nm = coord.nm
    )

    prj <- .basifoR_spatial_project_snfi_coords(
        coords = rt$coords,
        nfi.nr = nfi.nr,
        coord.factor = coord.factor,
        huso.method = huso.method,
        target.crs = target.crs,
        huso.candidates = huso.candidates,
        repair.table.huso = repair.table.huso,
        validate = validate
    )

    reg <- prj$geometry
    if (!inherits(reg, "sf"))
        stop("SNFI coordinate projection did not produce an sf object.", call. = FALSE)

    list(
        version = 2L,
        geometry = reg,
        key_cols = .basifoR_spatial_plot_cols(rt$coords, allow_missing_province = TRUE),
        key_uses_province = any(!is.na(reg$.nfi_pr)),
        normalized_key_cols = c(".nfi_pr", ".nfi_plot"),
        geometry_level = "plot",
        geometry_role = "plot_registry",
        source = "snfi_coordinate_table",
        source_dt.nm = source_dt.nm,
        coord_table = rt$coord.nm,
        coord_reference = unique(sf::st_drop_geometry(reg)[, c("huso", "huso_source", "huso_repaired", "datum", "source_epsg", "target_epsg"), drop = FALSE]),
        coordinate_warnings = prj$coordinate_warnings,
        coord_type = "UTM",
        coord_units = "m",
        coord_source_columns = prj$coord_source_columns,
        coord_factor = prj$coord_factor,
        coord_source_units = if (identical(as.integer(nfi.nr), 2L) && prj$coord_factor == 1000) "km" else "m",
        coord_accuracy = .basifoR_spatial_snfi_coord_accuracy(nfi.nr),
        coord_accuracy_note = .basifoR_spatial_snfi_coord_accuracy_note(nfi.nr),
        coord_update_supported = TRUE,
        sf_crs = sf::st_crs(reg),
        sf_crs_assigned = prj$target_crs,
        nfi.nr = as.integer(nfi.nr)[1L],
        nfi_tables = rt$tabs,
        local_files = rt$local_files,
        created = Sys.time()
    )
}


## ---- Optional boundary helpers -------------------------------------------
## These helpers avoid a hard dependency on boundary packages. They download
## lightweight boundary files, cache them, read them with sf, and filter Spanish
## boundaries using the same province table used by basifoR when it is available.

.basifoR_spatial_norm_name <- function(x) {
    x <- as.character(x)
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x <- tolower(trimws(x))
    gsub("[^a-z0-9]+", "", x)
}

.basifoR_spatial_gadm_province_lut <- function() {
    data.frame(
        codigo = sprintf("%02d", 1:52),
        gadm_name_2 = c(
            "Alava", "Albacete", "Alicante", "Almeria", "Avila",
            "Badajoz", "Illes Balears", "Barcelona", "Burgos",
            "Caceres", "Cadiz", "Castellon", "Ciudad Real",
            "Cordoba", "A Coruna", "Cuenca", "Girona", "Granada",
            "Guadalajara", "Gipuzkoa", "Huelva", "Huesca", "Jaen",
            "Leon", "Lleida", "La Rioja", "Lugo", "Madrid",
            "Malaga", "Murcia", "Navarra", "Ourense", "Asturias",
            "Palencia", "Las Palmas", "Pontevedra", "Salamanca",
            "Santa Cruz de Tenerife", "Cantabria", "Segovia", "Sevilla",
            "Soria", "Tarragona", "Teruel", "Toledo", "Valencia",
            "Valladolid", "Bizkaia", "Zamora", "Zaragoza", "Ceuta",
            "Melilla"
        ),
        gadm_aliases = c(
            "Alava|Araba", "Albacete", "Alicante|Alacant", "Almeria",
            "Avila", "Badajoz", "Illes Balears|Islas Baleares|Balears",
            "Barcelona", "Burgos", "Caceres", "Cadiz", "Castellon|Castello",
            "Ciudad Real", "Cordoba", "A Coruna|Coruna|La Coruna", "Cuenca",
            "Girona|Gerona", "Granada", "Guadalajara", "Gipuzkoa|Guipuzcoa",
            "Huelva", "Huesca", "Jaen", "Leon", "Lleida|Lerida",
            "La Rioja|Rioja", "Lugo", "Madrid", "Malaga", "Murcia",
            "Navarra|Navarre", "Ourense|Orense", "Asturias", "Palencia",
            "Las Palmas", "Pontevedra", "Salamanca", "Santa Cruz de Tenerife",
            "Cantabria", "Segovia", "Sevilla|Seville", "Soria", "Tarragona",
            "Teruel", "Toledo", "Valencia|Valencia/Valencia", "Valladolid",
            "Bizkaia|Vizcaya", "Zamora", "Zaragoza", "Ceuta", "Melilla"
        ),
        stringsAsFactors = FALSE
    )
}

.basifoR_spatial_split_aliases <- function(x) {
    x <- as.character(x)
    x <- unlist(strsplit(x, "\\|", fixed = FALSE), use.names = FALSE)
    x <- trimws(x)
    x[!is.na(x) & nzchar(x)]
}

.basifoR_spatial_province_identity <- function(prov, province_table = NULL) {
    lut <- .basifoR_spatial_gadm_province_lut()
    target <- .basifoR_spatial_norm_name(prov)

    get_row <- function(code = NULL, idx = NULL, source = "boundary_lut") {
        if (!is.null(code)) {
            code <- sprintf("%02d", suppressWarnings(as.integer(code))[1L])
            idx <- match(code, lut$codigo)
        }
        if (!length(idx) || is.na(idx[1L]))
            return(NULL)
        row <- lut[idx[1L], , drop = FALSE]
        aliases <- unique(c(row$gadm_name_2,
                            .basifoR_spatial_split_aliases(row$gadm_aliases)))
        list(
            codigo = row$codigo[1L],
            gadm_name_2 = row$gadm_name_2[1L],
            gadm_aliases = aliases,
            resolution_source = source
        )
    }

    ## Direct numeric province code. This avoids any curated redirects stored
    ## in procods and is the safest path for boundary filtering.
    if (length(prov) == 1L && grepl("^[0-9]+$", trimws(as.character(prov)))) {
        ans <- get_row(code = prov, source = "numeric_code")
        if (!is.null(ans))
            return(ans)
    }

    all_aliases <- lapply(seq_len(nrow(lut)), function(i)
        .basifoR_spatial_norm_name(c(lut$gadm_name_2[i],
                                     .basifoR_spatial_split_aliases(lut$gadm_aliases[i]))))
    hit <- which(vapply(all_aliases, function(a) target %in% a, logical(1)))
    if (length(hit) == 1L)
        return(get_row(idx = hit, source = "boundary_lut_alias"))

    ## Last resort: use basifoR's province resolver to obtain a province code.
    ## Do not use all procods aliases for GADM matching: procods may contain
    ## operational redirects/curation decisions used for MITECO downloads.
    if (is.null(province_table) && exists("procods", inherits = TRUE))
        province_table <- get("procods", inherits = TRUE)

    if (exists("find_code_", mode = "function", inherits = TRUE) &&
        is.data.frame(province_table)) {
        code <- tryCatch(
            find_code_(prov, is.ifn4 = FALSE, df = province_table,
                       complain = FALSE),
            error = function(e) NULL
        )
        if (!is.null(code) && length(code) && !is.na(code[1L])) {
            ans <- get_row(code = code[1L], source = "procods_code_resolver")
            if (!is.null(ans))
                return(ans)
        }
    }

    stop(
        "Province/code '", prov,
        "' could not be resolved to a Spanish province boundary.",
        call. = FALSE
    )
}

.basifoR_spatial_gadm_url <- function(country = "ESP", level = 2,
                                      ext = c("json", "gpkg"),
                                      version = "4.1") {
    ext <- match.arg(ext)
    country <- toupper(country)
    version_tag <- gsub("\\.", "", version)

    if (identical(ext, "json")) {
        return(sprintf(
            "https://geodata.ucdavis.edu/gadm/gadm%s/json/gadm%s_%s_%s.json.zip",
            version, version_tag, country, as.integer(level)
        ))
    }

    sprintf(
        "https://geodata.ucdavis.edu/gadm/gadm%s/gpkg/gadm%s_%s.gpkg",
        version, version_tag, country
    )
}


.basifoR_spatial_download_if_missing <- function(url, path,
                                                timeout = getOption(
                                                    "basifoR.boundary.timeout",
                                                    getOption(
                                                        "basifoR.gadm.timeout",
                                                        300
                                                    )
                                                )) {
    if (!dir.exists(path))
        dir.create(path, recursive = TRUE, showWarnings = FALSE)

    dest <- file.path(path, basename(url))
    if (file.exists(dest)) {
        dest_size <- file.info(dest)$size
        if (!is.na(dest_size) && dest_size > 0)
            return(dest)
        unlink(dest)
    }

    if (!file.exists(dest)) {
        timeout <- suppressWarnings(as.numeric(timeout)[1L])
        if (is.na(timeout) || timeout <= 0)
            timeout <- 300

        current_timeout <- suppressWarnings(as.numeric(getOption("timeout"))[1L])
        if (is.na(current_timeout) || current_timeout <= 0)
            current_timeout <- 60

        old_options <- options(timeout = max(timeout, current_timeout))
        on.exit(options(old_options), add = TRUE)

        tmp <- tempfile(pattern = paste0(basename(url), "."),
                        tmpdir = path)
        on.exit(unlink(tmp), add = TRUE)

        utils::download.file(url, destfile = tmp, mode = "wb", quiet = FALSE)

        if (!file.exists(tmp) || is.na(file.info(tmp)$size) ||
            file.info(tmp)$size <= 0) {
            stop("Downloaded boundary file is empty: ", url, call. = FALSE)
        }

        if (!file.rename(tmp, dest)) {
            ok <- file.copy(tmp, dest, overwrite = TRUE)
            if (!isTRUE(ok))
                stop("Could not move downloaded boundary file to cache: ", dest,
                     call. = FALSE)
        }
    }
    dest
}

.basifoR_spatial_gisco_url <- function(year = "2021", resolution = "10M",
                                       level = 3) {
    sprintf(
        paste0(
            "https://gisco-services.ec.europa.eu/distribution/v2/nuts/",
            "geojson/NUTS_RG_%s_%s_4326_LEVL_%s.geojson"
        ),
        resolution, year, as.integer(level)
    )
}

.basifoR_spatial_gisco_nuts_lut <- function() {
    data.frame(
        codigo = sprintf("%02d", 1:52),
        nuts_id = c(
            "ES211", "ES421", "ES521", "ES611", "ES411",
            "ES431", "ES531", "ES511", "ES412", "ES432",
            "ES612", "ES522", "ES422", "ES613", "ES111",
            "ES423", "ES512", "ES614", "ES424", "ES212",
            "ES615", "ES241", "ES616", "ES413", "ES513",
            "ES230", "ES112", "ES300", "ES617", "ES620",
            "ES220", "ES113", "ES120", "ES414", "ES704|ES705|ES708",
            "ES114", "ES415", "ES703|ES706|ES707|ES709", "ES130",
            "ES416", "ES618", "ES417", "ES514", "ES242",
            "ES425", "ES523", "ES418", "ES213", "ES419",
            "ES243", "ES630", "ES640"
        ),
        stringsAsFactors = FALSE
    )
}

.basifoR_spatial_gisco_nuts_ids <- function(prov) {
    id <- .basifoR_spatial_province_identity(prov)
    lut <- .basifoR_spatial_gisco_nuts_lut()
    hit <- match(id$codigo, lut$codigo)
    if (is.na(hit)) {
        stop(
            "Province code '", id$codigo,
            "' could not be mapped to a GISCO/NUTS-3 boundary.",
            call. = FALSE
        )
    }

    list(
        province = id,
        nuts_id = .basifoR_spatial_split_aliases(lut$nuts_id[hit])
    )
}

gisco_nuts_spatial <- structure(function
### Download and read Eurostat/GISCO NUTS boundaries.
(
    year = "2021", ##<< NUTS reference year used by GISCO.
    resolution = "10M", ##<< GISCO scale code. \code{"10M"} is light and
                        ## suitable for examples; use \code{"03M"} when a
                        ## more detailed final map is required.
    level = 3, ##<< NUTS level. Spanish provinces correspond to NUTS-3, with
               ## Canary Island provinces represented by several island units.
    path = tools::R_user_dir("basifoR", "cache"), ##<< Cache directory.
    quiet = TRUE ##<< Passed to \code{sf::st_read()}.
) {
    if (!requireNamespace("sf", quietly = TRUE))
        stop("Package 'sf' is required to read GISCO boundaries.", call. = FALSE)

    url <- .basifoR_spatial_gisco_url(
        year = year,
        resolution = resolution,
        level = level
    )
    f <- .basifoR_spatial_download_if_missing(url, path = path)
    adm <- sf::st_read(f, quiet = quiet)

    attr(adm, "gisco_year") <- year
    attr(adm, "gisco_resolution") <- resolution
    attr(adm, "gisco_level") <- as.integer(level)
    attr(adm, "gisco_source_url") <- url
    adm
    ##seealso<< readNFI_spatial, getNFIboundary_spatial
	}, ex = function() {
	    if (interactive()) {
	        adm <- gisco_nuts_spatial(path = tempdir())
	        plot(sf::st_geometry(adm))
	    }
	})

.basifoR_spatial_first_existing_col <- function(x, candidates) {
    nm <- names(x)
    hit <- match(tolower(candidates), tolower(nm))
    hit <- hit[!is.na(hit)]
    if (!length(hit))
        return(NA_character_)
    nm[hit[1L]]
}

filter_gisco_province_spatial <- structure(function
### Filter a GISCO NUTS-3 layer to one Spanish province.
(
    adm, ##<< An \code{sf} object returned by \code{gisco_nuts_spatial()}.
    prov ##<< Spanish province name or numeric code.
) {
    if (!requireNamespace("sf", quietly = TRUE))
        stop("Package 'sf' is required to filter GISCO boundaries.", call. = FALSE)

    id <- .basifoR_spatial_gisco_nuts_ids(prov)
    nuts_col <- .basifoR_spatial_first_existing_col(adm, "NUTS_ID")
    if (is.na(nuts_col))
        stop("The GISCO layer does not contain a NUTS_ID column.", call. = FALSE)

    keep <- as.character(adm[[nuts_col]]) %in% id$nuts_id
    out_raw <- adm[keep, , drop = FALSE]
    if (!nrow(out_raw)) {
        stop(
            "No GISCO NUTS-3 boundary matched province code ",
            id$province$codigo, " (", paste(id$nuts_id, collapse = ", "), ").",
            call. = FALSE
        )
    }

    name_col <- .basifoR_spatial_first_existing_col(
        out_raw,
        c("NAME_LATN", "NUTS_NAME", "NAME")
    )
    matched_names <- if (is.na(name_col)) id$province$gadm_name_2
                     else unique(as.character(out_raw[[name_col]]))

    ## GISCO boundaries are cartographic context. Dissolve multiple NUTS-3
    ## units in a projected European CRS so the operation is planar by design
    ## and does not depend on s2 for longitude/latitude geometry.
    out_union <- tryCatch(
        sf::st_transform(out_raw, 3035),
        error = function(e) out_raw
    )
    geom <- sf::st_union(sf::st_geometry(out_union))
    out <- sf::st_sf(
        data.frame(
            province_code = id$province$codigo,
            province_name = id$province$gadm_name_2,
            nuts_id = paste(id$nuts_id, collapse = "|"),
            nuts_name = paste(matched_names, collapse = "|"),
            stringsAsFactors = FALSE
        ),
        geometry = sf::st_sfc(geom, crs = sf::st_crs(out_union))
    )

    attr(out, "matched_province_code") <- id$province$codigo
    attr(out, "matched_gisco_nuts_id") <- id$nuts_id
    attr(out, "matched_gisco_name") <- matched_names
    out
    ##seealso<< gisco_nuts_spatial, readNFI_spatial
	}, ex = function() {
	    if (interactive()) {
	        adm <- gisco_nuts_spatial(path = tempdir())
	        toledo <- filter_gisco_province_spatial(adm, 45)
	        plot(sf::st_geometry(toledo))
	    }
	})

.basifoR_spatial_unzip_vector <- function(file, path, ext = "json") {
    if (!grepl("\\.zip$", file, ignore.case = TRUE))
        return(file)

    out_dir <- file.path(path, tools::file_path_sans_ext(basename(file)))
    if (!dir.exists(out_dir))
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    find_file <- function()
        list.files(out_dir, pattern = paste0("\\.", ext, "$"),
                   full.names = TRUE, recursive = TRUE,
                   ignore.case = TRUE)

    found <- find_file()
    if (!length(found)) {
        utils::unzip(file, exdir = out_dir)
        found <- find_file()
    }

    if (!length(found))
        stop("No .", ext, " file was found after decompressing GADM data.",
             call. = FALSE)

    found[1L]
}

gadm_spatial <- structure(function
### Download and read GADM boundaries without depending on geodata.
(
    country = "ESP", ##<< ISO3 country code. The default downloads Spain.
    level = 2, ##<< Administrative level to download. For Spain, level 2 is
               ## commonly useful as a province boundary layer; higher levels
               ## can be used for municipal or local cartographic context.
    path = tools::R_user_dir("basifoR", "cache"), ##<< Cache directory used
               ## to store downloaded GADM files.
    ext = c("json", "gpkg"), ##<< GADM file format. The default 
               ## \code{"json"} downloads the level-specific zipped GeoJSON.
    version = "4.1", ##<< GADM version string used to build the download URL.
    quiet = TRUE ##<< Passed to \code{sf::st_read()}.
) {
    ##details<< This helper downloads GADM directly from the public GADM
    ## distribution endpoint, caches the file in \code{path}, and
    ## reads it with \pkg{sf}. It is intended as an optional
    ## cartographic support layer for plotting and spatial quality
    ## checks. GADM boundaries are not Spanish NFI products and should
    ## not be interpreted as official inventory geometries.
    ##value<< An \code{sf} object containing the requested GADM administrative
    ## boundary layer. Attributes record country, level, version, and URL.

    if (!requireNamespace("sf", quietly = TRUE))
        stop("Package 'sf' is required to read GADM boundaries.", call. = FALSE)

    ext <- match.arg(ext)
    url <- .basifoR_spatial_gadm_url(country = country, level = level,
                                     ext = ext, version = version)
    f <- .basifoR_spatial_download_if_missing(url, path = path)

    if (identical(ext, "json")) {
        dec <- .basifoR_spatial_unzip_vector(f, path = path, ext = "json")
        adm <- sf::st_read(dec, quiet = quiet)
    } else {
        adm <- sf::st_read(f, quiet = quiet)
    }

    attr(adm, "gadm_country") <- toupper(country)
    attr(adm, "gadm_level") <- as.integer(level)
    attr(adm, "gadm_version") <- version
    attr(adm, "gadm_source_url") <- url
    adm
    ##seealso<< readNFI_spatial, getNFIboundary_spatial
	}, ex = function() {
	    if (interactive()) {
	        adm <- gadm_spatial("ESP", level = 2, path = tempdir())
	        plot(sf::st_geometry(adm))
	    }
	})

filter_gadm_province_spatial <- structure(function
### Filter a Spanish GADM layer using a strict province identity resolver.
(
    adm, ##<< An \code{sf} object returned by \code{gadm_spatial()} or a
         ## user-provided GADM-like boundary layer.
    prov, ##<< Province code or name. Numeric codes are matched directly.
          ## Non-numeric names are resolved against a clean GADM-oriented
          ## province-name lookup and, only as a last resort, against
          ## basifoR's \code{procods} code resolver.
    province_table = NULL, ##<< Optional province lookup table used only by
                           ## the fallback basifoR code resolver. It is not
                           ## used as a list of GADM aliases.
    level = attr(adm, "gadm_level", exact = TRUE), ##<< GADM level of
                           ## \code{adm}. Level 2 returns one province. Higher
                           ## levels return features whose \code{NAME_2}
                           ## belongs to the province, for example
                           ## municipalities inside a province.
    strict = TRUE ##<< \code{logical}. If \code{TRUE}, level-2 matches must
                  ## return one province. This prevents false matches from
                  ## short aliases such as \code{"ZA"}, which could match
                  ## both Zamora and Zaragoza.
) {
    ##details<< This function deliberately does not use all aliases stored in
    ## \code{procods} for GADM filtering. The \code{procods} table can
    ## contain operational curation and province redirections used by
    ## basifoR/MITECO workflows. Boundary filtering needs geographic
    ## province identity. Therefore \code{procods} is used, at most,
    ## to resolve the input to a numeric province code; the final GADM
    ## match uses exact normalized matching against \code{NAME_2} and
    ## a clean province-name lookup. This avoids false matches such as
    ## Zamora also selecting Zaragoza through a short alias.
    ##value<< A filtered \code{sf} object. For GADM level 2, the object should
    ## contain one province feature. For higher levels, the object may
    ## contain several subprovince features. Attributes store the
    ## resolved province code, GADM name, and aliases tested.

    if (!inherits(adm, "sf"))
        stop("'adm' must inherit from 'sf'.", call. = FALSE)

    if (is.null(level) || length(level) == 0L || is.na(level))
        level <- 2L
    level <- suppressWarnings(as.integer(level)[1L])

    if (!"NAME_2" %in% names(adm)) {
        stop(
            "Column 'NAME_2' was not found in the GADM object. ",
            "Use GADM level 2 or higher for province filtering.",
            call. = FALSE
        )
    }

    id <- .basifoR_spatial_province_identity(
        prov = prov,
        province_table = province_table
    )

    aliases_norm <- unique(.basifoR_spatial_norm_name(id$gadm_aliases))
    vals_norm <- .basifoR_spatial_norm_name(adm$NAME_2)

    ## Exact normalized matching only. Do not use grepl() with short aliases.
    hit <- vals_norm %in% aliases_norm

    if (!any(hit)) {
        stop(
            "No GADM province matched '", prov,
            "'. Resolved code: ", id$codigo,
            "; GADM name tested: ", id$gadm_name_2,
            "; aliases tested: ", paste(id$gadm_aliases, collapse = ", "),
            call. = FALSE
        )
    }

    out <- adm[hit, ]

    if (isTRUE(strict) && level <= 2L && nrow(out) != 1L) {
        cols <- intersect(c("GID_2", "NAME_1", "NAME_2", "VARNAME_2"), names(out))
        msg <- paste(utils::capture.output(
            print(sf::st_drop_geometry(out)[, cols, drop = FALSE])
        ), collapse = "\n")
        stop(
            "GADM province filtering returned ", nrow(out),
            " features for '", prov, "', but exactly one was expected.\n",
            msg,
            call. = FALSE
        )
    }

    attr(out, "matched_province") <- prov
    attr(out, "matched_province_code") <- id$codigo
    attr(out, "matched_gadm_name_2") <- id$gadm_name_2
    attr(out, "matched_aliases") <- id$gadm_aliases
    attr(out, "province_resolution_source") <- id$resolution_source
    out
    ##seealso<< gadm_spatial, readNFI_spatial
	}, ex = function() {
	    if (interactive()) {
	        adm <- gadm_spatial("ESP", level = 2, path = tempdir())
	        zamora <- filter_gadm_province_spatial(adm, 49)
	        nrow(zamora)
	    }
	})

.basifoR_spatial_boundary_from_object <- function(boundary.object,
                                                  boundary.crs = NULL,
                                                  target.crs = NULL,
                                                  role = "user_boundary") {
    if (is.null(boundary.object))
        return(NULL)
    if (!requireNamespace("sf", quietly = TRUE))
        stop("Package 'sf' is required for boundary objects.", call. = FALSE)
    if (!inherits(boundary.object, "sf"))
        stop("'boundary.object' must inherit from 'sf'.", call. = FALSE)

    b <- boundary.object
    if (is.na(sf::st_crs(b))) {
        if (is.null(boundary.crs)) {
            stop(
                "'boundary.object' has no CRS; supply 'boundary.crs'.",
                call. = FALSE
            )
        }
        sf::st_crs(b) <- boundary.crs
    }

    geometry_types <- unique(as.character(sf::st_geometry_type(b)))
    polygon_types <- c("POLYGON", "MULTIPOLYGON")
    if (!length(geometry_types) || any(!geometry_types %in% polygon_types)) {
        stop(
            "'boundary.object' must contain only POLYGON or MULTIPOLYGON geometries.",
            call. = FALSE
        )
    }

    if (!is.null(target.crs) && !is.na(sf::st_crs(b)))
        b <- sf::st_transform(b, target.crs)

    list(
        version = 1L,
        geometry = b,
        source = "user",
        role = role,
        boundary_level = NA_integer_,
        crs = sf::st_crs(b),
        created = Sys.time()
    )
}

.basifoR_spatial_boundary_from_gadm <- function(prov,
                                                level = 2,
                                                path = tools::R_user_dir("basifoR", "cache"),
                                                ext = "json",
                                                version = "4.1",
                                                target.crs = NULL,
                                                validate = TRUE) {
    ## `prov` may contain one or several actual province codes discovered in
    ## the imported data.  This is important because basifoR/procods can redirect
    ## some user inputs to another operational province or to a multi-province
    ## source.  Boundary selection must follow the data that were actually read,
    ## not necessarily the original input string.
    if (is.null(prov) || !length(prov))
        return(NULL)

    prov <- unique(prov[!is.na(prov) & nzchar(trimws(as.character(prov)))])
    if (!length(prov))
        return(NULL)

    if (!requireNamespace("sf", quietly = TRUE))
        stop("Package 'sf' is required for GADM boundaries.", call. = FALSE)

    adm <- gadm_spatial(country = "ESP", level = level, path = path,
                        ext = ext, version = version, quiet = TRUE)

    pieces <- lapply(prov, function(pr) {
        filter_gadm_province_spatial(adm, prov = pr, level = level)
    })

    b <- do.call(rbind, pieces)
    if (!is.null(target.crs) && !is.na(sf::st_crs(b)))
        b <- sf::st_transform(b, target.crs)

    matched_codes <- vapply(pieces, function(z)
        attr(z, "matched_province_code", exact = TRUE), character(1))
    matched_names <- vapply(pieces, function(z)
        attr(z, "matched_gadm_name_2", exact = TRUE), character(1))
    matched_aliases <- unique(unlist(lapply(pieces, function(z)
        attr(z, "matched_aliases", exact = TRUE)), use.names = FALSE))

    list(
        version = 2L,
        geometry = b,
        source = "GADM",
        role = if (as.integer(level) == 2L) "province_border" else "administrative_boundary",
        boundary_level = as.integer(level),
        matched_province = prov,
        matched_province_code = matched_codes,
        matched_gadm_name_2 = matched_names,
        matched_aliases = matched_aliases,
        gadm_country = attr(adm, "gadm_country", exact = TRUE),
        gadm_version = attr(adm, "gadm_version", exact = TRUE),
        gadm_source_url = attr(adm, "gadm_source_url", exact = TRUE),
        crs = sf::st_crs(b),
        note = paste(
            "GADM is stored as an optional cartographic reference for plotting",
            "and spatial checks. It is not an official Spanish NFI geometry.",
            "When the NFI import redirects or groups province inputs, the",
            "boundary follows the province codes actually present in the",
            "imported data or geometry registry."
        ),
        created = Sys.time()
    )
}

.basifoR_spatial_boundary_from_gisco <- function(prov,
                                                 path = tools::R_user_dir("basifoR", "cache"),
                                                 year = "2021",
                                                 resolution = getOption(
                                                     "basifoR.gisco.resolution",
                                                     "10M"
                                                 ),
                                                 target.crs = NULL,
                                                 validate = TRUE) {
    if (is.null(prov) || !length(prov))
        return(NULL)

    prov <- unique(prov[!is.na(prov) & nzchar(trimws(as.character(prov)))])
    if (!length(prov))
        return(NULL)

    if (!requireNamespace("sf", quietly = TRUE))
        stop("Package 'sf' is required for GISCO boundaries.", call. = FALSE)

    adm <- gisco_nuts_spatial(
        year = year,
        resolution = resolution,
        level = 3,
        path = path,
        quiet = TRUE
    )

    pieces <- lapply(prov, function(pr) {
        filter_gisco_province_spatial(adm, prov = pr)
    })

    b <- do.call(rbind, pieces)
    if (!is.null(target.crs) && !is.na(sf::st_crs(b)))
        b <- sf::st_transform(b, target.crs)

    matched_codes <- vapply(pieces, function(z)
        attr(z, "matched_province_code", exact = TRUE), character(1))
    matched_nuts <- unique(unlist(lapply(pieces, function(z)
        attr(z, "matched_gisco_nuts_id", exact = TRUE)), use.names = FALSE))
    matched_names <- unique(unlist(lapply(pieces, function(z)
        attr(z, "matched_gisco_name", exact = TRUE)), use.names = FALSE))

    list(
        version = 2L,
        geometry = b,
        source = "GISCO_NUTS",
        role = "province_border",
        boundary_level = 3L,
        matched_province = prov,
        matched_province_code = matched_codes,
        matched_gisco_nuts_id = matched_nuts,
        matched_gisco_name = matched_names,
        gisco_year = attr(adm, "gisco_year", exact = TRUE),
        gisco_resolution = attr(adm, "gisco_resolution", exact = TRUE),
        gisco_level = attr(adm, "gisco_level", exact = TRUE),
        gisco_source_url = attr(adm, "gisco_source_url", exact = TRUE),
        crs = sf::st_crs(b),
        note = paste(
            "Eurostat/GISCO NUTS boundaries are stored as an optional",
            "cartographic reference for plotting and spatial checks. They are",
            "not Spanish NFI plot geometries. Spanish provinces are matched to",
            "NUTS-3 units; Canary Island provinces aggregate their island NUTS-3",
            "units."
        ),
        created = Sys.time()
    )
}

.basifoR_spatial_make_boundary_safe <- function(nfi, boundary = FALSE,
                                                boundary.source = c("auto", "gisco", "gadm", "user", "none"),
                                                boundary.object = NULL,
                                                boundary.level = 2,
                                                boundary.path = tools::R_user_dir("basifoR", "cache"),
                                                boundary.ext = "json",
                                                boundary.version = "4.1",
                                                boundary.crs = NULL,
                                                target.crs = NULL,
                                                allow.gadm = TRUE,
                                                validate = TRUE) {
    boundary.source <- match.arg(boundary.source)
    if (identical(boundary.source, "none") || !isTRUE(boundary))
        return(NULL)

    if (identical(boundary.source, "user") && is.null(boundary.object)) {
        if (isTRUE(validate)) {
            warning(
                "'boundary.source = \"user\"' requires 'boundary.object'.",
                call. = FALSE
            )
        }
        return(NULL)
    }

    if (!is.null(boundary.object) || identical(boundary.source, "user")) {
        return(tryCatch(
            .basifoR_spatial_boundary_from_object(
                boundary.object = boundary.object,
                boundary.crs = boundary.crs,
                target.crs = target.crs,
                role = "user_boundary"
            ),
            error = function(e) {
                if (isTRUE(validate))
                    warning("Could not attach user boundary: ", conditionMessage(e),
                            call. = FALSE)
                NULL
            }
        ))
    }

    if (!isTRUE(allow.gadm)) {
        if (identical(boundary.source, "gadm") && isTRUE(validate)) {
            warning(
                paste(
                    "Automatic built-in boundaries are limited to Spanish NFI inputs.",
                    "Supply 'boundary.object' for an external inventory."
                ),
                call. = FALSE
            )
        }
        return(NULL)
    }

    ## Automatic built-in boundaries are attempted only for Spanish
    ## province-like inputs. Here `nfi` may be the original user input or a
    ## vector of actual province codes discovered in the imported data. The
    ## latter case is used to respect basifoR/procods data-assignment decisions.
    province_vector <- function(z) {
        if (is.numeric(z) || is.integer(z))
            return(unique(z[!is.na(z)]))
        if (is.character(z)) {
            z <- trimws(z)
            z <- z[!is.na(z) & nzchar(z)]
            if (!length(z))
                return(NULL)
            if (all(grepl("^[0-9]{1,2}$", z)))
                return(unique(z))
            if (length(z) == 1L)
                return(z)
        }
        NULL
    }

    prov <- province_vector(nfi)
    if (is.null(prov)) {
        if (!.basifoR_spatial_probably_snfi_source(nfi))
            return(NULL)
        prov <- .basifoR_spatial_input_province(nfi)
        if (is.na(prov) && is.character(nfi) && length(nfi) == 1L)
            prov <- nfi
    }

    try_gisco <- identical(boundary.source, "auto") ||
        identical(boundary.source, "gisco")
    try_gadm <- identical(boundary.source, "gadm")

    if (isTRUE(try_gisco)) {
        out <- tryCatch(
            .basifoR_spatial_boundary_from_gisco(
                prov = prov,
                path = boundary.path,
                target.crs = target.crs,
                validate = validate
            ),
            error = function(e) {
                if (isTRUE(validate) && identical(boundary.source, "gisco"))
                    warning("Could not attach GISCO boundary: ",
                            conditionMessage(e), call. = FALSE)
                NULL
            }
        )
        if (!is.null(out) || identical(boundary.source, "gisco"))
            return(out)
    }

    if (!isTRUE(try_gadm))
        try_gadm <- identical(boundary.source, "auto")

    if (!isTRUE(try_gadm))
        return(NULL)

    tryCatch(
        .basifoR_spatial_boundary_from_gadm(
            prov = prov,
            level = boundary.level,
            path = boundary.path,
            ext = boundary.ext,
            version = boundary.version,
            target.crs = target.crs,
            validate = validate
        ),
        error = function(e) {
            if (isTRUE(validate))
                warning("Could not attach automatic boundary: ",
                        conditionMessage(e),
                        call. = FALSE)
            NULL
        }
    )
}

.basifoR_spatial_attach_boundary <- function(x, boundary_registry,
                                             requested = FALSE) {
    if (is.null(boundary_registry)) {
        attr(x, "has_nfi_boundary") <- FALSE
        attr(x, "boundary_requested") <- requested
        attr(x, "nfi_boundary_failed") <- isTRUE(requested)
        return(x)
    }
    attr(x, "nfi_boundary") <- boundary_registry
    attr(x, "has_nfi_boundary") <- TRUE
    attr(x, "boundary_requested") <- requested
    attr(x, "nfi_boundary_failed") <- FALSE
    x
}

### Return the optional administrative-boundary sidecar.
getNFIboundary_spatial <- function(
x ##<< Object from which to retrieve the administrative-boundary sidecar stored in the \code{nfi_boundary} attribute.
) {
    ## Return the optional boundary sidecar stored by readNFI_spatial().
    attr(x, "nfi_boundary", exact = TRUE)
}

### Test whether an object contains a valid boundary sidecar.
hasNFIboundary_spatial <- function(
    x ##<< Object to test for a valid administrative-boundary sidecar.
) {
    ## Test whether an object carries an optional boundary sidecar.
    b <- getNFIboundary_spatial(x)
    !is.null(b) && is.list(b) && inherits(b$geometry, "sf")
}

### Copy the administrative-boundary sidecar between objects.
copyNFIboundary_spatial <- function(
    from, ##<< Source object carrying an administrative-boundary sidecar.
    to ##<< Target object that will receive the boundary sidecar.
) {
    ## Copy the boundary sidecar from one object to another.
    b <- attr(from, "nfi_boundary", exact = TRUE)
    if (!is.null(b)) {
        attr(to, "nfi_boundary") <- b
        attr(to, "has_nfi_boundary") <- TRUE
        attr(to, "nfi_boundary_failed") <- FALSE
        br <- attr(from, "boundary_requested", exact = TRUE)
        if (!is.null(br))
            attr(to, "boundary_requested") <- br
    } else {
        br <- attr(from, "boundary_requested", exact = TRUE)
        bf <- attr(from, "nfi_boundary_failed", exact = TRUE)
        if (!is.null(br))
            attr(to, "boundary_requested") <- br
        if (!is.null(bf))
            attr(to, "nfi_boundary_failed") <- bf
        if (!is.null(bf) && isTRUE(bf))
            attr(to, "has_nfi_boundary") <- FALSE
    }
    to
}

### Copy both plot-geometry and administrative-boundary sidecars.
copyNFIspatial_sidecars <- function(
    from, ##<< Source object carrying plot-geometry and administrative-boundary sidecars.
    to ##<< Target object that will receive both spatial sidecars.
) {
    ## Copy both plot-geometry and boundary sidecars.
    to <- copyNFIgeometry_spatial(from = from, to = to)
    to <- copyNFIboundary_spatial(from = from, to = to)
    to
}

.basifoR_spatial_finish <- function(x, registry, requested = "attribute",
                                    validate = TRUE,
                                    boundary_registry = NULL,
                                    boundary_requested = FALSE) {
    z <- .basifoR_spatial_attach_registry(x, registry, requested = requested)
    z <- .basifoR_spatial_attach_boundary(z, boundary_registry,
                                          requested = boundary_requested)
    if (identical(requested, "sf") && !is.null(registry)) {
        zsf <- asNFI_spatial_sf(z, registry = registry, validate = validate)
        zsf <- .basifoR_spatial_attach_boundary(zsf, boundary_registry,
                                                requested = boundary_requested)
        return(zsf)
    }
    z
}

readNFI_spatial <- structure(function
### Read SNFI data and store plot geometry as an attribute sidecar
###
### This function is a conservative spatial companion to \code{readNFI()}.
### It keeps the ordinary tabular output of \code{readNFI()} and stores a
### plot-level geometry registry in \code{attr(x, "nfi_geometry_registry")}.
### By default, the returned object remains a data frame and carries geometry
### as an attribute sidecar. If \code{spatial = "sf"}, the function returns an
### \code{sf} object by joining the sidecar geometry back to each row. The
### attribute-sidecar default lets downstream metric functions carry spatial
### information without pushing a geometry column through tabular routines.
(
    nfi, ##<< Inventory source passed to \code{readNFI()}: province code or
         ## name, local/remote zip archive, decompressed files, or a data frame.
    nfi.nr = 4, ##<< Spanish NFI stage. Use \code{2}, \code{3}, or \code{4}.
    dt.nm = "PCMayores", ##<< Main table to read and return as tabular data.
    file_ext = NULL, ##<< Optional file extension passed to \code{readNFI()} and
                     ## \code{readNFIsf()} when the input must be fetched or
                     ## filtered.
    file_name = NULL, ##<< Optional file-name filter passed to \code{readNFI()}.
                      ## The spatial registry reads coordinates independently.
    ..., ##<< Additional arguments passed to \code{readNFI()} and
         ## \code{readNFIsf()}, such as \code{dir} or \code{timeOut}.
    spatial = c("attribute", "sf", "none"), ##<< \code{"attribute"} stores a
                                       ## plot-level spatial registry in an
                                       ## attribute and keeps a tabular output.
                                       ## \code{"sf"} returns an \code{sf}
                                       ## object by joining the sidecar geometry
                                       ## to each row. \code{"none"} returns
                                       ## the ordinary \code{readNFI()} output.
    geometry.dt.nm = dt.nm, ##<< Table used to build the geometry registry. In
                            ## most SNFI workflows this can match \code{dt.nm}.
    coord.nm = NULL, ##<< Optional SNFI coordinate table name. If \code{NULL},
                     ## the function detects \code{DATEST} for IFN2 and
                     ## \code{PCDatosMap}/\code{Listado definitivo} for IFN3/IFN4.
    schema = NULL, ##<< Optional \code{external_schema}. For external
                   ## inventories, spatial metadata can be stored in
                   ## \code{schema$defaults$spatial}, for example
                   ## a named \code{list} containing plot, x, y, and crs entries.
                   ## Explicit coordinate arguments below override schema values.
    coords = NULL, ##<< Optional external coordinate table. Use this for non-
                   ## Spanish inventories when coordinates live in a separate
                   ## table. It can be a data frame or an \code{sf} object.
    x.col = NULL, ##<< Optional X/easting/longitude column for external
                  ## coordinate tables. If \code{NULL}, common names such as
                  ## \code{x}, \code{coord_x}, \code{easting}, and
                  ## \code{longitude} are detected.
    y.col = NULL, ##<< Optional Y/northing/latitude column for external
                  ## coordinate tables. If \code{NULL}, common names such as
                  ## \code{y}, \code{coord_y}, \code{northing}, and
                  ## \code{latitude} are detected.
    pr.col = NULL, ##<< Optional province, region, or stratum key for external
                   ## coordinate matching. It is not required for one-region
                   ## external inventories.
    plot.col = NULL, ##<< Optional plot identifier column for external
                     ## coordinate matching. If \code{NULL}, common plot names
                     ## such as \code{plot_id}, \code{sample_id}, and
                     ## \code{site_id} are detected.
    crs = NULL, ##<< Optional CRS for external coordinates, passed to
                ## \code{sf::st_as_sf()}. Use an EPSG code such as
                ## \code{25830} or \code{4326}.
    coord.units = "m", ##<< Coordinate units recorded in the external spatial
                       ## sidecar metadata.
    coord.accuracy.note = NULL, ##<< Optional note describing the accuracy or
                               ## provenance of user-supplied coordinates. This is
                               ## most useful when replacing public inventory
                               ## coordinates with field-validated plot-centre
                               ## coordinates. Spanish NFI public coordinates
                               ## receive an automatic accuracy note.
    geometry.source = c("auto", "snfi", "external", "none"), ##<< Source used
                       ## to build the sidecar. \code{"auto"} first uses
                       ## explicit external coordinates when available, then
                       ## tries SNFI geometry only for province-like sources.
    validate = TRUE, ##<< Warn when duplicate plot geometries are found or when
                     ## geometry cannot be attached.
    coord.factor = NULL, ##<< Optional coordinate multiplier for SNFI coordinate
                         ## tables. By default IFN2 kilometre coordinates are
                         ## detected and converted to metres.
    huso.method = c("auto", "candidate", "xgap", "province", "none"), ##<< Method for
                         ## assigning SNFI UTM zones when the coordinate table
                         ## does not contain a direct \code{Huso} column.
                         ## \code{"auto"} uses table Huso, then the historical
                         ## \code{huso1}/\code{huso2}/\code{huso3} +
                         ## \code{CoorX} rule, then province fallback.
    huso.candidates = NULL, ##<< Optional candidate UTM zones for SNFI Huso inference.
                       ## Use, for example, \code{c(29, 30)} for provinces
                       ## that span zones 29 and 30 when no Huso columns are
                       ## available.
    repair.table.huso = TRUE, ##<< \code{logical}. If \code{TRUE}, direct
                       ## table-supplied Huso values are checked against the
                       ## candidate-zone \code{CoorX} threshold rule before
                       ## building geometry. This repairs boundary-zone cases
                       ## such as plots stored in the neighbouring UTM zone.
    target.crs = NULL, ##<< Optional common CRS for projected SNFI geometries.
                       ## When \code{NULL}, a single source EPSG is kept, or
                       ## mixed ED50 zones are transformed to EPSG:23030.
    boundary = FALSE, ##<< \code{logical}. If \code{TRUE}, attach an optional
                      ## boundary sidecar in \code{attr(x, "nfi_boundary")}.
                      ## Spanish NFI province-like inputs can use built-in
                      ## GISCO/GADM download helpers; external inventories must
                      ## provide \code{boundary.object}.
    boundary.source = c("auto", "gisco", "gadm", "user", "none"), ##<< Boundary
                      ## source. \code{"auto"} uses a user object when supplied
                      ## and otherwise tries GISCO/NUTS before GADM for Spanish
                      ## province-like inputs.
    boundary.object = NULL, ##<< Optional user-provided \code{sf} polygon or
                      ## multipolygon layer. External inventories must use this
                      ## route; automatic built-in boundary selection is limited
                      ## to Spanish NFI inputs.
    boundary.level = 2, ##<< GADM administrative level used when
                      ## \code{boundary.source = "gadm"}. GISCO uses NUTS-3.
    boundary.path = tools::R_user_dir("basifoR", "cache"), ##<< Cache
                      ## directory used for optional boundary downloads.
    boundary.ext = "json", ##<< GADM extension used by \code{gadm_spatial()}.
    boundary.version = "4.1", ##<< GADM version used by \code{gadm_spatial()}.
    boundary.crs = NULL, ##<< CRS assigned to \code{boundary.object} when it
                      ## lacks CRS metadata. It is required when the supplied
                      ## object has no CRS.
    infer.huso = NULL ##<< Backward-compatible argument forwarded to the
                      ## fallback \code{readNFIsf()} route. The new SNFI table
                      ## route prefers \code{huso.method}.
) {
    spatial <- match.arg(spatial)
    geometry.source <- match.arg(geometry.source)
    huso.method <- match.arg(huso.method)
    boundary.source <- match.arg(boundary.source)

    ##details<< For Spanish NFI sources, the coordinate note is stage-specific
    ## and is stored in
    ## \code{attr(x, "nfi_geometry_registry")$coord_accuracy_note}.
    ## IFN2 coordinates are treated as official public locations
    ## derived from the kilometre UTM sampling grid. IFN3 coordinates
    ## are treated as official cartographic plot-location coordinates
    ## in UTM metres; when their \code{Huso} is not explicit, CRS
    ## metadata may be reconstructed from candidate Huso fields,
    ## coordinate rules, or fallback methods. IFN4 \code{CoorX}/
    ## \code{CoorY} are treated as official public coordinates
    ## available before fieldwork for the plot centre. These stage
    ## notes document coordinate interpretation; they do not claim
    ## field-validated exact plot-centre precision.
    ##
    ## External inventories or user-supplied coordinate tables do not
    ## receive an automatic coordinate-accuracy note because basifoR
    ## cannot know their measurement protocol or precision. If better
    ## coordinates become available, for example field GPS or
    ## institutional plot-centre coordinates, pass them through
    ## \code{coords}, \code{x.col}, \code{y.col}, \code{plot.col},
    ## and optionally \code{pr.col}, with \code{geometry.source =
    ## "external"} and provide \code{coord.accuracy.note} only when
    ## the coordinate source is known. This replaces the spatial
    ## sidecar while keeping the ordinary \code{readNFI()} table and
    ## downstream metric calculations unchanged.

    schema_spatial <- .basifoR_spatial_schema_spatial(schema)
    if (!is.null(schema_spatial)) {
        coords <- .basifoR_spatial_null_or(
            coords,
            .basifoR_spatial_schema_value(schema_spatial, c("coords", "coord_table"))
        )
        x.col <- .basifoR_spatial_null_or(
            x.col,
            .basifoR_spatial_schema_value(schema_spatial, c("x", "x.col", "easting", "longitude"))
        )
        y.col <- .basifoR_spatial_null_or(
            y.col,
            .basifoR_spatial_schema_value(schema_spatial, c("y", "y.col", "northing", "latitude"))
        )
        pr.col <- .basifoR_spatial_null_or(
            pr.col,
            .basifoR_spatial_schema_value(schema_spatial, c("pr", "pr.col", "region", "region_col", "province"))
        )
        plot.col <- .basifoR_spatial_null_or(
            plot.col,
            .basifoR_spatial_schema_value(schema_spatial, c("plot", "plot.col", "plot_id", "idp"))
        )
        plot.col <- .basifoR_spatial_null_or(
            plot.col,
            .basifoR_spatial_first_schema_alias(schema, "plot")
        )
        crs <- .basifoR_spatial_null_or(
            crs,
            .basifoR_spatial_schema_value(schema_spatial, c("crs", "epsg"))
        )
        coord.units <- .basifoR_spatial_null_or(
            .basifoR_spatial_schema_value(schema_spatial, c("coord.units", "coord_units", "units")),
            coord.units
        )
        if (identical(geometry.source, "auto"))
            geometry.source <- "external"
    }

    if (identical(geometry.source, "none"))
        spatial <- "none"

    if (inherits(nfi, "sf")) {
        if (!requireNamespace("sf", quietly = TRUE))
            stop("Package 'sf' is required to process sf input.", call. = FALSE)

        out <- sf::st_drop_geometry(nfi)
        if (is.null(attr(out, "nfi.nr", exact = TRUE)))
            attr(out, "nfi.nr") <- as.integer(nfi.nr)[1L]

        if (identical(spatial, "none"))
            return(out)

        reg <- .basifoR_spatial_registry_from_sf(
            nfi,
            source = "sf_input",
            source_dt.nm = geometry.dt.nm,
            validate = validate,
            pr.col = pr.col,
            plot.col = plot.col
        )
        boundary_context <- .basifoR_spatial_boundary_context(
            data = out,
            registry = reg,
            fallback = NULL
        )
        return(.basifoR_spatial_finish(
            out, reg, requested = spatial, validate = validate,
            boundary_registry = .basifoR_spatial_make_boundary_safe(
            nfi = boundary_context, boundary = boundary, boundary.source = boundary.source,
            boundary.object = boundary.object, boundary.level = boundary.level,
            boundary.path = boundary.path, boundary.ext = boundary.ext,
            boundary.version = boundary.version, boundary.crs = boundary.crs,
            target.crs = if (!is.null(reg) && inherits(reg$geometry, "sf")) sf::st_crs(reg$geometry) else NULL,
            allow.gadm = FALSE,
            validate = validate
        ),
            boundary_requested = boundary
        ))
    }

    read_args <- c(
        list(nfi = nfi, nfi.nr = nfi.nr, dt.nm = dt.nm),
        list(...)
    )
    if (!is.null(file_ext))
        read_args$file_ext <- file_ext
    if (!is.null(file_name))
        read_args$file_name <- file_name

    out <- do.call(readNFI, read_args)
    boundary_context <- .basifoR_spatial_boundary_context(
        data = out,
        registry = NULL,
        fallback = nfi
    )

    if (!is.data.frame(out) || identical(spatial, "none"))
        return(out)

    ## External inventories are handled before SNFI-specific coordinate
    ## discovery.  This keeps compatible CSV/data-frame workflows tabular by
    ## default, but allows a spatial sidecar when coordinates are supplied or
    ## when the main table already contains coordinate columns.
    external_requested <- identical(geometry.source, "external") ||
        !is.null(schema_spatial) || !is.null(coords) || !is.null(x.col) ||
        !is.null(y.col) || !is.null(crs)

    if (identical(geometry.source, "external") ||
        (identical(geometry.source, "auto") && external_requested)) {
        ext_reg <- tryCatch(
            .basifoR_spatial_registry_from_xy(
                x = out,
                coords = coords,
                x.col = x.col,
                y.col = y.col,
                pr.col = pr.col,
                plot.col = plot.col,
                crs = crs,
                coord.units = coord.units,
                coord.accuracy.note = coord.accuracy.note,
                source = if (is.null(coords)) "external_main_table" else "external_coords",
                source_dt.nm = dt.nm,
                validate = validate
            ),
            error = function(e) e
        )

        if (!inherits(ext_reg, "error"))
            return(.basifoR_spatial_finish(
            out, ext_reg, requested = spatial, validate = validate,
            boundary_registry = .basifoR_spatial_make_boundary_safe(
            nfi = boundary_context, boundary = boundary, boundary.source = boundary.source,
            boundary.object = boundary.object, boundary.level = boundary.level,
            boundary.path = boundary.path, boundary.ext = boundary.ext,
            boundary.version = boundary.version, boundary.crs = boundary.crs,
            target.crs = if (!is.null(ext_reg) && inherits(ext_reg$geometry, "sf")) sf::st_crs(ext_reg$geometry) else NULL,
            allow.gadm = FALSE,
            validate = validate
        ),
            boundary_requested = boundary
        ))

        if (identical(geometry.source, "external")) {
            if (isTRUE(validate)) {
                warning(
                    paste(
                        "Could not build the external spatial sidecar.",
                        conditionMessage(ext_reg),
                        "Returning tabular readNFI() output."
                    ),
                    call. = FALSE
                )
            }
            return(.basifoR_spatial_finish(
            out, NULL, requested = spatial, validate = validate,
            boundary_registry = .basifoR_spatial_make_boundary_safe(
            nfi = boundary_context, boundary = boundary, boundary.source = boundary.source,
            boundary.object = boundary.object, boundary.level = boundary.level,
            boundary.path = boundary.path, boundary.ext = boundary.ext,
            boundary.version = boundary.version, boundary.crs = boundary.crs,
            target.crs = NULL,
            allow.gadm = FALSE,
            validate = validate
        ),
            boundary_requested = boundary
        ))
        }
    }

    if (identical(geometry.source, "auto") &&
        !.basifoR_spatial_probably_snfi_source(nfi)) {
        ## External file paths and already decompressed compatible exports stay
        ## tabular unless the user supplies coords/x.col/y.col or sets
        ## geometry.source = "snfi" explicitly.
        return(.basifoR_spatial_finish(
            out, NULL, requested = spatial, validate = validate,
            boundary_registry = .basifoR_spatial_make_boundary_safe(
            nfi = boundary_context, boundary = boundary, boundary.source = boundary.source,
            boundary.object = boundary.object, boundary.level = boundary.level,
            boundary.path = boundary.path, boundary.ext = boundary.ext,
            boundary.version = boundary.version, boundary.crs = boundary.crs,
            target.crs = NULL,
            allow.gadm = FALSE,
            validate = validate
        ),
            boundary_requested = boundary
        ))
    }

    ## Preferred SNFI geometry route: read the coordinate table directly,
    ## infer per-plot Huso when possible, and transform all plots to one
    ## common CRS.  This fixes provinces that span zones 29/30 or 30/31 when
    ## the coordinate table carries historical huso1/huso2/huso3 fields.
    snfi_reg <- tryCatch(
        .basifoR_spatial_registry_from_snfi_tables(
            nfi = nfi,
            nfi.nr = nfi.nr,
            dots = list(...),
            file_ext = file_ext,
            coord.nm = coord.nm,
            coord.factor = coord.factor,
            huso.method = huso.method,
            target.crs = target.crs,
            huso.candidates = huso.candidates,
            repair.table.huso = repair.table.huso,
            source_dt.nm = geometry.dt.nm,
            validate = validate
        ),
        error = function(e) e
    )

    if (!inherits(snfi_reg, "error"))
        return(.basifoR_spatial_finish(
            out, snfi_reg, requested = spatial, validate = validate,
            boundary_registry = .basifoR_spatial_make_boundary_safe(
            nfi = boundary_context, boundary = boundary, boundary.source = boundary.source,
            boundary.object = boundary.object, boundary.level = boundary.level,
            boundary.path = boundary.path, boundary.ext = boundary.ext,
            boundary.version = boundary.version, boundary.crs = boundary.crs,
            target.crs = if (!is.null(snfi_reg) && inherits(snfi_reg$geometry, "sf")) sf::st_crs(snfi_reg$geometry) else NULL,
            validate = validate
        ),
            boundary_requested = boundary
        ))

    if (isTRUE(validate)) {
        warning(
            paste(
                "Could not build the improved SNFI spatial sidecar from the coordinate table.",
                conditionMessage(snfi_reg),
                "Trying the readNFIsf() fallback."
            ),
            call. = FALSE
        )
    }

    if (!exists("readNFIsf", mode = "function")) {
        if (isTRUE(validate)) {
            warning(
                "readNFIsf() was not found. Returning tabular readNFI() output without spatial sidecar.",
                call. = FALSE
            )
        }
        return(.basifoR_spatial_finish(
            out, NULL, requested = spatial, validate = validate,
            boundary_registry = .basifoR_spatial_make_boundary_safe(
            nfi = boundary_context, boundary = boundary, boundary.source = boundary.source,
            boundary.object = boundary.object, boundary.level = boundary.level,
            boundary.path = boundary.path, boundary.ext = boundary.ext,
            boundary.version = boundary.version, boundary.crs = boundary.crs,
            target.crs = NULL,
            validate = validate
        ),
            boundary_requested = boundary
        ))
    }

    sf_args <- c(
        list(nfi = nfi, nfi.nr = nfi.nr, dt.nm = geometry.dt.nm),
        list(...)
    )
    if (!is.null(file_ext))
        sf_args$file_ext <- file_ext
    if (!is.null(coord.nm))
        sf_args$coord.nm <- coord.nm
    ## Do not pass file_name to readNFIsf() for the geometry registry.  The
    ## coordinate table must remain visible even when the main-table read is
    ## file-filtered.
    if (!is.null(infer.huso))
        sf_args$infer.huso <- infer.huso

    xsf <- tryCatch(
        do.call(readNFIsf, sf_args),
        error = function(e) e
    )

    if (inherits(xsf, "error") || !inherits(xsf, "sf")) {
        if (isTRUE(validate)) {
            warning(
                paste(
                    "Could not build the spatial sidecar.",
                    if (inherits(xsf, "error")) conditionMessage(xsf) else "readNFIsf() did not return sf.",
                    "Returning tabular readNFI() output."
                ),
                call. = FALSE
            )
        }
        return(.basifoR_spatial_finish(
            out, NULL, requested = spatial, validate = validate,
            boundary_registry = .basifoR_spatial_make_boundary_safe(
            nfi = boundary_context, boundary = boundary, boundary.source = boundary.source,
            boundary.object = boundary.object, boundary.level = boundary.level,
            boundary.path = boundary.path, boundary.ext = boundary.ext,
            boundary.version = boundary.version, boundary.crs = boundary.crs,
            target.crs = NULL,
            validate = validate
        ),
            boundary_requested = boundary
        ))
    }

    reg <- .basifoR_spatial_registry_from_sf(
        xsf,
        source = "readNFIsf",
        source_dt.nm = geometry.dt.nm,
        validate = validate,
        pr.col = pr.col,
        plot.col = plot.col
    )

    .basifoR_spatial_finish(
            out, reg, requested = spatial, validate = validate,
            boundary_registry = .basifoR_spatial_make_boundary_safe(
            nfi = boundary_context, boundary = boundary, boundary.source = boundary.source,
            boundary.object = boundary.object, boundary.level = boundary.level,
            boundary.path = boundary.path, boundary.ext = boundary.ext,
            boundary.version = boundary.version, boundary.crs = boundary.crs,
            target.crs = if (!is.null(reg) && inherits(reg$geometry, "sf")) sf::st_crs(reg$geometry) else NULL,
            validate = validate
        ),
            boundary_requested = boundary
        )
    ##value<< A data frame with the same tabular content returned by
    ## \code{readNFI()}. When \code{spatial = "attribute"}, the object also
    ## carries a plot-level geometry sidecar in
    ## \code{attr(x, "nfi_geometry_registry")}. The sidecar stores one geometry
    ## per inventory plot, CRS metadata, coordinate provenance, and normalized
    ## join keys for later \code{*_spatial()} metric functions.
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

	    x <- readNFI_spatial(
	        toy,
	        spatial = "attribute",
	        geometry.source = "external",
	        plot.col = "esta",
	        x.col = "x",
	        y.col = "y",
	        crs = 4326,
	        boundary = FALSE
	    )

	    hasNFIgeometry_spatial(x)
	    names(getNFIgeometry_spatial(x))

	    ## Keep the ordinary readNFI() output but carry plot geometry in attributes.
	    ## cache <- tools::R_user_dir("basifoR", "cache")
	    ## x <- readNFI_spatial(28, nfi.nr = 3, dt.nm = "PCMayores", dir = cache)
    ## is.data.frame(x)
    ## hasNFIgeometry_spatial(x)
    ## reg <- getNFIgeometry_spatial(x)
    ## reg$geometry

    ## Return a true sf object when direct spatial output is wanted.
    ## xsf <- readNFI_spatial(28, nfi.nr = 3, dt.nm = "PCMayores",
    ##                        dir = cache, spatial = "sf")
    ## inherits(xsf, "sf")

    ## External inventory: keep tabular data but store geometry sidecar.
    ## ext <- data.frame(plot_id = 1:2, x = c(440000, 441000),
    ##                   y = c(4488000, 4489000), d = c(20, 30))
    ## y <- readNFI_spatial(ext, spatial = "attribute",
    ##                      geometry.source = "external",
    ##                      plot.col = "plot_id", x.col = "x", y.col = "y",
    ##                      crs = 25830)
    ## hasNFIgeometry_spatial(y)
    ##
    ## External inventory with schema-stored spatial metadata:
    ## sch <- new_external_schema(
    ##     colmap = list(plot = "plot_id", d = "d"),
    ##     units = list(d = "cm"),
    ##     levels = "plot_id",
    ##     defaults = list(spatial = list(plot = "plot_id", x = "x", y = "y", crs = 25830))
    ## )
    ## y2 <- readNFI_spatial(ext, spatial = "attribute", schema = sch)
    ## hasNFIgeometry_spatial(y2)
    ##
    ## Replacing public SNFI coordinates with improved coordinates uses the
    ## same external sidecar mechanism.  The improved table must contain the
    ## inventory plot keys and coordinate columns.
    ## improved <- data.frame(pr = 28, Estadillo = c(1, 2),
    ##                        x_gps = c(440000, 441000),
    ##                        y_gps = c(4488000, 4489000))
	    ## z <- readNFI_spatial(28, nfi.nr = 4, spatial = "attribute",
	    ##                      geometry.source = "external", coords = improved,
	    ##                      pr.col = "pr", plot.col = "Estadillo",
	    ##                      x.col = "x_gps", y.col = "y_gps", crs = 25830,
	    ##                      coord.accuracy.note = "Field-validated plot-centre coordinates.")
	})

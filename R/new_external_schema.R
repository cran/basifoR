new_external_schema <- structure(function #Define a schema for external inventory workflows
### Create a reusable \code{"external_schema"} object that records how
### source columns in an external inventory correspond to the standardized
### field names used by the basifoR external workflow. The schema can also
### store declared measurement units, default grouping levels, columns to
### preserve during processing, and auxiliary defaults that wrapper
### functions may reuse across repeated calls.
(
    colmap, ##<< Named \code{list} mapping standardized variables to one or more candidate source column names in the input data.
    units = list(), ##<< Named \code{list} of declared units for standardized variables, usually entries such as \code{list(d = "mm", h = "m")}.
    levels = NULL, ##<< Optional \code{character} vector of default grouping variables for downstream summaries.
    keep_cols = NULL, ##<< Optional \code{character} vector naming source columns that should be retained in downstream outputs.
    defaults = list() ##<< Optional named \code{list} of auxiliary defaults or metadata that wrappers may reuse.
) {
    ##description<< Create a reusable schema with column aliases, units, default grouping settings, retained columns, and optional defaults for the external basifoR workflow.
    ##details<<
    ##details<< The constructor standardizes all mapping entries to
    ##details<< non-empty character vectors. This lets wrappers search for
    ##details<< several possible source column names for the same
    ##details<< standardized variable.
    ##details<<
    ##details<< Unit entries are stored as single character values, while
    ##details<< \code{levels} and \code{keep_cols} are normalized to
    ##details<< non-empty character vectors. Empty strings and missing
    ##details<< values are removed during normalization.
    ##details<<
    ##details<< The returned object is lightweight. It validates the basic
    ##details<< structure of the inputs, assigns class
    ##details<< \code{c("external_schema", "list")}, and leaves semantic
    ##details<< interpretation to downstream helpers such as
    ##details<< \code{externalMetrics()}, \code{externalMetrics2Vol()}, or
    ##details<< \code{external_dendroMetrics()}.
    if (!is.list(colmap) || !length(colmap)) {
        stop("'colmap' must be a non-empty named list.", call. = FALSE)
    }
    if (is.null(names(colmap)) || anyNA(names(colmap)) || any(names(colmap) == "")) {
        stop("'colmap' must be a named list.", call. = FALSE)
    }
    if (!is.list(units)) {
        stop("'units' must be a named list.", call. = FALSE)
    }
    if (length(units) && (is.null(names(units)) || anyNA(names(units)) || any(names(units) == ""))) {
        stop("'units' must be a named list.", call. = FALSE)
    }

    normalize_chr <- function(x) {
        x <- unlist(x, use.names = FALSE)
        x <- as.character(x)
        x[!is.na(x) & nzchar(x)]
    }

    colmap <- lapply(colmap, normalize_chr)
    units <- lapply(units, function(x) as.character(x)[1L])

    schema <- list(
        colmap = colmap,
        units = units,
        levels = normalize_chr(levels),
        keep_cols = normalize_chr(keep_cols),
        defaults = defaults %||% list()
    )
    class(schema) <- c("external_schema", "list")
    schema
    ##value<< An object of class \code{"external_schema"} containing normalized \code{colmap}, \code{units}, \code{levels}, \code{keep_cols}, and \code{defaults} components, ready to pass to external workflow wrappers.
}, ex = function() {
    sch <- new_external_schema(
        colmap = list(
            plot = c("plot_id", "plot"),
            species = c("species_code", "sp"),
            d = c("dbh_mm", "diameter_mm"),
            h = c("height_m", "h")
        ),
        units = list(d = "mm", h = "m"),
        levels = "plot",
        keep_cols = c("plot_id", "species_code"),
        defaults = list(selector = "priority")
    )

    class(sch)
    sch$colmap$d
    sch$units
    sch$levels
})


print.external_schema <- function #Print an external schema summary
##title<< Print an external schema summary
##description<< Display the column mappings, declared units, and default grouping levels stored in an \code{"external_schema"} object.
(
    x, ##<< An \code{"external_schema"} object created by \code{\link{new_external_schema}}.
    ... ##<< Additional arguments accepted for S3 compatibility and ignored by this method.
) {
    cat("<external_schema>\n")
    cat("Columns:\n")
    for (nm in names(x$colmap)) {
        cat("  - ", nm, ": ", paste(x$colmap[[nm]], collapse = ", "), "\n", sep = "")
    }
    if (length(x$units)) {
        cat("Units:\n")
        for (nm in names(x$units)) {
            cat("  - ", nm, ": ", x$units[[nm]], "\n", sep = "")
        }
    }
    if (length(x$levels))
        cat("Levels: ", paste(x$levels, collapse = ", "), "\n", sep = "")
    invisible(x)
    ##value<< The input \code{"external_schema"} object, returned invisibly.
}


.resolve_external_schema <- function(
    schema = NULL,
    levels = NULL,
    keep_cols = NULL,
    colmap = NULL,
    d_unit = NULL,
    h_unit = NULL,
    metric_colmap = NULL,
    volume_colmap = NULL,
    metric_levels = NULL,
    metric_keep_cols = NULL,
    metric_d_unit = NULL,
    metric_h_unit = NULL
) {
    if (is.null(schema)) {
        return(list(
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
        ))
    }

    if (!inherits(schema, "external_schema")) {
        stop("'schema' must inherit from 'external_schema'.", call. = FALSE)
    }

    schema_colmap <- schema$colmap %||% list()
    schema_units <- schema$units %||% list()

    out_colmap <- if (is.null(colmap)) schema_colmap else utils::modifyList(schema_colmap, colmap)
    out_metric_colmap <- if (is.null(metric_colmap)) {
        schema_colmap[names(schema_colmap) %in% c("d", "h")]
    } else {
        utils::modifyList(schema_colmap[names(schema_colmap) %in% c("d", "h")], metric_colmap)
    }
    out_volume_colmap <- if (is.null(volume_colmap)) schema_colmap else utils::modifyList(schema_colmap, volume_colmap)

    list(
        levels = levels %||% schema$levels,
        keep_cols = keep_cols %||% schema$keep_cols,
        colmap = out_colmap,
        d_unit = d_unit %||% schema_units$d,
        h_unit = h_unit %||% schema_units$h,
        metric_colmap = out_metric_colmap,
        volume_colmap = out_volume_colmap,
        metric_levels = metric_levels %||% levels %||% schema$levels,
        metric_keep_cols = metric_keep_cols %||% keep_cols %||% schema$keep_cols,
        metric_d_unit = metric_d_unit %||% d_unit %||% schema_units$d,
        metric_h_unit = metric_h_unit %||% h_unit %||% schema_units$h
    )
}

.externalMetrics_base <- if (exists("externalMetrics", inherits = TRUE)) get("externalMetrics", inherits = TRUE) else NULL
.externalMetrics2Vol_base <- if (exists("externalMetrics2Vol", inherits = TRUE)) get("externalMetrics2Vol", inherits = TRUE) else NULL
.external_dendroMetrics_base <- if (exists("external_dendroMetrics", inherits = TRUE)) get("external_dendroMetrics", inherits = TRUE) else NULL

if (is.null(.externalMetrics_base) || is.null(.externalMetrics2Vol_base) || is.null(.external_dendroMetrics_base)) {
    stop(
        "Load the external workflow first (for example source('external_workflow_bundle_v2.R')) before sourcing this schema patch.",
        call. = FALSE
    )
}


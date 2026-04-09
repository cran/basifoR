\name{external_dendroMetrics}
\alias{external_dendroMetrics}
\title{Summarize external inventory tree data and optional volume outputs}
\description{Process external tree data through a unified workflow that standardizes measurements, computes missing dendrometric variables, optionally derives volume outputs, and returns either tree-level records or grouped stand-level summaries.
The function can work from already standardized inputs or from raw external tables, and it supports repeated processing of several tables with optional parallel execution.}
\usage{external_dendroMetrics(x, 
    summ.vr = NULL, cut.dt = "d == d", 
    report = FALSE, mc.cores = getOption("mc.cores", 
        1L), var = c("d", 
        "h", "ba", "n", 
        "Hd"), parametro = NULL, 
    design = NULL, parameter_table = NULL, 
    method_registry = external_volume_method_registry(), 
    levels = NULL, metric_levels = NULL, 
    keep_cols = NULL, 
    metric_keep_cols = NULL, 
    colmap = NULL, metric_colmap = list(d = c("d", 
        "dbh", "diameter", 
        "diameter_mm"), 
        h = c("h", "height", 
            "height_m")), 
    d_unit = NULL, metric_d_unit = c("mm", 
        "cm")[1], h_unit = NULL, 
    metric_h_unit = c("m", 
        "dm", "cm")[1], 
    tree_d_unit_out = NULL, 
    tree_h_unit_out = NULL, 
    volume_colmap = list(d = c("d"), 
        h = c("h"), dnm = c("dnm", 
            "d_nm", "D.n.m."), 
        v = c("v"), species = c("species", 
            "spec", "especie"), 
        region = c("region", 
            "pr"), equation_set = c("equation_set", 
            "eqset", 
            "tariff", 
            "model_set")), 
    selector = c("first", 
        "priority")[1], 
    track_provenance = FALSE, 
    compute_metrics_if_needed = TRUE, 
    schema = NULL, domheight_fun = null_or(get0("domheight_strict", 
        mode = "function", 
        inherits = TRUE), 
        get0("domheight", 
            mode = "function", 
            inherits = TRUE)), 
    ...)}
\arguments{
  \item{x}{External input table, processed table, or list of such objects.
Raw inputs must contain enough columns to resolve the requested metrics
and, if needed, the volume selectors.}
  \item{summ.vr}{Grouping variable or character vector of grouping variables.
Leave \code{NULL} to keep tree-level output; supply one or more column
names to obtain grouped summaries.}
  \item{cut.dt}{Character filter evaluated on the final table.
The expression is evaluated with \verb{eval(parse(\code{...}))} after metrics or
summaries have been computed.}
  \item{report}{Whether to write the returned table to \file{report.csv}.
The file is written in the working directory only when \code{report = TRUE}.}
  \item{mc.cores}{Number of worker processes used when \code{x} is a list.
Values smaller than 1 are reset to 1.}
  \item{var}{Requested variables.
Typical metric requests are \code{"d"}, \code{"h"}, \code{"ba"},
\code{"n"}, and \code{"Hd"}; volume-like names can also trigger volume
processing.}
  \item{parametro}{Optional volume-method codes, for example \code{"V"}.
If \code{NULL}, the function tries to infer required methods from
\code{var} and \code{method_registry}.}
  \item{design}{Inventory \code{design} used when missing metrics must be computed.
Supply an object inheriting from \code{"inventory_design"} when the
function must derive metrics such as \code{n} from raw external data.}
  \item{parameter_table}{Optional parameter table for the volume stage.
Passed to \code{externalMetrics2Vol()} for method-specific coefficients or
selection metadata.}
  \item{method_registry}{Volume method registry.
Usually created by \code{external_volume_method_registry()} or a custom
registry following the same structure.}
  \item{levels}{Grouping variables forwarded to \code{schema} resolution.
When \code{schema} is not used, these serve as defaults for workflow
grouping metadata.}
  \item{metric_levels}{Grouping variables used during internal metric computation.
These are forwarded specifically to \code{externalMetrics()} when missing
metrics are derived internally.}
  \item{keep_cols}{Source columns to preserve in the output or \code{schema} defaults.
These columns are retained in the returned data whenever possible.}
  \item{metric_keep_cols}{Columns to preserve during internal metric computation.
These are passed to \code{externalMetrics()} when metrics are derived
internally.}
  \item{colmap}{Optional aliases that update the default mappings.
Use this when source names differ from the expected volume or metric names.}
  \item{metric_colmap}{metric_colmap }
  \item{d_unit}{Optional diameter unit override shared with \code{schema} resolution.
Accepted values are \code{"mm"} and \code{"cm"}.}
  \item{metric_d_unit}{Diameter unit expected by \code{metric_colmap}.
Used when metrics must be computed internally.}
  \item{h_unit}{Optional height unit override shared with \code{schema} resolution.
Accepted values are \code{"m"}, \code{"dm"}, and \code{"cm"}.}
  \item{metric_h_unit}{Height unit expected by \code{metric_colmap}.
Used when metrics must be computed internally.}
  \item{tree_d_unit_out}{Optional output unit for tree-level diameter values.
Only used when \code{summ.vr = NULL}. Accepted values are \code{"mm"}
and \code{"cm"}.}
  \item{tree_h_unit_out}{Optional output unit for tree-level height values.
Only used when \code{summ.vr = NULL}. Accepted values are \code{"m"},
\code{"dm"}, and \code{"cm"}.}
  \item{volume_colmap}{volume_colmap }
  \item{selector}{Rule used by \code{externalMetrics2Vol()} when several matches remain.
\code{"first"} keeps the first surviving row; \code{"priority"} uses the
highest numeric \code{priority} value when available.}
  \item{track_provenance}{Whether to keep provenance columns from the volume workflow.
When \code{TRUE}, provenance columns are retained until the final summary
stage removes them from grouped output.}
  \item{compute_metrics_if_needed}{Whether to compute missing metrics from raw inputs.
If \code{FALSE}, the function stops when required standardized columns are
absent.}
  \item{schema}{Optional \code{"external_schema"} object created by \code{new_external_schema()}.
It centralizes column aliases, units, grouping defaults, and kept columns
for repeated workflows.}
  \item{domheight_fun}{domheight_fun }
  \item{\dots}{Additional arguments passed to downstream helpers.
These are mainly useful for custom options in the internal metric or
volume-processing steps.}
}
\details{This wrapper mirrors the arguments and behaviour of \code{dendroMetrics()}.

When \code{summ.vr = NULL}, the function returns tree-level records after applying \code{cut.dt}. When \code{summ.vr} contains one or more grouping variables, the function aggregates by those groups, reporting weighted means for \code{d}, \code{h}, and \code{Hd}, arithmetic sums for \code{ba}, \code{n}, and volume variables, and quadratic mean diameter \code{dg} when diameter and trees-per-hectare are available.

The function can work from already standardized inputs or from raw external tables. If requested metrics are missing and \code{compute_metrics_if_needed = TRUE}, it calls \code{externalMetrics()} internally, so raw inputs usually need a valid \code{design} plus diameter and, when relevant, height mappings. When \code{schema} is supplied, it provides reusable defaults for column aliases, units, grouping variables, and retained columns, while explicit arguments supplied in the call override those defaults.

Volume outputs are optional. They are computed only when \code{parametro} is supplied explicitly or can be inferred from \code{var} and \code{method_registry}. This lets the same entry point handle metric-only workflows, mixed metric-plus-volume workflows, and repeated processing of several input tables with optional parallel execution through \code{mc.cores}.}
\value{A \code{data.frame} with class \verb{c("external_dendroMetrics", "dendroMetrics", \code{...})}.

With \code{summ.vr = NULL}, the returned rows represent tree-level records, optionally converted to \code{tree_d_unit_out} and \code{tree_h_unit_out}. With \code{summ.vr} supplied, the returned rows represent grouped summaries and include standardized summary units such as \code{"cm"}, \code{"m"}, \code{"m2 ha-1"}, \code{"ha-1"}, and \code{"m3 ha-1"} when those variables are present.

The returned object stores the matched call in \code{attr(out, "call")}. When available, it also preserves \code{"units"}, \code{"design_meta"}, and \code{"volume_meta"} attributes from upstream processing, which makes the result suitable for downstream inspection and update methods.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
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

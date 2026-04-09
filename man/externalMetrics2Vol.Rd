\name{externalMetrics2Vol}
\alias{externalMetrics2Vol}
\title{Compute tree-level volume outputs from external inventory data}
\description{Compute one or more tree-level volume outputs from external inventory data or from a precomputed external metrics table.}
\usage{externalMetrics2Vol(x, 
    parametro = c("V"), 
    parameter_table = NULL, 
    method_registry = external_volume_method_registry(), 
    colmap = NULL, selector = c("first", 
        "priority")[1], 
    track_provenance = FALSE, 
    compute_metrics_if_needed = TRUE, 
    design = NULL, var = NULL, 
    metric_var = NULL, 
    levels = NULL, metric_levels = NULL, 
    keep_cols = NULL, 
    metric_keep_cols = NULL, 
    metric_colmap = list(d = c("d", 
        "dbh", "diameter", 
        "diameter_mm"), 
        h = c("h", "height", 
            "height_m")), 
    d_unit = NULL, metric_d_unit = c("mm", 
        "cm")[1], h_unit = NULL, 
    metric_h_unit = c("m", 
        "dm", "cm")[1], 
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
    ...)}
\arguments{
  \item{x}{Standardized columns such as \code{d}, \code{h}, \code{dnm}, or
Input data.frame or object inheriting from \code{"externalMetrics"}.
\code{v} must have named unit metadata in the \code{"units"} attribute of \code{x}.
Input data.frame or object inheriting from \code{"externalMetrics"}.}
  \item{parametro}{Requested volume outputs or method names.}
  \item{parameter_table}{Optional data.frame of coefficients or parameter rows used by the
selected methods. Rows may be filtered by columns such as
\code{parameter}, \code{parametro}, or \code{method}.}
  \item{method_registry}{Named list of method definitions, usually created with
\code{external_volume_method_registry()} and
\code{new_volume_method()}.}
  \item{colmap}{Optional alias list used to resolve contextual inputs for the volume
methods. If \code{NULL}, \code{volume_colmap} is used.}
  \item{selector}{Rule used when several parameter rows remain after filtering.}
  \item{track_provenance}{If \code{TRUE}, append provenance columns and store \code{volume_meta}.}
  \item{compute_metrics_if_needed}{If \code{TRUE}, derive missing standardized inputs with \code{externalMetrics()}.}
  \item{design}{Inventory \code{design} passed to \code{externalMetrics()} when metric derivation is needed.}
  \item{var}{Legacy alias for \code{metric_var}.}
  \item{metric_var}{Metric variables requested during automatic derivation.}
  \item{levels}{Legacy alias for \code{metric_levels}.}
  \item{metric_levels}{Grouping or identifier columns kept during metric derivation.}
  \item{keep_cols}{Legacy alias for \code{metric_keep_cols}.}
  \item{metric_keep_cols}{Extra columns kept during metric derivation.}
  \item{metric_colmap}{metric_colmap }
  \item{d_unit}{Legacy alias for \code{metric_d_unit}.}
  \item{metric_d_unit}{Unit of raw diameter columns used during metric derivation.}
  \item{h_unit}{Legacy alias for \code{metric_h_unit}.}
  \item{metric_h_unit}{Unit of raw height columns used during metric derivation.}
  \item{volume_colmap}{volume_colmap }
  \item{\dots}{Additional arguments passed only to \code{externalMetrics()} when metric derivation is triggered.}
}
\details{Required inputs are inferred from the selected methods in \code{method_registry}. When one or more standardized inputs are missing and \code{compute_metrics_if_needed = TRUE}, the function calls \code{externalMetrics()} to derive them, using \code{design}, \code{metric_colmap}, units, grouping columns, and retained columns from the corresponding arguments.

Each requested output is evaluated with its method definition. A method can resolve its own parameter rows with \code{get_pars}, use embedded parameters in \code{pars}, or fall back to \code{parameter_table}. Raw outputs are converted to cubic metres with \code{scale_to_m3}.

When \code{track_provenance = TRUE}, the result stores per-output provenance columns together with a \code{volume_meta} attribute describing input units, returned units, required inputs, and method settings.}
\value{A data.frame containing the original columns in \code{x} plus the requested volume outputs.

The returned object inherits from classes \code{"externalMetrics2Vol"} and \code{"metrics2vol"}. Named unit metadata are stored in \code{attr(out, "units")}, and the result may also preserve \code{design_meta} and \code{volume_meta}.
Use exact alias matching only here. This avoids false positives such as
matching the requested standardized metric "n" to source columns like
"CAMPAGNE" during the preflight check that decides whether metrics need
to be computed.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
x <- data.frame(
    species = c("sp1", "sp2"),
    d = c(120, 260),
    h = c(7.1, 13.2)
)
attr(x, "units") <- c(d = "mm", h = "m")

pars <- data.frame(
    parameter = "V",
    species = c("sp1", "sp2"),
    a = c(0.00002, 0.00003),
    b = c(2.30, 2.10),
    model = c("demo_sp1", "demo_sp2")
)

ext_methods <- external_volume_method_registry(list(
    V = new_volume_method(
        output = "v",
        fun = function(dbh_mm, h_m, pars) {
            dbh_cm <- dbh_mm / 10
            pars$a + pars$b * (dbh_cm^2) * h_m
        },
        raw_unit = "cm3",
        unit = "m3",
        scale_to_m3 = 1 / 1e6,
        build_args = function(ctx, pars, resolved) {
            list(dbh_mm = ctx$d_mm, h_m = ctx$h_m, pars = pars)
        },
        fallback = function(ctx, pars, resolved) NA_real_,
        match_by = "species",
        required_inputs = c("d", "h")
    )
))

out <- externalMetrics2Vol(
    x,
    parametro = "V",
    parameter_table = pars,
    method_registry = ext_methods,
    track_provenance = TRUE
)

out[, c("species", "v", "v_source", "v_status")]
}

\name{metrics2Vol}
\alias{metrics2Vol}
\title{Compute tree-level volume variables from NFI metrics}
\description{Computes one or more tree-level
volume variables from Spanish National Forest Inventory data. The
function standardizes the input, selects the appropriate volume
model for each tree, applies equation-based methods defined in a
method registry, converts results to cubic metres, and optionally
keeps legacy outputs (from previous versions of the package) and
provenance information.}
\usage{metrics2Vol(nfi, cub.met = "freq", 
    parametro = c("VCC"), 
    keep.var = TRUE, 
    keep.legacy = FALSE, 
    legacy_fallback = TRUE, 
    method_registry = snfi_volume_method_registry(), 
    track_provenance = FALSE, 
    ...)}
\arguments{
  \item{nfi}{Input accepted by \code{nfiMetrics()}, or a precomputed \code{"nfiMetrics"}
object with tree-level metrics.}
  \item{cub.met}{Cubication selector used when several coefficient rows match.}
  \item{parametro}{One or more volume outputs to compute.}
  \item{keep.var}{Keep auxiliary coefficient columns when available.}
  \item{keep.legacy}{Also return the legacy volume estimate for backward compatibility.}
  \item{legacy_fallback}{\code{logical}. When the official equation cannot be used for a tree
(no coefficients for its species, parameter, province or cycle, or the
equation cannot be evaluated), use the legacy equation for the same
province and species if it has coefficients, labelled
\code{"fallback_legacy"} in the provenance columns. Set to
\code{FALSE} to use official equations only and return \code{NA}
instead. Coefficients of another species are never used.}
  \item{method_registry}{Registry that maps each requested output to descriptive metadata,
its equation function, output column name, units, and fallback rule.}
  \item{track_provenance}{Add per-row provenance columns and audit metadata, including the
method label, equation description, reference, and output description.}
  \item{\dots}{Passed to \code{nfiMetrics()} when \code{nfi} is not already an
\code{"nfiMetrics"} object.}
}
\details{Computes requested volume outputs from standardized NFI

tree metrics using registry-based methods and optional

fallback to legacy estimates. With

\code{track_provenance = TRUE}, the \code{volume_meta}

attribute also records each method's label, equation

description, reference, units, scale, and output description.}
\value{A data.frame with the requested volume outputs added. When

\code{track_provenance = TRUE}, attribute \code{volume_meta}

contains computational and descriptive method metadata.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}



\seealso{nfiMetrics, snfi_volume_method_registry,

default_snfi_volume_equations
Identify the core columns needed by the volume equations.
Validate and standardize measurement-unit metadata before
dispatching any equation.
Validate requested outputs against the method registry.
Preallocate all requested output columns.
Compute the legacy volume once so modern methods can fall back to
it when needed.
The legacy helper evaluates the parEqVcc equations with d in mm and
h in m. Make the unit metadata explicit before entering the
compatibility path.
Decide whether legacy estimates can be computed from the input.
Load and normalize the coefficient table used by registry-based
methods.
Convert relevant inputs and coefficient keys to numeric values.
Cache row matches to avoid repeated filtering of the same
coefficient subset.
Resolve the parameter row for one method and one tree.
Resolve the equation function for one registry entry.
Evaluate one registry method and return both value and
provenance.
Evaluate methods row by row. `VCC` is computed first because
later outputs may reuse it.
Drop auxiliary coefficient columns when requested.
Keep only the requested computed outputs.
Reorder identifying columns to the front of the output.
Rebuild units from surviving input columns plus returned volume
outputs. `nfiMetrics` stores units as a named vector:
names = variable names, values = unit strings.
Add units for the computed outputs that are returned.
Computed outputs override any original units with the same name.
Keep units only for columns present in the final output.}
\examples{
x <- structure(
    data.frame(
        nfi.nr = 4,
        pr = 28,
        especie = 21,
        d = c(180, 260),
        h = c(9.5, 14.2),
        n = c(31.83, 14.15)
    ),
    class = c("nfiMetrics", "data.frame"),
    units = c(d = "mm", h = "m", n = "ha-1"),
    nfi.nr = 4
)

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

y <- metrics2Vol(
    x,
    parametro = "VCC",
    method_registry = demo_registry,
    keep.var = FALSE
)

y[, c("nfi.nr", "pr", "especie", "vcc_demo")]

## Real SNFI workflows may require local Access/mdbtools support or
## previously cached data, so they are intentionally not run here.
## z <- nfiMetrics(28, nfi.nr = 4, dir = tools::R_user_dir("basifoR", "cache"))
## metrics2Vol(z, parametro = c("VCC", "VSC"))
}

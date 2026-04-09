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
    method_registry = snfi_volume_method_registry(), 
    track_provenance = FALSE, 
    ...)}
\arguments{
  \item{nfi}{Input accepted by `nfiMetrics()`, or a precomputed `"nfiMetrics"`
object with tree-level metrics.}
  \item{cub.met}{Cubication selector used when several coefficient rows match.}
  \item{parametro}{One or more volume outputs to compute.}
  \item{keep.var}{Keep auxiliary coefficient columns when available.}
  \item{keep.legacy}{Also return the legacy volume estimate for backward compatibility.}
  \item{method_registry}{Registry that maps each requested output to its equation function,
output column name, units, and fallback rule.}
  \item{track_provenance}{Add per-row provenance columns and audit metadata.}
  \item{\dots}{Passed to `nfiMetrics()` when `nfi` is not already an
`"nfiMetrics"` object.}
}
\details{Computes requested volume outputs from standardized NFI

tree metrics using registry-based methods and optional

fallback to legacy estimates.}
\value{A data.frame with the requested volume outputs added.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}
\note{Registry-based dispatcher for volume equations.
Return early for `NULL` input.
Build dendrometric metrics when the input is not already
standardized.
Resolve the inventory edition from the object attribute first and
from a column only as a fallback.
Match the first available column name among accepted aliases.
Identify the core columns needed by the volume equations.
Validate and standardize measurement-unit metadata before
dispatching any equation.
Validate requested outputs against the method registry.
Preallocate all requested output columns.
Compute the legacy volume once so modern methods can fall back to
it when needed.
Decide whether legacy estimates can be computed from the input.
need_legacy <- keep.legacy || any(parametro \%in\% c("V", "VCC"))
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


\seealso{nfiMetrics, snfi_volume_method_registry}
\examples{if (FALSE) {
  x <- nfiMetrics('toledo')
  y <- metrics2Vol(x, parametro = c('VCC', 'VSC'))
  head(y[c('nfi.nr', 'pr', 'especie', 'vcc', 'vsc')])
}}

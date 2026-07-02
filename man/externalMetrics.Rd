\name{externalMetrics}
\alias{externalMetrics}
\title{Compute tree-level metrics from external inventory data}
\description{Standardize external tree measurements for external inventory workflows and return requested tree-level metrics in basifoR units.}
\usage{externalMetrics(x, var = c("d", 
    "h", "ba", "n", "Hd", 
    "Dd"), levels = NULL, 
    design, colmap = NULL, 
    d_unit = c("mm", 
        "cm")[1], h_unit = c("m", 
        "dm", "cm")[1], 
    keep_cols = NULL, 
    domheight_fun = NULL, 
    domdiameter_fun = NULL, 
    domheight_method = "Hd_strict", 
    domheight_registry = dominant_height_method_registry())}
\arguments{
  \item{x}{Input data.frame with one row per tree or stem.}
  \item{var}{Requested metrics to return. Supported values are \code{"d"}, \code{"h"}, \code{"ba"}, \code{"n"}, \code{"Hd"}, and \code{"Dd"}. Requesting \code{"Hd"} requires \code{"d"}, \code{"h"}, and \code{"n"}; requesting \code{"Dd"} requires \code{"d"} and \code{"n"}.}
  \item{levels}{Columns defining the groups within which dominant
metrics are computed.}
  \item{design}{Inventory \code{design} used to compute expansion factors for \code{n}.}
  \item{colmap}{Named list of candidate raw column names for diameter and height.
Matching is case-insensitive and also accepts numeric suffixes such
as \code{diameter_1} or \code{height.2}.}
  \item{d_unit}{Unit of raw diameter columns in \code{colmap$d}.}
  \item{h_unit}{Unit of raw height columns in \code{colmap$h}.}
  \item{keep_cols}{Additional source columns to carry into the result
without changing the groups defined by \code{levels}.}
  \item{domheight_fun}{Optional function used when \code{"Hd"} is requested. If supplied, it overrides \code{domheight_method}.}
  \item{domdiameter_fun}{Optional function used when \code{"Dd"} is
requested together with a custom
\code{domheight_fun}. It must accept \code{d}
and \code{n}.}
  \item{domheight_method}{Dominant-height method code resolved through \code{domheight_registry} when \code{domheight_fun = NULL}.}
  \item{domheight_registry}{Named registry created with \code{\link{dominant_height_method_registry}}.}
}
\details{The function first resolves measurement columns from \code{colmap}. Exact matches are preferred, then case-insensitive matches with optional numeric suffixes are considered. When several repeated measurement columns are found for the same variable, row-wise non-missing means are used.

Zero values in resolved diameter or height columns are treated as missing before aggregation. Returned units are standardized to millimetres for \code{d} and \code{Dd}, metres for \code{h} and \code{Hd}, square metres per tree for \code{ba}, and trees per hectare for \code{n}.

When \code{var} includes \code{"n"}, the function uses \code{design} to obtain expansion factors. Fixed-area designs call \code{trees_per_ha()}, while concentric designs choose the proper factor from the \code{design} thresholds. Dominant height and dominant diameter are computed within each resolved group defined only by \code{levels}; \code{keep_cols} preserves source columns without changing those groups. Both metrics use the diameter ordering, threshold, and fallback convention selected through \code{domheight_registry}; \code{Dd} uses valid diameter and expansion-factor values and therefore may be available when \code{Hd} is missing because heights are unavailable. When both metrics are requested with custom functions, supply both \code{domheight_fun} and \code{domdiameter_fun} so the pair remains explicit.}
\value{A data.frame containing the requested tree-level metrics, optionally preceded by resolved grouping columns.

The returned object inherits from classes \code{"externalMetrics"} and \code{"nfiMetrics"}. Unit metadata are stored in \code{attr(out, "units")}. When \code{var} includes \code{"n"}, \code{"Hd"}, or \code{"Dd"}, the result also stores sampling \code{design} metadata in \code{attr(out, "design_meta")}.}

\author{Wilson Lara [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Cristobal Ordonez [aut] (ORCID:
<https://orcid.org/0000-0001-5354-3760>),
Aitor Vázquez-Veloso [aut] (ORCID:
<https://orcid.org/0000-0003-0227-506X>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




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

externalMetrics(
    x = x,
    var = c("d", "h", "ba", "n"),
    levels = c("plot", "species"),
    design = sq_0.1ha,
    colmap = list(d = "diameter_mm", h = "height_m"),
    d_unit = "mm",
    h_unit = "m"
)
}
